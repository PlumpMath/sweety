(ns sweety.defwidget
  (:use [clojure.string   :only [split]]
        [sweety.utils     :only [de-camel method->keyword getters setters]]
        [sweety.constants :only [events constants]])
  (:import (org.eclipse.swt SWT)))

(defprotocol Widget
  (set-swt-object! [this val])
  (get-swt-object [this] "Returns the underlying SWT widget.")
  (get-id [this] "Returns the unique id of this widget.")
  (get-children [this] "Returns a coll of the children of this widget.")
  (add-init-fn! [this f] "Adds f to the coll of init functions. f
  must be a function (presumably with side effects) which takes one
  argument, the widget.")
  (init! [this] "Calls all init functions for this widget."))


(def ^{:dynamic true :doc "TODO: doc"} *parent*)

(defmacro with-parent
  "Evaluates body with the *parent* bound to the given widget."
  [widget & body]
  `(binding [*parent* ~widget] ~@body))


(defmulti get-property-hook
  (fn [class method-name] [class method-name]))

(defmethod get-property-hook :default [_ _]
  nil)

(defmacro defhooks
  "Defines property hooks for the given class or interface. Each
  declaration has the form: (name set-hook get-hook), where name is
  the property name (i.e. setter or getter name without 'set' or
  'get') and set-hook and get-hook are single-argument functions which
  will be applied to the value passed to/returned from appropriate
  setter/getter."
  [class & declarations]
  (->> (for [[prop set-hook get-hook] declarations
             :let [setter (->> prop (str "set") symbol)
                   getter (->> prop (str "get") symbol)]]
         `[(defmethod get-property-hook [~class '~setter] [_# _#] ~set-hook)
           (defmethod get-property-hook [~class '~getter] [_# _#] ~get-hook)])
       (apply concat)
       (cons `do)))


(defn emit-widget-impl []
  `[(set-swt-object! [this# val#] (reset! ~'swt-object val#))
    (get-swt-object [this#] (deref ~'swt-object))
    (get-id [this#] ~'id)
    (get-children [this#] ~'children)
    (add-init-fn! [this# f#] (swap! ~'init-fns conj f#))
    (init! [this#] (doseq [f# (deref ~'init-fns)]
                      (f# this#)))])

(defn keyword+method-pairs [gen-method syms]
  (mapcat (juxt method->keyword gen-method) syms))

(defn emit-itransient-impl [class]
  (let [[this val] (map gensym ["this" "val"])
        kws+setters (keyword+method-pairs
                     (fn [sym]
                       (if-let [hook (get-property-hook class sym)]
                         `(. (get-swt-object ~this) ~sym (~hook ~val))
                         `(. (get-swt-object ~this) ~sym ~val)))
                     (setters class))]
    `[(assoc [~this key# ~val]
        (case key#
          ~@kws+setters)
        ~this)]))

(defn emit-ilookup-impl [class]
  (let [this (gensym "this")
        kws+getters (keyword+method-pairs
                     (fn [sym]
                       (if-let [hook (get-property-hook class sym)]
                         `(~hook (. (get-swt-object ~this) ~sym))
                         `(. (get-swt-object ~this) ~sym)))
                     (getters class))]
    `[(valAt [~this key#]
             (case key#
               ~@kws+getters))
      (valAt [this# key# not-found#]
             (if-let [ret# (get this# key#)] ret# not-found#))]))

(defn emit-ireference-impl []
  `[(resetMeta [this# m#])
    (alterMeta [this# f# args#])])

(defn emit-iref-impl []
  `[(setValidator [this# f#])
    (getValidator [this#])
    (addWatch [this# key# f#])
    (removeWatch [this# key#])
    (getWatches [this#])])


(defmacro deftype-for-widget [name class]
  (let [class (resolve class)
        fields '[swt-object init-fns id children metadata validator watchers]]
    `(deftype ~name ~fields
       Widget
       ~@(emit-widget-impl)

       clojure.lang.ITransientAssociative ; mutable map support (assoc!)
       ~@(emit-itransient-impl class)

       clojure.lang.ILookup ; keyword access (get)
       ~@(emit-ilookup-impl class)

       clojure.lang.IMeta ; support for returning metadata (meta)
       (meta [this#] (deref ~'metadata))

       clojure.lang.IReference ; mutable metadata (reset-meta! and alter-meta!)
       ~@(emit-ireference-impl)

       clojure.lang.IRef ; watchers and validators
       ~@(emit-iref-impl)

       clojure.lang.IDeref ; @widget syntax
       (deref [this#]))))

(defmacro make-widget
  [class {:keys [swt-object init-fns id children meta validator watchers]
          :or {swt-object `(atom nil)
               init-fns `(atom ())
               meta `(atom {})
               watchers `(atom {})}}]
  (list 'new class swt-object init-fns id children meta validator watchers))

(defn initialized? [widget]
  (not= nil (get-swt-object widget)))


;; TODO: it's ugly, refactor somehow
(defn method-call? [form]
  (if (list? form)
    (let [[sym] form]
      (and (symbol? sym) (= \. (first (str sym)))))
    false))

;; FIXME: rewrite
;; TODO: return opts too
(defn args-for-defwidget [[maybe-name :as more]]
  (let [[name [maybe-init :as more]] (if (keyword? maybe-name)
                                       [maybe-name (rest more)]
                                       [nil more])
        [init more] (if (set? maybe-init)
                      [maybe-init (rest more)]
                      [#{} more])
        [methods more] (split-with (every-pred list? method-call?)
                                   more)
        [keys+vals children] (map (partial apply concat)
                                  (split-with (comp keyword? first)
                                              (partition-all 2 more)))]
    [name init methods keys+vals children]))

(defn parse-name-keyword [kw]
  (let [[id & classes] (split (name kw) #"#")]
    [(when (seq id) (keyword (namespace kw) id))
     (map keyword classes)]))

(defn compute-style
  "Given a collection of SWT constants (or corresponding keywords),
  applies bit-or to them and returns the resulting number."
  [coll]
  {:pre [(every? (some-fn constants number?) coll)]}
  (if-not (seq coll)
    SWT/NULL
    (->> coll
         (map #(if (keyword? %) (get constants %) %))
         (reduce bit-or))))

(defn gen-init-fn [class style methods keys+vals]
  {:pre [(even? (count keys+vals))]}
  (let [widget (gensym "widget")
        assoc-keys+vals (when (seq keys+vals)
                          `(assoc! ~widget ~@keys+vals))]
    `(fn [~widget]
       (set-swt-object! ~widget
                        (doto (new ~class *parent* (compute-style ~style))
                          ~@methods))
       ~assoc-keys+vals)))

(defmacro defwidget*
  [macro-name type-name class attr-map]
  `(let [type# (deftype-for-widget ~type-name ~class)]
     (defmacro ~macro-name ~attr-map
       [& args#]
       (let [[name# style# methods# keys+vals# children#] (args-for-defwidget args#)
             [id# classes#] (parse-name-keyword name#) ;; TODO: classes
             opts# {} ; XXX: will be returned from args-for-defwidget
             opts# (merge opts# {:id id# :children (vec children#)})
             init# (gen-init-fn ~class style# methods# keys+vals#)]
         `(doto (make-widget ~type# ~opts#)
            (add-init-fn! ~init#))))))

(defmacro defwidget
  ([class]
     `(defwidget ~class "" {}))
  ([class doc]
     `(defwidget ~class ~doc {}))
  ([class doc attr-map]
     (let [type-name (-> class resolve .getSimpleName symbol)
           name (-> type-name str de-camel symbol)
           arglists '(quote ([name? style? methods* keys+vals* children*]))
           attr-map (merge {:arglists arglists :doc doc}
                           attr-map)]
       `(defwidget* ~name ~type-name ~class ~attr-map))))
