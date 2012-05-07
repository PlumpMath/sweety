(ns sweety.defwidget
  (:use [clojure.string   :only [split]]
        [sweety.utils     :only [de-camel method->keyword getters setters]]
        [sweety.constants :only [events constants]])
  (:import (org.eclipse.swt SWT)))

;;; defwidget ------------------------------------------------------------------

(defprotocol Widget
  (-swt-object [this] "Returns underlying SWT widget.")
  (-id [this] "Returns unique id of this widget.")
  (-children [this] "Returns a coll of the children of this widget.")
  (-add-init-fn! [this f] "Adds f to the coll of init functions. f must
  be a function which takes and returns the widget.")
  (-init! [this] "Creates the widget and calls all init functions on it."))

(defn emit-widget-impl []
  `[(-swt-object [this#] (deref ~'swt-object))
    (-id [this#] ~'id)
    (-children [this#] ~'children)
    (-add-init-fn! [this# f#] (swap! ~'init-fns conj f#))
    (-init! [this#] (swap! ~'swt-object (comp ~'init-fns)))])

(defmulti get-property-hook
  (fn [class method-name] [class method-name]))

(defmethod get-property-hook :default [_ _]
  nil)

;; FIXME: getIME becomes just :
(defn keyword+method-pairs [gen-method syms]
  (mapcat (juxt method->keyword gen-method) syms))

(defn emit-itransient-impl [class]
  (let [val (gensym "val")
        kws+setters (keyword+method-pairs
                     (fn [sym]
                       (if-let [hook (get-property-hook class sym)]
                         `(. ~'swt-object ~sym (~hook ~val))
                         `(. ~'swt-object ~sym ~val)))
                     (setters class))]
    `[(assoc [this# key# ~val]
        (case key#
          ~@kws+setters))]))

(defn emit-ilookup-impl [class]
  (let [kws+getters (keyword+method-pairs
                     (fn [sym] `(. ~'swt-object ~sym))
                     (getters class))]
    `[(valAt [this# key#]
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
          :or {init-fns (atom ()), metadata {}, watchers {}}}]
  (list 'new class swt-object init-fns id children meta validator watchers))

(defn initialized? [widget]
  (not= nil (-swt-object widget)))


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

(defn reduce-init [coll]
  {:pre [(every? (some-fn constants number?) coll)]}
  (if-not (seq coll)
    SWT/NULL
    (->> coll
         (map #(if (keyword? %) (get constants %) %))
         (reduce bit-or))))

(defn gen-create-widget [class init methods keys+vals]
  (let [swt-widget `(doto (new ~class *parent* (reduce-init ~init))
                      ~@methods)]
    (if (seq keys+vals)
      `(assoc! ~swt-widget ~@keys+vals)
      swt-widget)))

(defn parse-name-keyword [kw]
  (let [[id & classes] (split (name kw) #"#")]
    [(when (seq id) (keyword (namespace kw) id))
     (map keyword classes)]))

(defmacro defwidget*
  [macro-name type-name class attr-map]
  `(let [type# (deftype-for-widget ~type-name ~class)]
     (defmacro ~macro-name ~attr-map
       [& args#]
       (let [[name# init# methods# keys+vals# children#] (args-for-defwidget args#)
             create-widget# (gen-create-widget ~class init# methods# keys+vals#)
             [id# classes#] (parse-name-keyword name#) ;; TODO: classes
             opts# {} ; XXX: will be returned from args-for-defwidget
             opts# (merge opts# {:id id# :children (vec children#)})]
         `(doto (make-widget ~type# ~opts#)
            (-add-init-fn! (fn [~(gensym)] ~create-widget#)))))))

(defmacro defwidget
  ([class]
     `(defwidget ~class "" {}))
  ([class doc]
     `(defwidget ~class ~doc {}))
  ([class doc attr-map]
     (let [type-name (-> class resolve .getSimpleName symbol)
           name (-> type-name str de-camel symbol)
           arglists '(quote ([name? init? methods* keys+vals* children*]))
           attr-map (merge {:arglists arglists :doc doc}
                           attr-map)]
       `(defwidget* ~name ~type-name ~class ~attr-map))))
