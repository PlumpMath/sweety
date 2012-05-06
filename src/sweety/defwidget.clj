(ns sweety.defwidget
  (:use [clojure.string   :only [split]]
        [sweety.utils     :only [de-camel method->keyword getters setters]]
        [sweety.constants :only [events constants]])
  (:import (org.eclipse.swt SWT) 
           (org.eclipse.swt.widgets Listener)))

;;; defwidget ------------------------------------------------------------------

(defprotocol Widget
  (-assoc! [this key val]))


(defn keyword+method-pairs [make-form syms]
  (mapcat (juxt method->keyword make-form) syms))

(declare common-hooks)

;; TODO: ITransientAssociative (assoc!)
;; TODO: clojure.lang.IReference (mutable metadata support)
;; TODO: clojure.lang.IRef (watchers and validators)
(defmacro deftype-for-widget [name class opts]
  (let [class (resolve class)
        setter-forms (keyword+method-pairs
                      (fn [sym]
                        (if-let [hook (->> opts :hooks (merge common-hooks) sym)]
                          `(. ~'widget ~sym (~hook ~'val))
                          `(. ~'widget ~sym ~'val)))
                      (setters class))
        getter-forms (keyword+method-pairs
                      (fn [sym] `(. ~'widget ~sym))
                      (getters class))]
    `(deftype ~name [~'widget]
       Widget
       (-assoc! [this# key# ~'val]
         (case key#
           ~@setter-forms))

       clojure.lang.ILookup
       (valAt [this# key#]
         (case key#
           ~@getter-forms))
       (valAt [this# key# not-found#]
         (if-let [ret# (get this# key#)] ret# not-found#))

       clojure.lang.IDeref
       (deref [this#] ~(:deref opts)))))


;; TODO: it's ugly, refactor somehow
(defn method-call? [form]
  (if (list? form)
    (let [[sym] form]
      (and (symbol? sym) (= \. (first (str sym)))))
    false))

;; FIXME: rewrite
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
  (if-not (seq coll)
    SWT/NULL
    (->> coll
         (map #(if (keyword? %) (get constants %) %))
         (reduce bit-or))))

(defn gen-create-widget [type-name class init methods keys+vals]
  (let [swt-widget `(new ~type-name
                         (doto (new ~class *parent* (reduce-init ~init))
                           ~@methods))]
    (if (seq keys+vals)
      `(assoc! ~swt-widget ~@keys+vals)
      swt-widget)))

(defn gen-create-children [parent children]
  (if (seq children)
    `(with-parent (.widget ~parent) ~@children)
    parent))

(defn parse-name-keyword [kw]
  (let [[id & classes] (split (name kw) #"#")]
    {:id (when (seq id) (keyword (namespace kw) id))
     :classes (map keyword classes)}))

(defprotocol WidgetInstance
  (-info [this] "Returns a map with keys :id and :classes.")
  (-init! [this] "Creates the SWT widget as a child of *parent* and
  initializes it."))

(defmacro defwidget*
  [macro-name type-name class opts]
  `(do
     (deftype-for-widget ~type-name ~class ~opts)
     (defmacro ~macro-name ~(:doc opts)
       {:arglists '([name? init? methods* keys+vals* children*])}
       [& args#]
       (let [[name# init# methods# keys+vals# children#] (args-for-defwidget args#)
             create-widget# (gen-create-widget ~type-name ~class init# methods# keys+vals#)]
         `[(reify WidgetInstance
             (-info [this#] ~(parse-name-keyword name#))
             (-init! [this#] ~create-widget#))
           ~@children#]))))

(defmacro defwidget
  ([class]
     `(defwidget ~class "" nil))
  ([class doc]
     `(defwidget ~class ~doc nil))
  ([class doc opts]
     (let [type-name (-> class resolve .getSimpleName)
           name (de-camel type-name)]
       `(defwidget* ~(symbol name) ~(symbol type-name) ~class
          ~(merge {:doc doc} opts)))))
