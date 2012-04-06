(ns sweety.core
  (:refer-clojure :exclude [list assoc!])
  (:use [clojure.string :only [capitalize split join lower-case]]
        [sweety.constants :only [de-camel events constants]])
  (:import (org.eclipse.swt SWT)
           (org.eclipse.swt.graphics Color)
           (org.eclipse.swt.layout RowLayout RowData
                                   GridLayout GridData
                                   FillLayout FillData
                                   FormLayout FormData FormAttachment)
           (org.eclipse.swt.widgets Listener)))

;;; defwidget ------------------------------------------------------------------

(defprotocol Widget
  (-assoc [this key val]))

(letfn [(methods-starting-with [start c]
          (->> (.getMethods c)
               (map #(.getName %))
               (filter #(.startsWith % start))
               distinct
               (map symbol)))]

  (def ^:private getters (partial methods-starting-with "get"))
  (def ^:private setters (partial methods-starting-with "set")))

(defn method->keyword [sym]
  (-> sym str (subs 3) de-camel keyword))

(defn keyword+method-pairs [make-form syms]
  (mapcat (juxt method->keyword make-form) syms))

(defmacro deftype-for-widget [name class [& fields] opts]
  (let [class (resolve class)
        setter-forms (keyword+method-pairs (fn [sym] `(. ~'widget ~sym ~'val))
                                           (setters class))
        getter-forms (keyword+method-pairs (fn [sym] `(. ~'widget ~sym))
                                           (getters class))]
    `(deftype ~name [~'widget ~@fields]
       Widget
       (-assoc [this# key# ~'val]
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

(defn args-for-defwidget [[maybe-init :as more]]
  (let [[init more] (if (set? maybe-init)
                      [maybe-init (rest more)]
                      [#{} more])
        [methods more] (split-with (every-pred list? method-call?)
                                   more)
        [keys+vals children] (map (partial apply concat)
                                  (split-with (comp keyword? first)
                                              (partition-all 2 more)))]
    [init methods keys+vals children]))

(defn reduce-init [coll]
  (if-not (seq coll)
    SWT/NULL
    (->> coll
         (map #(if (keyword? %) (get constants %) %))
         (reduce bit-or))))

(defmacro defwidget*
  [name type-name class [& fields] opts]
  `(do
     (deftype-for-widget ~type-name ~class [~@fields] ~opts)
     (defmacro ~name ~(:doc opts)
       {:arglists '([init? methods* keys+vals*] [name? init? methods* keys+vals* children*])}
       [& args#]
       (let [[init# methods# keys+vals# children#] (args-for-defwidget args#)]
         ;; `(assoc! ~@keys+vals#)
         `(new ~~type-name
               (doto (new ~~class *parent* (reduce-init ~init#))
                 ~@methods#))))))

(defmacro defwidget
  ([class]
     `(defwidget ~class "" nil))
  ([class doc]
     `(defwidget ~class ~doc nil))
  ([class doc deref]
     (let [type-name (-> class resolve .getSimpleName)
           name (de-camel type-name)]
       `(defwidget* ~(symbol name) ~(symbol type-name) ~class []
          {:doc ~doc :deref ~deref}))))


;;; Widgets declaration --------------------------------------------------------

(defwidget org.eclipse.swt.widgets.Composite)
(defwidget org.eclipse.swt.widgets.Label)
(defwidget org.eclipse.swt.widgets.Button)
(defwidget org.eclipse.swt.widgets.List)
(defwidget org.eclipse.swt.widgets.Canvas)
(defwidget org.eclipse.swt.widgets.FileDialog)
(defwidget org.eclipse.swt.widgets.Group)
(defwidget org.eclipse.swt.widgets.Scale)
(defwidget org.eclipse.swt.widgets.Text)
(defwidget org.eclipse.swt.widgets.MessageBox)


;;; Utilities ------------------------------------------------------------------

(defn assoc!
  "TODO: doc"
  ([widget key val]
     (-assoc widget key val))
  ([widget key val & keys+vals]
     (doseq [[k v] (concat [key val] keys+vals)]
       (-assoc widget k v))))

(defn update!
  "TODO: doc"
  [widget key f & args]
  (assoc! widget key (apply f (get widget key) args)))


(def ^{:dynamic true :doc "TODO: doc"} *display*)

(defmacro with-display
  "Evaluates body in the try block, with the *display* bound to the given
   display, then calls .dispose on it."
  [display & body]
  `(binding [*display* ~display]
     (try ~@body (finally (.dispose *display*)))))

(defmacro with-new-display
  "Same as (with-display (Display.) ...)"
  [& body]
  `(with-display (Display.) ~@body))


(def ^{:dynamic true :doc "TODO: doc"} *parent*)

(defmacro with-parent
  "Evaluates body with the *parent* bound to the given widget."
  [widget & body]
  `(binding [*parent* ~widget] ~@body))


(defn add-listener
  "Adds a Listener on the event to the given widget. Returns widget.
  f must be a function of 1 arg (the Event object)"
  ([widget event f]
     (let [event (if (keyword? event)
                   (get events event)
                   event)]
       (doto widget
         (.addListener widget event (reify Listener
                                      (handleEvent [this e]
                                        (f e)))))))
  ([widget event f & events+functions]
     (doseq [[e f] (concat [event f] events+functions)]
       (add-listener widget e f))))

(defmacro deflistener
  "TODO: doc"
  [widget event args & body]
  (assert (= (count args) 1) "Arguments vector must contain exactly 1 item")
  `(add-listener ~widget ~event (fn [~@args] ~@body)))


(defmacro async-exec
  "Asyncronously executes body (which presumably updates UI).
   Returns the value returned by the body. Use it when you want
   to have access to the SWT widgets from non-UI thread."
  [& body]
  `(let [res# (atom nil)]
     (.asyncExec (Display/getDefault)
                 (reify Runnable
                   (run [this#] (reset! res# (do ~@body)))))
     @res#))

(defmacro sync-exec
  "Like async-exec, but blocks the thread until execution of the function
   has been finished."
  [& body]
  `(let [res# (atom nil)]
     (.syncExec (Display/getDefault)
                (reify Runnable
                  (run [this#] (reset! res# (do ~@body)))))
     @res#))


;;================================
;; Shell
;;================================

(defmacro shell
  "Creates shell on the current *display* and returns it."
  [params & more]
  `(init-bean (Shell. *display* (reduce bit-or ~params)) ~@more))

(defn open-shell!
  "FIXME Calls .open on the given shell and runs ?service? loop while shell is not disposed."
  [shell]
  (try
    (.open shell)
    (while (not (.isDisposed shell))
      (when (.readAndDispatch *display*)
        (.sleep *display*)))
    (finally
     (when-not (.isDisposed shell)
       (.dispose shell)))))


;;; Layouts --------------------------------------------------------------------
;; TODO: refactor

(defn- camel-case [s]
  (join (map capitalize (split s #"-"))))

(defn keyword->field [kw]
  (let [[f & rest] (-> kw name camel-case seq)]
    (symbol (apply str (Character/toLowerCase f) rest))))

(defmacro layout [type & args]
  (let [class (case type
                :row RowLayout
                :grid GridLayout
                :fill FillLayout
                :form FormLayout
                (throw (IllegalArgumentException. "Unsupported layout type.")))
        layout (gensym "layout")]
    (if (seq args)
      `(let [~layout (new ~class)]
         ~@(for [[k v] (partition 2 args)]
             `(set! (. ~layout ~(keyword->field k)) ~v))
         ~layout)
      `(new ~class))))

(defmacro form-layout [& args] `(layout :form ~@args))
(defmacro row-layout  [& args] `(layout :row ~@args))
(defmacro grid-layout [& args] `(layout :grid ~@args))
(defmacro fill-layout [& args] `(layout :fill ~@args))


;;--------------------------------------------------------------------------------
;; WARNING: Heavy copypasting here; to be refactored
(defmacro layout-data [type & args]
  (let [class (case type
                :row RowData, :grid GridData
                :fill FillData, :form FormData
                (throw (IllegalArgumentException. "Unsupported layout data type.")))
        layout (gensym "layout")]
    (if (seq args)
      `(let [~layout (new ~class)]
         ~@(for [[k v] (partition 2 args)]
             `(set! (. ~layout ~(keyword->field k)) ~v))
         ~layout)
      `(new ~class))))

(defmacro form-data [& args] `(layout-data :form ~@args))
(defmacro row-data  [& args] `(layout-data :row ~@args))
(defmacro grid-data [& args] `(layout-data :grid ~@args))
(defmacro fill-data [& args] `(layout-data :fill ~@args))
;;--------------------------------------------------------------------------------

(defn form-attachment [percents-or-widget shift]
  (FormAttachment. percents-or-widget shift))


;;; Misc -----------------------------------------------------------------------

(defn color [r g b]
  (Color. *display* r g b))

(defn widget?  [x] (instance? org.eclipse.swt.widgets.Widget x))
(defn control? [x] (instance? org.eclipse.swt.widgets.Control x))
(defn dialog?  [x] (instance? org.eclipse.swt.widgets.Dialog x))
