(ns sweety.core
  (:refer-clojure :exclude [list assoc!])
  (:use [clojure.string :only [capitalize split join lower-case]]
        [clojure.walk :only [macroexpand-all]]
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

(defn args-for-defwidget [[maybe-name :as more]]
  (let [[name [maybe-init :as more]] (if (symbol? maybe-name)
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

(defmacro defwidget*
  [macro-name type-name class [& fields] opts]
  `(do
     (deftype-for-widget ~type-name ~class [~@fields] ~opts)
     (defmacro ~macro-name ~(:doc opts)
       {:arglists '([init? methods* keys+vals*] [name? init? methods* keys+vals* children*])
        ::widget true}
       [& args#]
       (let [[name# init# methods# keys+vals# children#] (args-for-defwidget args#)
             create-widget# (gen-create-widget ~type-name ~class init# methods# keys+vals#)]
         (set! *widget-names* (conj *widget-names* name#)) ; will be defined later, see defgui
         (if name#
           `(do (def ~name# ~create-widget#)
                ~(gen-create-children name# children#))
           (gen-create-children create-widget# children#))))))

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


;;; defgui ---------------------------------------------------------------------

(def ^:private ^:dynamic *widget-names*)

(defprotocol Gui
  (init! [this] "Actually creates a widgets defined in this gui.")
  (root [this] "Returns the root widget of this gui."))

(defn force-listeners [w]
  (->> w meta ::listeners (map force)))

(defmacro defgui
  "Defines a Gui with the given name which contains the root widget
   and its children. Also, declares names for every named widget. If
   parent argument is not given, it defaults to *display*. Note that
   defgui call does not actually create widgets, see also 'create!'."
  ([name root]
     `(defgui ~name *display* ~root))
  ([name parent root]
     (binding [*widget-names* []]
       (let [expanded-root (macroexpand-all root)]
         `(do (declare ~@*widget-names*)
              (let [widgets# (delay (with-parent ~parent ~expanded-root))]
                (def ~name
                  (reify Gui
                    (init! [this#] 
                      (let [listeners# (->> [~@(map (fn [name] `(var ~name)) *widget-names*)]
                                            (mapcat (comp ::listeners meta)))]
                        (prn listeners#)
                        (force widgets#)
                        (dorun (map force listeners#))))
                    (root [this#]
                      ~(first *widget-names*))))))))))


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
(defwidget org.eclipse.swt.widgets.Shell)


;;; Utilities ------------------------------------------------------------------

(defn assoc!
  "Sets the property 'key' of the given widget to val. Keys are
   obtained from setters by replacing CamelCase with hyphens and
   keywordizing the result. E.g. .setText becomes :set-text.
   See also: update!"
  ([widget key val]
     (-assoc widget key val)
     widget)
  ([widget key val & keys+vals]
     (doseq [[k v] (concat [key val] keys+vals)]
       (-assoc widget k v))
     widget))

(defn update!
  "Applies the function f to the old value of property with given key
  and any number of given args, then sets the value of this property
  to what the f has returned.
  See also: assoc!"
  [widget key f & args]
  (assoc! widget key (apply f (get widget key) args)))


(defn dispose-if-not [this]
  (when-not (.isDisposed this)
    (.dispose this)))

(def ^{:dynamic true :doc "TODO: doc"} *display*)

(defmacro with-display
  "Evaluates body in the try block, with the *display* bound to the given
   display, then calls .dispose on it."
  [display & body]
  `(binding [*display* ~display]
     (try ~@body (finally (dispose-if-not *display*)))))

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
       (doto (.widget widget)
         (.addListener event (reify Listener
                               (handleEvent [this e]
                                 (f e)))))))
  ([widget event f & events+functions]
     (doseq [[e f] (concat [event f] events+functions)]
       (add-listener widget e f))))

(defmacro deflistener
  "Adds a listener to the given widget. Event must be one of SWT event
  constants, or corresponding keyword (e.g. SWT/MouseUp
  <=> :mouse-up). Args are binding vector, which must contain 1 item -
  the event object.
  See also: sync-exec, async-exec."
  [widget event args & body]
  (assert (= (count args) 1) "Arguments vector must contain exactly 1 item")
  `(alter-meta! (var ~widget) update-in [::listeners] conj
                (delay (add-listener ~widget ~event
                         (fn [~@args] ~@body)))))


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


(defn open!
  "Shows the given shell/dialog on the screen."
  [this]
  (-> this .widget .open))

(defn create!
  "Given a gui, initializes it, opens the root widget (assuming it's a
  shell and has name defined) and starts a service loop. You should
  call it only once to start the main shell of your application.
  Example: (with-new-display (init! my-gui) (create! my-gui))"
  [gui]
  (let [shell (-> gui root .widget)]
    (try
      (.open shell)
      (while (not (.isDisposed shell))
        (when-not (.readAndDispatch *display*)
          (.sleep *display*)))
      (finally
       (dispose-if-not shell)))))



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
