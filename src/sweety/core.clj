(ns sweety.core
  (:use [sweety.defwidget :only [with-parent init! add-init-fn!
                                 get-swt-object get-id get-children]]
        [sweety.constants :only [events]])
  (:import (org.eclipse.swt.graphics Color)
           (org.eclipse.swt.widgets Listener Display)))

(defn update!
  "Sets the value of property named by `key` to:
  (apply f current-value-of-property args). Returns widget.
  Note: use clojure.core/assoc! to set the value of property
  directly."
  [widget key f & args]
  (assoc! widget key (apply f (get widget key) args)))

(defn open!
  "Shows the given shell/dialog on the screen."
  [this]
  (-> this get-swt-object .open))


(defn listener
  "Turns the given single-argument function into a Listener object."
  [f]
  (reify Listener
    (handleEvent [this e]
      (f e))))

(defn add-listener!
  "Adds a listener to the given widget. `event` must be an SWT event
  constant or corresponding keyword (e.g. SWT/MouseUp <=> :mouse-up).
  `f` must be a function of one argument, the Event object.
  See also: deflistener."
  ([widget event f] {:pre [(or (events event) (some #{event} (vals events)))]}
     (let [event (if (keyword? event)
                   (get events event)
                   event)]
       (doto (get-swt-object widget)
         (.addListener event (listener f)))))
  ([widget event f & events+functions]
     (doseq [[e f] (concat [event f] events+functions)]
       (add-listener! widget e f))))

(declare add-pre-run-hook!)

(defmacro deflistener
  "Defines a listener for the widget named by `id`.
  `event` must be an SWT event constant or corresponding
  keyword (e.g. SWT/MouseUp <=> :mouse-up). `args` must be a vector
  containing one symbol to which the Event object will be bound in
  `body`. The listener will be added right after initialization of the
  widget (refer to the `defgui` doc for details about widget
  initialization process).
  See also: add-listener!, sync-exec, async-exec."
  [id event args & body]
  {:pre [(= (count args) 1)]}
  `(add-pre-run-hook!
    (fn [] (add-listener! (by-id ~id) ~event (fn [~@args] ~@body)))))


(defmacro async-exec
  "Asyncronously executes body (which presumably updates UI).
  Returns nil. Use it when you want to have access to SWT widgets from
  non-UI thread. Note that you should usually prefer this over the
  `sync-exec`."
  [& body]
  `(.asyncExec (Display/getDefault)
               (reify Runnable
                 (run [this#] (do ~@body)))))

(defmacro sync-exec
  "Like `async-exec`, but blocks current thread until execution of the
  body has been finished. Returns the value returned by body."
  [& body]
  `(let [res# (atom nil)]
     (.syncExec (Display/getDefault)
                (reify Runnable
                  (run [this#] (reset! res# (do ~@body)))))
     @res#))


(def widgets-by (atom {:id {} :class {}}))

(defn by-id
  "Returns a widget with the given id."
  [id]
  (get (@widgets-by :id) id))

(defn add-ids! [widget]
  (swap! widgets-by assoc-in [:id (get-id widget)] widget)
  (doseq [child (get-children widget)]
    (add-ids! child)))

(defn init-widgets!
  "Initializes the widget and all its children. Returns widget."
  [widget]
  (init! widget)
  (doseq [child (get-children widget)]
    (with-parent (get-swt-object widget) (init-widgets! child)))
  widget)

(defmacro defgui
  "TODO: doc"
  ([name args widgets]
     `(defgui ~name nil ~args ~widgets))
  ([name doc-string args widgets]
     `(defn ~name {:doc ~doc-string ::gui true}
        [~@args]
        (fn [] (let [widgets# ~widgets]
                 (add-ids! widgets#)
                 (init-widgets! widgets#))))))


(defn dispose-if-not [this]
  (when-not (.isDisposed this)
    (.dispose this)))

(def ^{:dynamic true :doc "TODO: doc"} *display*)

(defmacro with-display
  "Evaluates body in the try block, with *display* bound to the given
   value, then calls .dispose on it."
  [display & body]
  `(binding [*display* ~display]
     (try ~@body (finally (dispose-if-not *display*)))))

(def pre-run-hooks (atom []))

(defn add-pre-run-hook! [f]
  (swap! pre-run-hooks conj f))

(defn call-pre-run-hooks []
  (doseq [f @pre-run-hooks] (f)))

(defn run!
  "TODO: doc"
  [gui]
  (with-display (Display.)
    (let [shell (get-swt-object (with-parent *display* (gui)))] 
      (call-pre-run-hooks)
      (try
        (.open shell)
        (while (not (.isDisposed shell))
          (when-not (.readAndDispatch *display*)
            (.sleep *display*)))
        (finally
         (dispose-if-not shell))))))


;;; Misc -----------------------------------------------------------------------

(defn color [r g b]
  (Color. *display* r g b))

(defn widget?  [x] (instance? org.eclipse.swt.widgets.Widget x))
(defn control? [x] (instance? org.eclipse.swt.widgets.Control x))
(defn dialog?  [x] (instance? org.eclipse.swt.widgets.Dialog x))
