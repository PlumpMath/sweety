(ns sweety.core
  (:use [sweety.defwidget :only [with-parent init! add-init-fn!
                                 get-swt-object get-id get-children]]
        [sweety.constants :only [events]])
  (:import (org.eclipse.swt.graphics Color)
           (org.eclipse.swt.widgets Listener Display)))

;;; Widgets lookup and defgui --------------------------------------------------

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

(def post-init-hooks (atom []))

(defn add-post-init-hook! [f]
  (swap! post-init-hooks conj f))

(defn call-post-init-hooks! []
  (doseq [f @post-init-hooks]
    (f)))

(defmacro defgui
  "TODO: doc"
  ([name args widgets]
     `(defgui ~name nil ~args ~widgets))
  ([name doc-string args widgets]
     `(defn ~name {:doc ~doc-string ::gui true}
        [~@args]
        (fn [] (let [widgets# ~widgets]
                 (add-ids! widgets#)
                 (init-widgets! widgets#)
                 (call-post-init-hooks!)
                 widgets#)))))


;;; Listeners ------------------------------------------------------------------

(defn get-event [event]
  {:pre [(or (events event) (some #{event} (vals events)))]}
  (if (keyword? event)
    (get events event)
    event))

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
  See also: listening?, remove-listeners!, get-listeners, deflistener."
  ([widget event f]
     (doto (get-swt-object widget)
       (.addListener (get-event event) (listener f))))
  ([widget event f & events+functions]
     (doseq [[e f] (concat [event f] events+functions)]
       (add-listener! widget e f))
     widget))

(defn listening?
  "Returns true if there is a listener registered for the given event,
  false otherwise."
  [widget event]
  (.isListening (get-swt-object widget) (get-event event)))

(defn get-listeners
  "Returns a set of all listeners registered for the given event.
  See also: get-listener."
  [widget event]
  (set (.getListeners (get-swt-object widget) (get-event event))))

(defn get-listener
  "If there is only one listener registered for the given event,
  returns it."
  [widget event]
  (first (get-listeners widget event)))

(defn remove-listener!
  "Given a listener object, unregisters it from recieving events of
  given type."
  [widget event listener]
  (doto (get-swt-object widget)
    (.removeListener (get-event event) listener)))

(defn remove-listeners!
  "Removes all listeners on the event from the given widget.
  See also: remove-listener!."
  [widget event]
  (doseq [listener (get-listeners widget event)]
    (remove-listener! widget event listener)))

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
  `(add-post-init-hook!
    (fn []
      (doto (by-id ~id)
        (remove-listeners! ~event) ; this allows to recompile deflistener calls 
        (add-listener! ~event (fn [~@args] ~@body))))))


;;; Run ------------------------------------------------------------------------

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

(defn run!
  "TODO: doc"
  [gui]
  (with-display (Display.)
    (let [shell (get-swt-object (with-parent *display* (gui)))]
      (try
        (.open shell)
        (while (not (.isDisposed shell))
          (when-not (.readAndDispatch *display*)
            (.sleep *display*)))
        (finally
         (dispose-if-not shell))))))


;;; Misc utilities -------------------------------------------------------------

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


;;; Misc -----------------------------------------------------------------------

(defn color [r g b]
  (Color. *display* r g b))

(defn widget?  [x] (instance? org.eclipse.swt.widgets.Widget x))
(defn control? [x] (instance? org.eclipse.swt.widgets.Control x))
(defn dialog?  [x] (instance? org.eclipse.swt.widgets.Dialog x))
