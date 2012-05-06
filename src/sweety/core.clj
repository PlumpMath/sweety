(ns sweety.core
  (:refer-clojure :exclude [assoc!])
  (:use [sweety.defwidget :only [-assoc! -init!]]
        [sweety.constants :only [events]])
  (:import (org.eclipse.swt.graphics Color) 
           (org.eclipse.swt.widgets Listener)))

(defn assoc!
  "Sets the property 'key' of the given widget to val. Keys are
   obtained from setters by replacing CamelCase with hyphens and
   keywordizing the result. E.g. .setText becomes :text.
   See also: update!"
  [widget & keys+vals]
  (doseq [[k v] (partition 2 keys+vals)]
    (-assoc! widget k v))
  widget)

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

;; FIXME: rework
;; (defn create!
;;   "Given a gui, initializes it, opens the root widget (assuming it's a
;;   shell and has name defined) and starts a service loop. You should
;;   call it only once to start the main shell of your application.
;;   Example: (with-new-display (init! my-gui) (create! my-gui))"
;;   [gui]
;;   (let [shell (-> gui root .widget)]
;;     (try
;;       (.open shell)
;;       (while (not (.isDisposed shell))
;;         (when-not (.readAndDispatch *display*)
;;           (.sleep *display*)))
;;       (finally
;;        (dispose-if-not shell)))))


;;; defgui --------------------------------------------------------------------- 

(defn init-widget
  "Creates and initializes the widget and all its children. Returns widget."
  [[root & children :as widgets]]
  (letfn [(init-widget-children [[parent & children]]
            (-init! parent)
            (doseq [child children]
              (with-parent parent
                (init-widget-children child))))]
    (init-widget-children widgets)))

(defmacro defgui
  ([name args widgets]
     `(defgui ~name nil ~args ~widgets))
  ([name doc-string args widgets]
     `(defn ~name {:doc ~doc-string ::gui true}
        [~@args]
        (init-widget ~widgets))))


;;; Misc -----------------------------------------------------------------------

(defn color [r g b]
  (Color. *display* r g b))

(defn widget?  [x] (instance? org.eclipse.swt.widgets.Widget x))
(defn control? [x] (instance? org.eclipse.swt.widgets.Control x))
(defn dialog?  [x] (instance? org.eclipse.swt.widgets.Dialog x))
