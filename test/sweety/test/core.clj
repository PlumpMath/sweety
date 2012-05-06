(ns sweety.test.core
  (:refer-clojure :exclude [assoc!])
  (:use sweety.core [sweety.defwidget :only [WidgetInstance]] :reload)
  (:use midje.sweet))

(tabular "init-widget"
  (fact
    (let [res (atom [])
          f #(reify WidgetInstance
               (-init! [this] (swap! res conj %)))]
      (init-widget input) => nil
      @res => result))

  input                           result
  [(f 1)]                         [1]
  [(f 1) [(f 2) [(f 3)]]]         [1 2 3]
  [(f 1) [(f 2) [(f 3)]] [(f 4)]] [1 2 3 4])

;;; Example --------------------------------------------------------------------

(comment
  (defgui main-gui []
    (shell ::main-shell #{:title :resize}
      :size [640 480]
      (button ::a-button#buttons #{:push}
        :text "click"
        :size [100 50])
      (label ::lable
        :text "unnamed"
        :location [0 50]
        :size [100 50])))

  (deflistener a-button :mouse-down [e]
    (update! a-button :text str "!"))

  (defn start []
    (with-new-display
      (with-parent *display*
        (main-gui)))))
