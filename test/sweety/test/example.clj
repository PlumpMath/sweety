(ns sweety.test.example
  (:use sweety.core
        [sweety.widgets :only [shell button label]]))

(defgui main-gui [x]
  (shell ::main-shell #{:title :resize}
    :size [640 480]
    (button ::a-button#buttons
      :text x
      :size [100 50])
    (label
      :text (format "label for %s" (:text (by-id ::a-button)))
      :location [0 50]
      :size [100 50])))

(deflistener ::a-button :mouse-down [e]
  (update! (by-id ::a-button) :text str "!"))

;; (run! (main-gui "test"))
