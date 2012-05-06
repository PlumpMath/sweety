(ns sweety.test.constants
  (:use sweety.constants :reload)
  (:use midje.sweet)
  (:import org.eclipse.swt.SWT))

;; TODO: add a couple of checks

(fact "events and constants"
  (:mouse-up events) => SWT/MouseUp
  (:null constants) => SWT/NULL)
