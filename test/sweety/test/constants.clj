(ns sweety.test.constants
  (:use sweety.constants :reload)
  (:use midje.sweet
        [midje.util :only [testable-privates]])
    (:import org.eclipse.swt.SWT))

(fact "de-camel"
  (de-camel "MouseUp") => "mouse-up"
  (de-camel "Dispose") => "dispose")

(fact "lower-case"
  (lower-case "NULL") => "null"
  (lower-case "COLOR_RED") => "color-red")

(fact "events and constants"
  (:mouse-up events) => SWT/MouseUp
  (:null constants) => SWT/NULL)
