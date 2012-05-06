(ns sweety.test.utils
  (:use sweety.utils :reload)
  (:use midje.sweet))

(fact "de-camel"
  (de-camel "MouseUp") => "mouse-up"
  (de-camel "Dispose") => "dispose")

(fact "lower-case"
  (lower-case "NULL") => "null"
  (lower-case "COLOR_RED") => "color-red")

(fact "getters and setters"
  (getters String) => '(getChars getBytes getClass)
  (setters String) => ()
  (setters java.util.Date) => '(setTime setYear setMonth setDate setHours setMinutes setSeconds))

(fact "method->keyword"
  (method->keyword 'getSomething) => :something
  (method->keyword 'setSomethingDifferent) => :something-different)
