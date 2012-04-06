(ns sweety.test.core
  (:refer-clojure :exclude [list assoc!])
  (:use sweety.core :reload)
  (:use midje.sweet
        midje.util)
  (:import java.util.Date
           org.eclipse.swt.SWT
           (org.eclipse.swt.widgets Display Listener
                                    Shell Composite Group
                                    Label Button List Canvas Scale Text
                                    FileDialog MessageBox)))

(testable-privates sweety.core getters setters)


(fact "getters and setters"
  (getters String) => '(getChars getBytes getClass)
  (setters String) => ()
  (setters java.util.Date) => '(setTime setYear setMonth setDate setHours setMinutes setSeconds))

(fact "method->keyword"
  (method->keyword 'getSomething) => :something
  (method->keyword 'setSomethingDifferent) => :something-different)

(fact "keyword+method-pairs"
  (keyword+method-pairs identity '[setFoo getBar])
  => '[:foo setFoo :bar getBar])


(deftype-for-widget ADate Date []
  {:deref (.getDate widget)})

(fact "the Widget protocol and keyword lookup"
  (let [date (ADate. (Date.))]
    (-assoc date :hours 12)
    (-> date .widget .getHours) => 12
    (:hours date) => 12
    @date => (-> date .widget .getDate)))


(fact "method-call?"
  (method-call? '(.foo)) => true
  (method-call? '(foo)) => false
  (method-call? :foo) => false)

(fact "args-for-defwidget"
  (args-for-defwidget []) => [#{} () () ()]
  (args-for-defwidget [#{:foo}]) => [#{:foo} () () ()]
  (args-for-defwidget [:prop :val :another :one])
  => [#{} () [:prop :val :another :one] ()]
  (args-for-defwidget '[:prop :val (child)]) => [#{} () [:prop :val] '[(child)]]
  (args-for-defwidget '[(.method) (child)]) => [#{} '[(.method)] () '[(child)]]
  (args-for-defwidget '[#{:init}
                        (.setter) (.another)
                        :a-key a-val :b-key b-val
                        (child) (another-one)])
  => [#{:init}
      '[(.setter) (.another)]
      '[:a-key a-val :b-key b-val]
      '[(child) (another-one)]])

(fact "reduce-init"
  (reduce-init #{}) => SWT/NULL
  (reduce-init #{:ok}) => SWT/OK
  (reduce-init #{:f1 SWT/F2 :f3}) => (bit-or SWT/F1 SWT/F2 SWT/F3)) 

(defonce test-shell (Shell.))

(fact
  (with-parent test-shell
    (let [a (button (.setText "foo"))
          b (button :text "bar")]
      a => (partial instance? AButton)
      @a => "deref"
      (-> a .widget .getText) => "foo"
      (:text b) => "bar"))
  (-> #'button meta :doc) => "creates a button")
