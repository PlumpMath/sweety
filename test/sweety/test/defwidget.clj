(ns sweety.test.defwidget
  (:use sweety.defwidget :reload)
  (:use midje.sweet)
  (:import (java.util Date)
           (org.eclipse.swt SWT)))

(fact "keyword+method-pairs"
  (keyword+method-pairs identity '[setFoo getBar])
  => '[:foo setFoo :bar getBar])


(comment
  (deftype-for-widget ADate Date
   {:deref (.getDate widget)})

 (fact "the Widget protocol and keyword lookup"
   (let [date (ADate. (Date.))]
     (-assoc! date :hours 12)
     (-> date .widget .getHours) => 12
     (:hours date) => 12
     @date => (-> date .widget .getDate))))


(fact "method-call?"
  (method-call? '(.foo)) => true
  (method-call? '(foo)) => false
  (method-call? :foo) => false)

(fact "args-for-defwidget"
  (args-for-defwidget '[::name #{:init} (.method) :key val (child)])
  => '[::name #{:init} [(.method)] [:key val] [(child)]]
  (args-for-defwidget []) => [nil #{} () () ()]
  (args-for-defwidget [#{:foo}]) => [nil #{:foo} () () ()]
  (args-for-defwidget [:prop :val :another :one])
  => [nil #{} () [:prop :val :another :one] ()]
  (args-for-defwidget '[:prop :val (child)]) => [nil #{} () [:prop :val] '[(child)]]
  (args-for-defwidget '[(.method) (child)]) => [nil #{} '[(.method)] () '[(child)]]
  (args-for-defwidget '[#{:init}
                        (.setter) (.another)
                        :a-key a-val :b-key b-val
                        (child) (another-one)])
  => [nil #{:init}
      '[(.setter) (.another)]
      '[:a-key a-val :b-key b-val]
      '[(child) (another-one)]])

(fact "reduce-init"
  (reduce-init #{}) => SWT/NULL
  (reduce-init #{:ok}) => SWT/OK
  (reduce-init #{:f1 SWT/F2 :f3}) => (bit-or SWT/F1 SWT/F2 SWT/F3))

(tabular "parse-name-keyword"
  (fact (parse-name-keyword input) => {:id id :classes classes})
  input      id     classes
  ::foo      ::foo  []
  ::foo#bar  ::foo  [:bar]
  :#bar      nil    [:bar]
  ::foo#a#b  ::foo  [:a :b])
