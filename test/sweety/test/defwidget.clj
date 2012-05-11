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


(fact "name-keyword?"
  (name-keyword? ::name) => true
  (name-keyword? :#class) => true
  (name-keyword? :foo) => false)

(fact "method-call?"
  (method-call? '(.foo)) => true
  (method-call? '(. foo bar)) => true
  (method-call? '(foo)) => false
  (method-call? :foo) => false)

(fact "args-for-defwidget"
  (args-for-defwidget '[::name #{:init} (.method) :key val (child)])
  => '{:name ::name :style #{:init} :opts {}
       :methods [(.method)] :properties [:key val] :children [(child)]}

  (args-for-defwidget [])
  => {:name nil :style #{} :opts {}
      :methods () :properties () :children ()}
  
  (args-for-defwidget [#{:foo}])
  => {:name nil :style #{:foo} :opts {}
      :methods () :properties () :children ()}
  
  (args-for-defwidget [:prop :val :another :one])
  => {:name nil :style #{} :opts {}
      :methods () :properties [:prop :val :another :one] :children ()}
  
  (args-for-defwidget '[:prop :val (child)])
  => {:name nil :style #{} :opts {}
      :methods () :properties [:prop :val] :children '[(child)]}
  
  (args-for-defwidget '[(.method) (child)])
  => {:name nil :style #{} :opts {}
      :methods '[(.method)] :properties () :children '[(child)]}
  
  (args-for-defwidget [{:opts true} :key :val])
  => {:name nil :style #{} :opts {:opts true}
      :methods () :properties [:key :val] :children ()}
  
  (args-for-defwidget '[#{:init}
                        (.setter) (.another)
                        :a-key a-val :b-key b-val
                        (child) (another-one)])
  => {:name nil :style #{:init} :opts {}
      :methods '[(.setter) (.another)]
      :properties '[:a-key a-val :b-key b-val]
      :children '[(child) (another-one)]}

  (args-for-defwidget '[::name :key val])
  => {:name ::name :style #{} :opts {}
      :methods () :properties '[:key val] :children ()})


(fact "compute-style"
  (compute-style #{}) => SWT/NULL
  (compute-style #{:ok}) => SWT/OK
  (compute-style #{:f1 SWT/F2 :f3}) => (bit-or SWT/F1 SWT/F2 SWT/F3))

(tabular "parse-name-keyword"
  (fact (parse-name-keyword input) => [id classes])
  input      id     classes
  ::foo      ::foo  []
  ::foo#bar  ::foo  [:bar]
  :#bar      nil    [:bar]
  ::foo#a#b  ::foo  [:a :b])
