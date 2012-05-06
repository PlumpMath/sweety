(ns sweety.layouts
  (:require [clojure.string :as string])
  (:import (org.eclipse.swt.layout RowLayout RowData
                                   GridLayout GridData
                                   FillLayout FillData
                                   FormLayout FormData FormAttachment)))

;; TODO: tests
;; TODO: refactor

(defn- camel-case [s]
  (string/join (map string/capitalize (string/split s #"-"))))

(defn keyword->field [kw]
  (let [[f & rest] (-> kw name camel-case seq)]
    (symbol (apply str (Character/toLowerCase f) rest))))

(defmacro layout [type & args]
  (let [class (case type
                :row RowLayout
                :grid GridLayout
                :fill FillLayout
                :form FormLayout
                (throw (IllegalArgumentException. "Unsupported layout type.")))
        layout (gensym "layout")]
    (if (seq args)
      `(let [~layout (new ~class)]
         ~@(for [[k v] (partition 2 args)]
             `(set! (. ~layout ~(keyword->field k)) ~v))
         ~layout)
      `(new ~class))))

(defmacro form-layout [& args] `(layout :form ~@args))
(defmacro row-layout  [& args] `(layout :row ~@args))
(defmacro grid-layout [& args] `(layout :grid ~@args))
(defmacro fill-layout [& args] `(layout :fill ~@args))


;;--------------------------------------------------------------------------------
;; WARNING: Heavy copypasting here; to be refactored
(defmacro layout-data [type & args]
  (let [class (case type
                :row RowData, :grid GridData
                :fill FillData, :form FormData
                (throw (IllegalArgumentException. "Unsupported layout data type.")))
        layout (gensym "layout")]
    (if (seq args)
      `(let [~layout (new ~class)]
         ~@(for [[k v] (partition 2 args)]
             `(set! (. ~layout ~(keyword->field k)) ~v))
         ~layout)
      `(new ~class))))

(defmacro form-data [& args] `(layout-data :form ~@args))
(defmacro row-data  [& args] `(layout-data :row ~@args))
(defmacro grid-data [& args] `(layout-data :grid ~@args))
(defmacro fill-data [& args] `(layout-data :fill ~@args))
;;--------------------------------------------------------------------------------

(defn form-attachment [percents-or-widget shift]
  (FormAttachment. percents-or-widget shift))
