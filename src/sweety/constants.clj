(ns sweety.constants
  "TODO: doc"
  (:refer-clojure :exclude [min max time short long])
  (:require [clojure.string :as string])
  (:import org.eclipse.swt.SWT)) 

(defn de-camel [s]
  (->> (partition-by #(Character/isUpperCase %) s)
       (partition 2)
       (map #(string/lower-case (apply str (apply concat %))))
       (string/join "-")))

(defn lower-case [s]
  (-> s (string/replace "_" "-") string/lower-case))

(defn generate-swt-constants-map [f names]
  (into {} (map (fn [s] [(-> s f keyword)
                         (.get (.getField SWT s) nil)])
                names)))

(let [events #{"KeyDown" "KeyUp"
               "MouseDown" "MouseUp" "MouseMove" "MouseEnter" "MouseExit" "MouseDoubleClick"
               "Paint" "Move" "Resize" "Dispose" "Selection" "DefaultSelection"
               "FocusIn" "FocusOut" "Expand" "Collapse" "Iconify" "Deiconify"
               "Close" "Show" "Hide" "Modify" "Verify" "Activate" "Deactivate" "Help"
               "DragDetect" "Arm" "Traverse" "MouseHover" "HardKeyDown" "HardKeyUp"
               "MenuDetect" "SetData" "MouseWheel" "Settings"
               "EraseItem" "MeasureItem" "PaintItem" "ImeComposition"}]

  (def ^{:doc "TODO: doc"} events
    (generate-swt-constants-map de-camel events))

  (def ^{:doc "TODO: doc"} constants
    (generate-swt-constants-map lower-case
                                (->> (.getFields SWT)
                                     (map #(.getName %))
                                     (remove events)))))
