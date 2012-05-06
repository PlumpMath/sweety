(ns sweety.constants
  "TODO: doc"
  (:use [sweety.utils :only [de-camel lower-case]])
  (:import org.eclipse.swt.SWT))

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
