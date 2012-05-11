(ns sweety.widgets
  (:refer-clojure :exclude [list]) 
  (:use [sweety.defwidget :only [defwidget defhooks]])
  (:import (org.eclipse.swt.widgets Control)
           (org.eclipse.swt.graphics Point Rectangle)))

(letfn [(vec->pt [[x y]] (Point. x y))
        (pt->vec [p] [(.x p) (.y p)])
        (vec->rect [[x y w h]] (Rectangle. x y w h))
        (rect->vec [r] [(.x r) (.y r) (.width r) (.height r)])]

  (defhooks Control
    (Size vec->pt pt->vec)
    (Location vec->pt pt->vec)
    (Bounds vec->rect rect->vec)))


;;; Widgets declaration --------------------------------------------------------

(defwidget org.eclipse.swt.browser.Browser)

(defwidget org.eclipse.swt.widgets.Button)
(defwidget org.eclipse.swt.widgets.Canvas)
(defwidget org.eclipse.swt.widgets.Combo)
(defwidget org.eclipse.swt.widgets.Composite)
(defwidget org.eclipse.swt.widgets.CoolBar)
(defwidget org.eclipse.swt.widgets.DateTime)
(defwidget org.eclipse.swt.widgets.ExpandBar)
(defwidget org.eclipse.swt.widgets.Group)
(defwidget org.eclipse.swt.widgets.Label)
(defwidget org.eclipse.swt.widgets.Link)
(defwidget org.eclipse.swt.widgets.List)
(defwidget org.eclipse.swt.widgets.Menu)
(defwidget org.eclipse.swt.widgets.ProgressBar)
(defwidget org.eclipse.swt.widgets.Sash)
(defwidget org.eclipse.swt.widgets.Shell)
(defwidget org.eclipse.swt.widgets.Slider)
(defwidget org.eclipse.swt.widgets.Scale)
(defwidget org.eclipse.swt.widgets.Spinner)
(defwidget org.eclipse.swt.widgets.TabFolder)
(defwidget org.eclipse.swt.widgets.Table)
(defwidget org.eclipse.swt.widgets.Text)
(defwidget org.eclipse.swt.widgets.ToolBar)
(defwidget org.eclipse.swt.widgets.Tray)
(defwidget org.eclipse.swt.widgets.Tree)

(defwidget org.eclipse.swt.widgets.FileDialog)
(defwidget org.eclipse.swt.widgets.MessageBox)

;;;  Custom
(defwidget org.eclipse.swt.custom.CTabFolder)
(defwidget org.eclipse.swt.custom.ScrolledComposite)
(defwidget org.eclipse.swt.custom.StyledText)
