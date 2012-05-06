(ns sweety.widgets
  (:use [sweety.defwidget :only [defwidget]])
  (:import (org.eclipse.swt.graphics Point Rectangle)))

;;; Widgets declaration -------------------------------------------------------- 

(letfn [(vec->pt [[x y]] (Point. x y))
        (vec->rect [[x y w h]] (Rectangle. x y w h))]

  (def ^:private common-hooks
    {'setSize vec->pt
     'setLocation vec->pt
     'setBounds vec->rect}))

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
