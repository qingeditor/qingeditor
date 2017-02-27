;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define some global variables and consts

(defgroup qingeditor nil "qingeditor variables group"
  :group 'starter-kit
  :prefix 'qingeditor-)

(defvar qingeditor/post-user-cfg-hook nil  "当用户配置函数调用之后执行的钩子函数。")
(defvar qingeditor/post-user-cfg-hook-run nil
  "标志变量，判断`qingeditor/post-user-config-hook'是否已经运行。")

(defvar qingeditor/editor-ready-hooks nil
  "when qineditor load finished, run hook in this list.")

(defvar qingeditor/insecure nil
  "If non-nil force `qingeditor' to operate without secured protocols.")

(defvar qingeditor/post-theme-change-hook nil
  "theme change hooks.")

(defvar qingeditor/global-listeners-pool nil
  "Collect the listeners of shared eventmgr, we can use this
list to prevent listener handler memory leak.")

(defconst qingeditor/window-resize-event "window-resize-event"
  "When `emacs' frame changed it's size, `qingeditor' will dispatch this event.")

(defconst qingeditor/display-system-ready-event "display-system-ready-event"
  "When display system initialized, dispatch this event.")

(defconst qinegditor/theme-changed-event "theme-changed-event"
  "After a new theme loaded, dispatch this event.")

(defconst qingeditor/editor-ready-event "editor-ready-event"
  "After qingeditor finish all the setup procedure, dispatch this event.")

(defvar qingeditor/initialized nil
  "`t' if the qingeditor finished init.")

(defvar qingeditor/command-properties nil
  "Specifications made by `qingeditor/define-command'.")

(defvar qingeditor/interactive-alist nil
  "Association list of qingeditor-specific interactive codes.")

(defcustom qingeditor/track-eol t
  "If non-nil line moves after a call to `qingeditor/end-of-line' stay at eol.
This is analogous to `track-eol' but deals with the end-of-line
interpretation of qingeditor."
  :type 'boolean
  :group 'qingeditor)

(defcustom qingeditor/move-beyond-eol nil
  "Whether the cursor is allowed to move past the last character of 
a line."
  :type 'boolean
  :group 'qingeditor)

(defcustom qingeditor/indent-convert-tabs t
  "If non-nil `qingeditor/indent' converts between leading tabs and spaces.
  Whether tabs are converted to spaces or vice versa depends on the
  value of `indent-tabs-mode'."
  :type 'boolean
  :group 'qingeditor)


(qingeditor/define-local-var qingeditor/this-type nil
  "current motion type.")

(provide 'qingeditor-gvars)
