;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define some global variables and consts

(defgroup qingeditor nil "`QingEditor'自定义分组"
  :group 'starter-kit
  :prefix 'qingeditor-)

(defvar qingeditor/post-user-cfg-hook nil  "当用户配置函数调用之后执行的钩子函数。")
(defvar qingeditor/post-user-cfg-hook-run nil
  "标志变量，判断`qingeditor/gvars/post-user-config-hook'是否已经运行。")

(defvar qingeditor/insecure nil
  "If non-nil force `qingeditor' to operate without secured protocols.")

(defvar qingeditor/post-theme-change-hook nil
  "当风格变化的时候执行的钩子函数。")

(defvar qingeditor/global-listeners-pool nil
  "Collect the listeners of shared eventmgr, we can use this
list to prevent listener handler memory leak.")

(defconst qingeditor/window-resize-event "window-resize-event"
  "When `emacs' frame changed it's size, `qingeditor' will dispatch this event.")

(defconst qingeditor/display-system-ready-event "display-system-ready-event"
  "When display system initialized, dispatch this event.")

(provide 'qingeditor-gvars)
