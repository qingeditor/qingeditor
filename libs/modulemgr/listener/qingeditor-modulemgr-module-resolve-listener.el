;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The module resolve function

(defun qingeditor/modulemgr/module-resolve (event)
  "This function handl the module resolve, it make a `qingeditor/modulemgr/module' object
from a module `specific'."
  (prin1 (qingeditor/cls/get-module-info event))
  nil)

(provide 'qingeditor-modulemgr-module-resolve-listener)
