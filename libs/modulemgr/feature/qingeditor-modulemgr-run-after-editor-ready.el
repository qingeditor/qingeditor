;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The run after editor init ready interface

(defclass qingeditor/modulemgr/feature/run-after-editor-ready ()
  ()
  :abstract t
  :documentation "The run after editor init ready interface.
If you have some function need to be invoked after `qingeditor' initialized.
In this method you can return a list of current module's method symbol. If
you must ensure the invoked order, you can return a list like this:
`((,#'method-symbol1 1) (,#'method-symbol2 2)).
if you don't care about invoke order, you can just return:
(list #'method-symbol1 #'method-symbol2).")

(defmethod qingeditor/cls/get-callables ((this qingeditor/modulemgr/feature/run-after-editor-ready))
  "return callables list that want to be invoked after `qingeditor' initialized.
you can return list like:
`((,#'method-symbol1 1) (,#'method-symbol2 2))
or
(list #'method-symbol1 #'method-symbol2)."
  nil)

(provide 'qingeditor-modulemgr-run-after-editor-ready)
