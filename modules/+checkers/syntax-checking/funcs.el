;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qingeditor/syntax-checking/add-flycheck-hook (mode)
  "Use flycheck in `mode' by default, if `qingeditor/syntax-checking/syntax-checking-enable-by-default'
is true."
  (when (and qingeditor/syntax-checking/syntax-checking-enable-by-default
             (listp flycheck-global-modes)
             (not (eq 'not (car flycheck-global-modes))))
    (push mode flycheck-global-modes)))
