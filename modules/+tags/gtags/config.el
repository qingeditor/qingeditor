;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defvar qingeditor/gtags/enabled-by-default t
  "Whether or not to enabled ggtags-mode.")

(qingeditor/define-jump-handlers tcl-mode)
(qingeditor/define-jump-handlers vhdl-mode)
(qingeditor/define-jump-handlers awk-mode)
(qingeditor/define-jump-handlers compilation-mode)
(qingeditor/define-jump-handlers shell-mode)
