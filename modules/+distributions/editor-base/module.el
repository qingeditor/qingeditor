;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; The editor-base module class

(defclass qingeditor/module/editor-base
  (qingeditor/modulemgr/module)
  ()
  :documentaion "The editor-base module class")

(defmethod qingeditor/cls/define-package-specs
  ((this qingeditor/module/editor-base))
  "Declare the require package specs of this module."
  '((abbrev :location built-in)))

(defmethod qingeditor/cls/provide-extra-func-definitions
  ((this qingeditor/module/editor-base))
  t)

(defmethod qingeditor/cls/provide-extra-module-config
  ((this qingeditor/module/editor-base))
  t)
