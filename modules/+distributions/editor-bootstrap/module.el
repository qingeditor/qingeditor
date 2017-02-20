;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The editor-bootstrap module class

(defclass qingeditor/module/editor-bootstrap
  (qingeditor/modulemgr/module)
  ()
  :documentaion "The editor-bootstrap module class")

(defmethod qingeditor/cls/define-package-specs
  ((this qingeditor/module/editor-bootstrap))
  "Declare the require package specs of this module."
  '((async :stage bootstrap)
    (bind-map :stage bootstrap)
    (bind-key :stage bootstrap)
    (diminish :stage bootstrap)
    (hydra :stage bootstrap)
    (use-package :stage bootstrap)
    (which-key :stage bootstrap)))
