;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defclass qingeditor/module/editor-completion
  (qingeditor/modulemgr/module)
  ()
  :documentaion "The editor-completion module class")

(defmethod qingeditor/cls/define-package-specs
  ((this qingeditor/module/editor-completion))
  "Declare the require package specs of this module."
  '((default-helm-config :location built-in)
    (default-ivy-config :location built-in)
    (ido :location built-in)
    ido-vertical-mode))
