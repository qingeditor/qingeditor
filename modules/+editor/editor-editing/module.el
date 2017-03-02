;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defclass qingeditor/module/editor-editing
  (qingeditor/modulemgr/module)
  ()
  :documentaion "The editor-editing module class")

(defmethod qingeditor/cls/define-package-specs
  ((this qingeditor/module/editor-editing))
  "Declare the require package specs of this module."
  '(aggressive-indent
    avy
    (bracketed-paste :toggle (version<= emacs-version "25.0.92"))
    clean-aindent-mode
    eval-sexp-fu
    expand-region
    (hexl :location built-in)
    hungry-delete
    link-hint
    lorem-ipsum
    move-text
    (origami :toggle (eq 'origami qingeditor/config/folding-method))
    smartparens
    (qingeditor/whitespace-cleanup :location local)
    undo-tree
    uuidgen
    ws-butler))
