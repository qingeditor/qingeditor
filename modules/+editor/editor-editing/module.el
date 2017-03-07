;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(qingeditor/define-module
 editor-editing
 "The editor-editing config module"
 :has-extra-funcs-defs t
 :has-extra-config t
 :require-packages
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
   ws-butler)
 )
