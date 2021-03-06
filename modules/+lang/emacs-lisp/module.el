;;; qingeditor --- a distribution of Emacs editor
;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(qingeditor/define-module
 emacs-lisp
 "The emacs-lisp config module"
 :has-extra-funcs-defs t
 :has-extra-config t
 :require-packages
 '(
   auto-compile
   company
   eldoc
   elisp-slime-nav
   (emacs-lisp :location built-in)
   flycheck
   ggtags
   helm-gtags
   (ielm :location built-in)
   macrostep
   semantic
   smartparens
   srefactor))
