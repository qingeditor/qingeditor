;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(qingeditor/define-module
 c-c++
 "The emacs-lisp config module"
 :has-extra-funcs-defs t
 :has-extra-config t
 :require-packages
 '(cc-mode
   disaster
   clang-format
   cmake-mode
   company
   (company-c-headers :toggle (qingeditor/modulemgr/package-usedp 'company))
   company-ycmd
   flycheck
   gdb-mi
   ggtags
   helm-cscope
   helm-gtags
   semantic
   srefactor
   stickyfunc-enhance
   ycmd
   xcscope))
