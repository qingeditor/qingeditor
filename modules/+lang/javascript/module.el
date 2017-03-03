;;; qingeditor --- a distribution of Emacs editor
;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; The editor-base module class
;;; Code:

(qingeditor/define-module
 javascript
 "The javascript config module"
 :has-extra-funcs-defs t
 :has-extra-config t
 :require-packages
 '(coffee-mode
   (company :has-init nil)
   (company-tern :toggle (qingeditor/cls/package-usedp (qingeditor/gmodulemgr) 'company))
   flycheck
   ggtags
   helm-gtags
   js-doc
   js2-mode
   js2-refactor
   json-mode
   json-snatcher
   (tern :toggle (qingeditor/javascript/tern-detect))
   web-beautify
   skewer-mode
   livid-mode)
 )
