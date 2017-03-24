;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(qingeditor/define-module
 auto-completion
 "The auto-completion config module"
 :has-extra-config t
 :has-extra-funcs-defs t
 :require-packages
 '(company
   (helm-company :toggle (qingeditor/modulemgr/package-usedp 'helm)))
 )
