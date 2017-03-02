;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defclass qingeditor/module/javascript
  (qingeditor/modulemgr/module)
  ()
  :documentaion "The javascript module class")

(defmethod qingeditor/cls/define-package-specs
  ((this qingeditor/module/javascript))
  "Declare the require package specs of this module."
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
    livid-mode))

(defmethod qingeditor/cls/provide-extra-func-definitions
  ((this qingeditor/module/javascript))
  t)
