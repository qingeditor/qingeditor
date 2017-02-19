;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The module resolve listener handler
(defun qingeditor/modulemgr/module-resolve (event)
  "This function handle the module resolve, it make a `qingeditor/modulemgr/module'
 object from a module `specific'."
  (let* ((module (qingeditor/cls/get-module event))
         (module-spec (qingeditor/cls/get-module-spec event)))
    (prin1 module-spec)))

(provide 'qingeditor-modulemgr-resolve-listener)
