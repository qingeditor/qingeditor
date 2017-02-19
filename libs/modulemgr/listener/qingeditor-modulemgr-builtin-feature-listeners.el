;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; Define the module builtin feature listeners

(defun qingeditor/modulemgr/extra-func-defs-handler (event)
  "Hand the extra function definitions of the module."
  (let* ((module (qingeditor/cls/get-module event))
         (has-func-defs (qingeditor/cls/provide-extra-func-definitions module))
         (func-filename (concat (qingeditor/cls/get-module-dir module) "func.el")))
    (when (and has-func-defs
               (file-exists-p func-filename))
      (load func-filename))))

(provide 'qingeditor-modulemgr-builtin-feature-listeners)
