;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; Define the module builtin feature listeners

(defun qingeditor/modulemgr/extra-func-defs-handler (event)
  "Hand the extra function definitions of the module."
  (let* ((module (qingeditor/cls/get-module event))
         (has-func-defs (qingeditor/cls/provide-extra-func-definitions module))
         (func-filename (concat (qingeditor/cls/get-module-dir module) "funcs.el")))
    (when (and has-func-defs
               (file-exists-p func-filename))
      (load func-filename))))

(defun qingeditor/modulemgr/alias-defs-handler (event)
  "Give the command a short namespace."
  (let* ((module (qingeditor/cls/get-module event))
         (alias-filename (concat (qingeditor/cls/get-module-dir module) "alias-defs.el")))
    (when (file-exists-p alias-filename)
      (load alias-filename))))

(defun qingeditor/modulemgr/extra-module-config-handler (event)
  "Provide some extra config settup."
  (let* ((module (qingeditor/cls/get-module event))
         (extra-config-filename (concat (qingeditor/cls/get-module-dir module) "config.el")))
    (when (and (qingeditor/cls/provide-extra-module-config module)
               (file-exists-p extra-config-filename))
      (load extra-config-filename))))

(defun qingeditor/modulemgr/init-method-init-handler (event)
  "Load package init file."
  (let* ((module (qingeditor/cls/get-module event))
         (init-method-filename
          (concat (qingeditor/cls/get-module-dir module)
                  "package-init-defs.el")))
    (when (file-exists-p init-method-filename)
      (load init-method-filename))))

(provide 'qingeditor-modulemgr-builtin-feature-listeners)
