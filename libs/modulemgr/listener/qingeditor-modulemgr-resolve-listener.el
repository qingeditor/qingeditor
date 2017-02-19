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
         (module-name (qingeditor/cls/get-name module))
         (module-spec (qingeditor/cls/get-module-spec event))
         (disabled (qingeditor/mplist-get module-spec :disabled-for))
         (enabled (if (memq :enable-for module-spec)
                      (qingeditor/mplist-get module-spec :enable-for)
                    'unspecified))
         (variables (qingeditor/mplist-get module-spec :variables)))
    (oset module :disabled-for disabled)
    (oset module :enable-for enabled)
    (oset module :variables variables)
    (qingeditor/modulemgr/resolve-package module (qingeditor/cls/get-modulemgr event))
    ))

(defun qingeditor/modulemgr/resolve-package (module modulemgr)
  "Resolve the packages of `module'."
  (let ((module-package-specs (qingeditor/cls/get-require-package-specs module)))
    (dolist (spec module-package-specs)
      (let* ((pkg-name (if (listp spec) (car spec) spec))
             (pkg (qingeditor/cls/get (oref modulemgr :pakage-repo) pkg-name))
             (excluded (when (listp spec) (plist-get (cdr spec) :excluded)))
             (location (when (listp spec) (plist-get (cdr spec) :location))))
        (qingeditor/cls/set-property pkg :excluded excluded)

        ))))

(provide 'qingeditor-modulemgr-resolve-listener)
