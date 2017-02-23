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
         (variables (qingeditor/mplist-get module-spec :variables))
         (package-init-list (oref module :package-init-list))
         (package-pre-init-list (qingeditor/cls/get-package-pre-init-list module))
         (package-post-init-list (qingeditor/cls/get-package-post-init-list module)))
    (oset module :disabled-for disabled)
    (oset module :enable-for enabled)
    (oset module :variables variables)
    (qingeditor/modulemgr/resolve-package
     module (qingeditor/cls/get-modulemgr event)
     package-init-list package-pre-init-list package-post-init-list)))

(defun qingeditor/modulemgr/resolve-package
    (module modulemgr init-list pre-init-list post-init-list)
  "Resolve the packages of `module'."
  (let ((module-package-specs (qingeditor/cls/get-require-package-specs module)))
    (dolist (spec module-package-specs)
      (let* ((pkg-name (if (listp spec) (car spec) spec))
             (pkg-name-str (symbol-name pkg-name))
             (pkg (qingeditor/cls/get (oref modulemgr :package-repo) pkg-name))
             (excluded (when (listp spec) (plist-get (cdr spec) :excluded)))
             (location (when (listp spec) (plist-get (cdr spec) :location)))
             (has-init (memq pkg-name init-list))
             (has-pre-init (memq pkg-name pre-init-list))
             (has-post-init (memq pkg-name post-init-list))
             (ownerp (or (and (eq (qingeditor/cls/get-from-source pkg) 'config)
                              (null (qingeditor/cls/get-owners pkg)))
                         has-init)))
        (qingeditor/cls/set-property pkg :excluded excluded)
        ;; setup package location
        (when location
          (let* ((pkg-from-source (qingeditor/cls/get-from-source pkg)))
            (if (and (listp location)
                     (eq (car location) 'recipe)
                     (eq (plist-get (cdr location) :fetcher) 'local))
                (cond
                 ((eq pkg-from-source 'module)
                  (let ((path (expand-file-name
                               (format "%s%s/%s.el"
                                       (qingeditor/cls/get-local-dir module)
                                       pkg-name-str
                                       pkg-name-str))))
                    (qingeditor/cls/set-property pkg :location `(recipe :fetcher file :path ,path))))
                 ((eq pkg-from-source 'config)
                  ;; TODO what is the local path for a packages owned by the user config file?
                  nil))
              (qingeditor/cls/set-property pkg :location location))))
        (when ownerp
          ;; warn about multiple owners
          (when (and (qingeditor/cls/get-owners pkg)
                     (not (memq pkg-name (qingeditor/cls/get-owners pkg))))
            (qingeditor/cls/warning
             module
             (format (concat "More than one init function found for "
                             "package %S. previous owner was %S, "
                             "replacing it with module %S.")
                     pkg-name (car (qingeditor/cls/get-owners pkg))
                     (qingeditor/cls/get-name module))))
          ;; last owner wins over the previous one
          (object-add-to-list pkg :owners module))
        ;; check consistency between package and defined init functions
        (unless (or ownerp
                    (eq 'config (qingeditor/cls/get-from-source pkg))
                    has-pre-init
                    has-post-init
                    (oref pkg :excluded))
          (qingeditor/cls/warning
           module
           (format (concat "package %s not initialized in module %s"
                           "package %S. previous owner was %S, "
                           "replacing it with module %S.")
                   pkg-name (car (qingeditor/cls/get-owners pkg))
                   (qingeditor/cls/get-name module))))
        ;; check if toggle can be applied
        (when (and (not ownerp)
                   (and (not (eq 'unspecified toggle))
                        toggle))
          (qingeditor/cls/warning
           module
           (format (concat "Ignoring :toggle for package %s because "
                           "module %S does not own it."
                           pkg-name
                           (qingeditor/cls/get-name module)))))
        (when has-pre-init
          (object-add-to-list pkg :pre-init-modules (qingeditor/cls/get-name module)))
        (when has-post-init
          (object-add-to-list pkg :post-init-modules (qingeditor/cls/get-name module)))
        ;; add to used package hash table
        (qingeditor/cls/set (oref modulemgr :used-packages) pkg-name pkg)))))

(provide 'qingeditor-modulemgr-resolve-listener)
