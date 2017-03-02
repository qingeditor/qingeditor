;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define module class
;; we use `qingeditor/modulemgr/module' to describe the `emacs' package module

(defclass qingeditor/modulemgr/module ()
  ((name
    :initarg :name
    :initform symbol
    :reader qingeditor/cls/get-name
    :writer qingeditor/cls/set-name
    :documentation "The name of the module.")

   (dir
    :initarg :dir
    :initform nil
    :type (satisfies (lambda (dir) (or (null dir) (stringp dir))))
    :reader qingeditor/cls/get-module-dir
    :writer qingeditor/cls/set-module-dir
    :documentation "Absolute path to the module directory.")

   (require-package-specs
    :initarg :require-package-specs
    :initform nil
    :type list
    :reader qingeditor/cls/get-require-package-specs
    :documentation "List of package specs required by this module.")

   (selected-packages
    :initarg :selected-packages
    :initform 'all
    :type (satisfies (lambda (x) (or (and (symbolp x) (eq 'all x))
                                     (listp x))))
    :documentation "List of selected package symbols.")

   (variables
    :initarg :variables
    :initform nil
    :type list
    :documentation "A list of variable-value pairs.")

   (disabled
    :initarg :disabled-for
    :initform nil
    :type list
    :documentation "A list of modules where this module is disabled.")

   (enabled
    :initarg :enable-for
    :initform 'unspecified
    :type (satisfies (lambda (x) (or (listp x) (eq 'unspecified x))))
    :documentation (concat "A list of modules where this module is enabled."
                           "(Takes precedence over `:disabled-for'.)"))

   (lazy-install
    :initarg :lazy-install
    :initform nil
    :type boolean
    :documentation "If non-nil then the module needs to be installed later.")

   (package-init-list
    :initarg :package-init-list
    :initform nil
    :type list
    :documentation "Save the package list information."))
  :documentation "`qingeditor' configuration module.")

(defmethod qingeditor/cls/init ((this qingeditor/modulemgr/module))
  "The init method that will invoked after instance finished construct."
  (oset this :require-package-specs
        (qingeditor/cls/define-package-specs this))
  (oset this :package-init-list
        (qingeditor/cls/get-package-init-list this))
  (qingeditor/cls/module-init this))

(defmethod qingeditor/cls/get-local-dir ((this qingeditor/modulemgr/module))
  "Return local dir of current module."
  (concat (oref this :dir) "local/"))

(defmethod qingeditor/cls/module-init ((this qingeditor/modulemgr/module))
  "You can override this method to provide user init procedure."
  nil)

(defmethod qingeditor/cls/define-package-specs ((this qingeditor/modulemgr/module))
  "This method define the package specs that module required."
  nil)

(defmethod qingeditor/cls/provide-keymap-defs ((this qingeditor/modulemgr/module))
  "Is this module provide a key map setting script. default does not provide."
  nil)

(defmethod qingeditor/cls/provide-extra-func-definitions ((this qingeditor/modulemgr/module))
  "Is this module provide a extra function definitions."
  nil)

(defmethod qingeditor/cls/provide-extra-module-config ((this qingeditor/modulemgr/module))
  "Is this module provide a extra config settings, if return `t',
Module manager will load `config.el' in current module directory. You can set some default
variable for this module. default return `nil'"
  nil)

(defmethod qingeditor/cls/has-init-for-package ((this qingeditor/modulemgr/module) package-name)
  "check wether has a init method for `package-name'."
  (memq package-name (oref this :package-init-list)))

(defmethod qingeditor/cls/get-package-init-list ((this qingeditor/modulemgr/module))
  "This will be invoked when we need to know wether a package has a module initializer,
default will return a list of package name that doesn't has a :has-init nil in spec."
  (let ((specs (qingeditor/cls/get-require-package-specs this))
        (ret nil))
    (dolist (spec specs)
      (if (not (listp spec))
          (push spec ret)
        (let ((has-init t))
          (setq has-init (or (not (plist-member (cdr spec) :has-init))
                             (plist-get (cdr spec) :has-init)))
          (when has-init
            (push (car spec) ret)))))
    ret))

(defmethod qingeditor/cls/get-package-pre-init-list ((this qingeditor/modulemgr/module))
  "Return a list of package names that has a pre init method.
default return `nil'."
  nil)

(defmethod qingeditor/cls/get-package-post-init-list ((this qingeditor/modulemgr/module))
  "Return a list of package names that has a post init method.
default return `nil'."
  nil)

(provide 'qingeditor-modulemgr-module)
