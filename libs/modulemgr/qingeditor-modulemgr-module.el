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
    :documentation "The name of the layer.")

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
    :documentation "If non-nil then the module needs to be installed later."))
  :documentation "`qingeditor' configuration module.")

(defmethod qingeditor/cls/init ((this qingeditor/modulemgr/module))
  "The init method that will invoked after instance finished construct."
  (oset this :require-package-specs
        (qingeditor/cls/get-require-package-specs this)))

(defmethod qingeditor/cls/get-load-paths ((this qingeditor/modulemgr/module))
  "The return list of this method will be added into load-path."
  nil)

(defmethod qingeditor/cls/get-require-package-specs
  ((this qingeditor/modulemgr/module))
  "Get the require package specs of this module."
  nil)

(defmethod qingeditor/cls/provide-keymap-setter ((this qingeditor/modulemgr/module))
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

(provide 'qingeditor-modulemgr-module)
