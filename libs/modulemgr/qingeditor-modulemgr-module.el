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
    :documentation "The name of the layer.")

   (dir
    :initarg :dir
    :initform nil
    :type (satisfies (lambda (dir) (or (null dir) (stringp dir))))
    :documentation "Absolute path to the module directory.")

   (packages
    :initarg :packages
    :initform nil
    :type list
    :documentation "List of package symbols declared in this module.")

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

   (lazy-install
    :initarg :lazy-install
    :initform nil
    :type boolean
    :documentation "If non-nil then the module needs to be installed later."))
  :documentation "`qingeditor' configuration module.")

(defmethod qingeditor/cls/get-require-packages ((this qingeditor/modulemgr/module))
  "Return a list of packages that the module requires."
  '())

(defmethod qingeditor/cls/resovle-dependence ((this qingeditor/modulemgr/module))
  "When the module depend on some others modules, you can resove in this method."
  t)

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
