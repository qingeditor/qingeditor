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
    :type (satisfies (lambda (x) (or (null x) (stringp x))))
    :documentation "Absolute path to the module directory.")

   (packages
    :initarg :packages
    :initform :nil
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
    :documentation "If non-nil then the module needs to be installed later.")
   )
  :documentation "`qingeditor' configuration module."
  )

(provide 'qingeditor-modulemgr-module)
