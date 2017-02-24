;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define package class
;; we use `qingeditor/modulemgr/package' to describe the `emacs' package

(defclass qingeditor/modulemgr/package ()
  ((name
    :initarg :name
    :type symbol
    :reader qingeditor/cls/get-name
    :documentation "Name of the package.")

   (min-version
    :initarg :min-version
    :initform nil
    :type list
    :reader qingeditor/cls/get-min-version
    :documentation "Minimum version to install as version list.")

   (owners
    :initarg :owners
    :initform nil
    :type list
    :reader qingeditor/cls/get-owners
    :documentation "The modules which the package belong to.")

   (location
    :initarg :location
    :initform elpa
    :reader qingeditor/cls/get-location
    :type (satisfies (lambda (x)
                       (or (stringp x)
                           (memq x '(built-in local site elpa))
                           (and (listp x) (eq 'recipe (car x))))))
    :documentation "Location of the package.")

   (toggle
    :initarg :toggle
    :initform t
    :type (satisfies (lambda (x) (or (symbolp x) (listp x))))
    :documentation "Packge is enable/installed if toggle evaluates to non-nil.")

   (stage
    :initarg :stage
    :initform nil
    :reader qingeditor/cls/get-stage
    :type (satisfies (lambda (x) (member x '(nil bootstrap pre))))
    :documentation "Package initialization stage.")

   (lazy-install
    :initarg :lazy-install
    :initform nil
    :type boolean
    :documentation "If non-nil then the package needs to be installed.")

   (protected
    :initarg :protected
    :initform nil
    :type boolean
    :documentation "If non-nil then this package cannot be excluded.")

   (excluded
    :initarg :excluded
    :initform nil
    :type boolean
    :documentation "If non-nil this package is excluded from all modules.")

   (property-readonly
    :initarg :property-readonly
    :initform nil
    :type boolean
    :documentation "If this property is `t', then the properties of `package'
cannot being changed.")

   (installed
    :initarg :installed
    :initform nil
    :type boolean
    :reader qingeditor/cls/get-installed
    :documentation "Whether current package is installed.")

   (from-source
    :initarg :from-source
    :initform module
    :reader qingeditor/cls/get-from-source
    :type (satisfies (lambda (x) (or (eq x 'module) (eq x 'config))))
    :documentation "The location of the package spec defined.
 support `module' or `config'.")

   (pre-init-modules
    :initarg :pre-init-modules
    :initform '()
    :type list
    :reader qingeditor/cls/get-pre-init-modules
    :documentation "The list of modules with a pre init method.")

   (post-init-modules
    :initarg :post-init-modules
    :initform '()
    :type list
    :reader qingeditor/cls/get-post-init-modules
    :documentation "The list of modules with a post init method.")

   (modulemgr
    :initarg :modulemgr
    :initform nil
    :type (satisfies (lambda (x)
                       (or (null x)
                           (object-of-class-p x qingeditor/modulemgr/mgr))))
    :reader qingeditor/cls/get-modulemgr
    :writer qingeditor/cls/set-modulemgr
    :documentation "The module manager reference.")
   )
  :documentation "The `qingeditor' package description class.")

(defmethod qingeditor/cls/enabledp
  ((this qingeditor/modulemgr/package) &optional inhibit-messages)
  "Check the package is enabled or not."
  (let ((message-log-max (unless inhibit-messages message-log-max))
        (toggle (oref this :toggle)))
    ;; maybe expression here
    (eval toggle)))

(defmethod qingeditor/cls/set-property
  ((this qingeditor/modulemgr/package) slot value)
  "Set `slot' to the given `value' for the `package'.
If `property-readonly' of the `qingeditor/modulemgr/packge' is `t', the value will not been
changed."
  (unless (oref this :property-readonly)
    (eval `(oset this ,slot value))))

(defmethod qingeditor/cls/get-safe-owner ((this qingeditor/modulemgr/package))
  "Safe method to return the name of the `module' which owns `package'."
  ;; The owner of a package is the first *used* module in `:owners' slot.
  ;; Note: for packages in `used-packages' the owner is
  ;; always the car of the `:owners' slot.
  (let ((modules (oref this :owners))
        (modulemgr (oref this :modulemgr)))
    (while (and (consp modules)
                (not (qingeditor/cls/module-usedp modulemgr (car modules))))
      (pop modules))
    (when (qingeditor/cls/module-usedp modulemgr (car layers))
      (car modules))))

(provide 'qingeditor-modulemgr-package)
