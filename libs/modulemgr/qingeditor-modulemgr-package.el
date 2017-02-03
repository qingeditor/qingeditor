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

(defclass qingeditor/modulemgr/packge ()
  ((name
    :initarg :name
    :type symbol
    :documentation "Name of the package.")

   (min-version
    :initarg :min-version
    :initform nil
    :type list
    :documentation "Minimum version to install as version list.")

   (owners
    :initarg :owners
    :initform nil
    :type list
    :documentation "The modules which the package belong to.")

   (location
    :initarg :location
    :initform elpa
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

   (init-stage
    :initarg :init-stage
    :initform nil
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
cannot being changed."))
  :documentation "The `qingeditor' package description class.")

(defmethod qingeditor/cls/enabled ((this qingeditor/modulemgr/packge) &optional inhibit-messages)
  "Check the package is enabled or not."
  (let ((message-log-max (unless inhibit-messages message-log-max))
        (toggle (oref pkg :toggle)))
    ;; maybe expression here
    (eval toggle)))

(defmethod qingeditor/cls/set-property ((this qingeditor/modulemgr/packge) slot value)
  "Set `slot' to the given `value' for the package `pkg'.
If `property-readonly' of the `qingeditor/modulemgr/packge' is `t', the value will not been
changed."
  (unless (oref this :property-readonly)
    (eval `(oset pkg ,slot value))))

(provide 'qingeditor-modulemgr-package)
