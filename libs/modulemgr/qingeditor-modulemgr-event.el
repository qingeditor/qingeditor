;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The module manager event class

(require 'qingeditor-eventmgr-event)

(defconst qingeditor/modulemgr/module-detect-event "module-detect-event"
  "When module manager init, it will detect all the modules under
direcotry `qingeditor/modulemgr/module-directory'. when this stage finished,
dispatch this event.")

(defconst qingeditor/modulemgr/before-load-modules-event "before-load-modules-event"
  "Before module manager begin load modules, dispatch this event.")

(defconst qingeditor/modulemgr/after-load-modules-event "after-load-modules-event"
  "After module manager finished load all used modules, dispatch this event.")

(defconst qingeditor/modulemgr/before-load-module-cycle-event "before-load-module-cycle-event"
  "Before module manager before load target module, dispatch this event.")

(defconst qingeditor/modulemgr/load-module-cycle-event "load-module-cycle-event"
  "When module manager load target module, dispatch this event.")

(defconst qingeditor/modulemgr/after-load-module-cycle-event "after-load-module-cycle-event"
  "After module manager load target module, dispatch this event.")

(defclass qingeditor/modulemgr/event (qingeditor/eventmgr/event)
  ((module-info
    :initarg :module-info
    :initform nil
    :type list
    :reader qingeditor/cls/get-module-info
    :writer qingeditor/cls/set-module-info
    :documentation "The detected  module information."))

  (module
   :initarg :module
   :initform nil
   :reader qingeditor/cls/get-module
   :type (satisfies (lambda (obj) (or (null obj) (object-of-class-p obj qingeditor/modulemgr/module))))
   :documentation "The create module object.")
  :documentation "The module manager event class.")

(defmethod qingeditor/cls/set-module ((this qingeditor/modulemgr/event) module)
  "Set the module object for this event."
  (unless (object-of-class-p module qingeditor/modulemgr/module)
    (error "`module' arg must be instance of `qingeditor/modulemgr/module', %s given"
           (type-of module)))
  (oset this :module module))

(provide 'qingeditor-modulemgr-event)
