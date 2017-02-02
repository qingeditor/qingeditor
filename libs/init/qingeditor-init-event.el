;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define initialization event class

(require 'qingeditor-eventmgr-event)

(defconst qingeditor/init/event/editor-cfg-ready-event "editor-cfg-ready-event"
  "when `qingeditor' load configuration file, dispatch this event.")

(defconst qingeditor/init/event/bootstrap-event "bootstrap-event"
  "after load configuration, dispatch bootstrap event.")

(defconst qingeditor/init/event/before-load-modules-event "before-load-modules-event"
  "before `qingeditor' load modules, dispatch the event.")

(defconst qingeditor/init/event/adter-load-modules-event "adter-load-modules-event"
  "when all configured modules loaded, dispatch this event.")

(defconst qingeditor/init/event/render-event-event "render-event-event"
  "after loaded target modules, `qingeditor' render starup screen. but
if you invoke `qingeditor' from terminal with a file path, this event will not
been dispatched.")

(defconst qingeditor/init/event/ready-event "ready-event"
  "after all step of process, `qingeditor' is ready for used, dispatch this event.")

(defconst qingeditor/init/event/error-event "error-event"
  "when `qingeditor' catch some error, will dispatch this event. The error event listener
handler will process the error information.")

(defclass qingeditor/init/event (qingeditor/eventmgr/event)
  ((initializer
    :initarg :initializer
    :initform nil
    :reader qingeditor/init/event/get-initializer
    :documentation "global initializer object reference.")

   (result
    :initarg :result
    :initform nil
    :reader qingeditor/init/event/get-result
    :documentation "The result of previous event handler."))
  :documentation "The event class for initialization stage")

(defun qingeditor/init/event/init (&optional name target params)
  "初始化事件对象。"
  (let ((event (qingeditor/init/event :name name :target target)))
    (when params
      (qingeditor/eventmgr/event/set-params-from-alist event params))
    event))

(defmethod qingeditor/init/event/set-initializer ((this qingeditor/init/event) initializer)
  "Set initializer for event object."
  (qingeditor/eventmgr/event/set-param this "initializer" initializer)
  (oset this :initializer initializer)
  this)

(defmethod qingeditor/init/event/set-result ((this qingeditor/init/event) result)
  "Set the result of current handler."
  (qingeditor/eventmgr/event/set-param this "__RESULT__" result)
  (oset this :result result))

(defmethod qingeditor/init/event/is-error ((this qingeditor/init/event))
  "Get the error state of event object."
  (not (eq (qingeditor/eventmgr/event/get-param this "error" nil) nil)))

(defmethod qingeditor/init/event/set-error ((this qingeditor/init/event) message)
  "Set error message of event object."
  (qingeditor/eventmgr/event/set-param this "error" message)
  this)

(defmethod qingeditor/init/event/get-error ((this qingeditor/init/event))
  "Get error message."
  (qingeditor/eventmgr/event/get-param this "error" ""))

(provide 'qingeditor-init-event)
