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

(defclass qingeditor/init/event (qingeditor/eventmgr/event)
  ((initializer
    :initarg :initializer
    :initform nil
    :type qingeditor/initializer
    :reader qingeditor/init/event/get-initializer
    :documentation "global initializer object reference.")

   (result
    :initarg :result
    :initform nil
    :reader qingeditor/init/event/get-result
    :documentation "The result of previous event handler."))
  :documentation "The event class for initialization stage")

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
