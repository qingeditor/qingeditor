;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The base event class

(require 'qingeditor-hash-table)

(defclass qingeditor/eventmgr/event ()
  ((name
    :initarg :name
    :initform nil
    :type (satisfies (lambda (x)
		       (or (null x) (stringp x))))
    :reader qingeditor/cls/get-name
    :writer qingeditor/cls/set-name
    :documentaion "The name of the event.")

   (target
    :initarg :target
    :initform nil
    :reader qingeditor/cls/get-target
    :writer qingeditor/cls/set-target
    :documentaion "The target of the event.")

   (params
    :initarg :params
    :initform (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "The params of the event.")

   (stop-propagation
    :initarg :stop-propagation
    :initform nil
    :type boolean
    :reader qingeditor/cls/get-stop-propagation
    :writer qingeditor/cls/set-stop-propagation
    :documentation "Is the event been stopped.")
   )
  :documentation "The base event class, encapsulate the information
during the event dispatch process.")

(defun qingeditor/eventmgr/event/init (&optional name target params)
  (let ((event (make-instance 'qingeditor/eventmgr/event)))
    (when name
      (qingeditor/cls/set-name event name))
    (when target
      (qingeditor/cls/set-target event target))
    (when params
      (qingeditor/cls/set-params-from-alist event params))
    event))

(defmethod qingeditor/cls/set-params-from-alist
  ((this qingeditor/eventmgr/event) alist)
  "Set the params of event object from `alist'."
  (qingeditor/cls/set-from-alist (oref this :params) alist)
  this)

(defmethod qingeditor/cls/set-param
  ((this qingeditor/eventmgr/event) key value)
  "Set event param use `key' and `value'."
  (qingeditor/cls/set (oref this :params) key value)
  this)

(defmethod qingeditor/cls/get-params
  ((this qingeditor/eventmgr/event))
  "Get the params of event object."
  (oref this :params))

(defmethod qingeditor/cls/get-param
  ((this qingeditor/eventmgr/event) name &optional default)
  "Get the param by `name', if `name not exist, return `default'.'"
  (qingeditor/cls/get (oref this :params) name default))

(defmethod qingeditor/cls/clear-params
  ((this qingeditor/eventmgr/event))
  "Clear the event params."
  (qingeditor/cls/clear (oref this :params)))

(provide 'qingeditor-eventmgr-event)
