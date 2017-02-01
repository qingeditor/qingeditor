;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; 封装一个事件对象

(require 'qingeditor-hash-table)

(defclass qingeditor/eventmgr/event ()
  ((name
    :initarg :name
    :initform nil
    :type (satisfies (lambda (x)
		       (or (null x) (stringp x))))
    :reader qingeditor/eventmgr/event/get-name
    :writer qingeditor/eventmgr/event/set-name
    :documentaion "事件的名称。")

   (target
    :initarg :target
    :initform nil
    :type (satisfies
	   (lambda (x)
	     (or (null x)
		 (stringp x)
		 (object-of-class-p x eieio-default-superclass))))
    :reader qingeditor/eventmgr/event/get-target
    :writer qingeditor/eventmgr/event/set-target
    :documentaion "当前的事件目标对象。")

   (params
    :initarg :params
    :initform (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "设置事件的参数。")

   (stop-propagation
    :initarg :stop-propagation
    :initform nil
    :type boolean
    :reader qingeditor/eventmgr/event/get-stop-propagation
    :writer qingeditor/eventmgr/event/set-stop-propagation
    :documentation "是否停止事件传播。")
   )
  :documentation "封装一个目标的上下文和事件参数传递的类，这个类会定义一些与
事件管理器交互的方法。")

(defun qingeditor/eventmgr/event/init (&optional name target params)
  "初始化事件对象。"
  (let ((event (qingeditor/eventmgr/event :name name :target target)))
    (when params
      (qingeditor/eventmgr/event/set-params-from-alist event params))
    event))

(defmethod qingeditor/eventmgr/event/set-params-from-alist
  ((this qingeditor/eventmgr/event) alist)
  "给当前的事件设置参数。"
  (qingeditor/hash-table/set-from-alist (oref this :params) alist)
  this)

(defmethod qingeditor/eventmgr/event/set-param
  ((this qingeditor/eventmgr/event) key value)
  "给当前的事件对象设置参数。"
  (qingeditor/hash-table/set (oref this :params) key value)
  this)

(defmethod qingeditor/eventmgr/event/get-params
  ((this qingeditor/eventmgr/event))
  "获取当前事件对象的参数。"
  (oref this :params))

(defmethod qingeditor/eventmgr/event/get-param
  ((this qingeditor/eventmgr/event) name &optional default)
  "获取指定的参数不存在的话返回默认值。"
  (qingeditor/hash-table/get (oref this :params) name default))

(defmethod qingeditor/eventmgr/event/clear-params
  ((this qingeditor/eventmgr/event))
  "删除事件对象的所有的参数。"
  (qingeditor/hash-table/clear (oref this :params)))

(provide 'qingeditor-eventmgr-event)
