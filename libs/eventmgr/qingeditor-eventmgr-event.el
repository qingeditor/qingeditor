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
    :reader qingeditor/cls/get-name
    :writer qingeditor/cls/set-name
    :documentaion "事件的名称。")

   (target
    :initarg :target
    :initform nil
    :reader qingeditor/cls/get-target
    :writer qingeditor/cls/set-target
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
    :reader qingeditor/cls/get-stop-propagation
    :writer qingeditor/cls/set-stop-propagation
    :documentation "是否停止事件传播。")
   )
  :documentation "封装一个目标的上下文和事件参数传递的类，这个类会定义一些与
事件管理器交互的方法。")

(defun qingeditor/eventmgr/event/init (name target params)
  (let ((event (qingeditor/eventmgr/event :name name :target target)))
    (when params
      (qingeditor/cls/set-params-from-alist event params))))


(defmethod qingeditor/cls/set-params-from-alist
  ((this qingeditor/eventmgr/event) alist)
  "给当前的事件设置参数。"
  (qingeditor/cls/set-from-alist (oref this :params) alist)
  this)

(defmethod qingeditor/cls/set-param
  ((this qingeditor/eventmgr/event) key value)
  "给当前的事件对象设置参数。"
  (qingeditor/cls/set (oref this :params) key value)
  this)

(defmethod qingeditor/cls/get-params
  ((this qingeditor/eventmgr/event))
  "获取当前事件对象的参数。"
  (oref this :params))

(defmethod qingeditor/cls/get-param
  ((this qingeditor/eventmgr/event) name &optional default)
  "获取指定的参数不存在的话返回默认值。"
  (qingeditor/cls/get (oref this :params) name default))

(defmethod qingeditor/cls/clear-params
  ((this qingeditor/eventmgr/event))
  "删除事件对象的所有的参数。"
  (qingeditor/cls/clear (oref this :params)))

(provide 'qingeditor-eventmgr-event)
