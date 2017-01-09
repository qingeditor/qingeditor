;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;; 
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; Event接口定义

(defclass qingeditor/event-interface ()
  :abstract
  :documentation "eventmgr模块的event接口定义。")

(defmethod get-name ((this qingeditor/event-interface))
  "获取事件的名称。")

(defmethod get-target ((this qingeditor/event-interface))
  "获取这个事件产生的上下文对象或者目标对象")

(defmethod get-params ((this qingeditor/event-interface))
  "获取事件对象的参数信息。")

(defmethod get-param ((this qingeditor/event-interface) name &optional default-value)
  "获取事件对象名称为`name'参数，如果不存在就返回`default'参数指定的值。")

(defmethod set-name ((this qingeditor/event-interface))
  "设置当前事件对象的名称。")

(defmethod set-target ((this qingeditor/event-interface) target)
  "设置当前事件对象的目标对象或者上下文信息为`target'参数指定的值。")

(defmethod set-params ((this qingeditor/event-interface) params)
  "批量设置事件对象的参数为`params'。")

(defmethod set-param ((this qingeditor/event-interface) name value)
  "给当前的事件对象添加一个参数。")

(defmethod stop-propagation ((this qingeditor/event-interface) &optional (flag t))
  "设置当前的事件对象是否停止冒泡。")

(defmethod propagation-is-stopped ((this qingeditor/event-interface))
  "当前的事件对象是否停止冒泡。")

(provide 'qingeditor-event-interface)
