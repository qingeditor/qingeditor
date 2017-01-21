;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;; 
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;; 
;; 封装一个事件对象

(require 'qingeditor-event-interface)

(defclass qingeditor/event (qingeditor/event-interface)
  ((name
    :initarg :name
    :initform nil
    :type (satisfies (lambda (x) (or (null x) (stringp x))))
    :reader qingeditor/event/get-name
    :writer qingeditor/event/set-name
    :documentaion "事件的名称。")
   
   (target
    :initarg :target
    :initform nil
    :type (satisfies (lambda (x)
		       (or (null x)
			   (stringp x)
			   (object-of-class-p x eieio-default-superclass))))
    :reader qingeditor/event/get-target
    :writer qingeditor/event/set-target
    :documentaion "当前的事件目标对象。")

   
   )
  :documentation "封装一个目标的上下文和事件参数传递的类，这个类会定义一些与
事件管理器交互的方法。")

