;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;; 
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;; 
;; 封装一个事件处理器，一般事件处理器有`listener'进行维护

(defconst qingeditor/eventmgr/normal-callable 1
  "普通类型的调用，`lisp'里面的函数闭包等。")

(defconst qingeditor/eventmgr/method-callable 2
  "定义在`eieio'类里面的方法调用类型。")

(defclass qingeditor/eventmgr/event-handler ()
  ((type
    :initarg :type
    :initform (eval qingeditor/eventmgr/normal-callable)
    :type number
    :reader qingeditor/cls/get-type
    :documentation "处理器的类型")

   (callable-data
    :initarg :callable-data
    :initform nil
    :type list
    :reader qingeditor/cls/get-callable
    :documentaion "底层处理器引用，第一个元素是调用对象，如果是`qingeditor/eventmgr/method-callable'
则第二个元素是对象的引用。"))
  :documentaion "事件处理器类，事件处理器在事件监听器里面进行注册
提供事件处理过程中的实际的操作。")

(defun qingeditor/eventmgr/event-handler/init (callable &optional type)
  "初始化一个事件处理器。"
  (when (and type
	     (not (= type qingeditor/eventmgr/normal-callable))
	     (not (= type qingeditor/eventmgr/method-callable)))
    (error "type specified, but not supported, please check."))
  (when (eq (car callable) 'lambda)
    (setq callable (list callable)))
  (unless type
    (when (= (length callable) 1)
      (setq type qingeditor/eventmgr/normal-callable))
    (when (= (length callable) 2)
      (setq type qingeditor/eventmgr/method-callable)))
  (make-instance 'qingeditor/eventmgr/event-handler
                 :type type :callable-data callable))

(defmethod qingeditor/cls/call
  ((this qingeditor/eventmgr/event-handler) &rest args)
  "调用处理器。"
  (cond
   ((= qingeditor/eventmgr/normal-callable (oref this :type))
    (apply (car (oref this :callable-data)) args ))
   ((= qingeditor/eventmgr/method-callable (oref this :type))
    (apply (car (oref this :callable-data)) (cadr (oref this :callable-data)) args))))

(provide 'qingeditor-eventmgr-event-handler)
