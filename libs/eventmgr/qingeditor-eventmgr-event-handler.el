;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;; 
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The event handler class

(defconst qingeditor/eventmgr/normal-callable 1
  "Normal callable type, you can wrap `lambda' expression or
function symbol in this type of event handler object")

(defconst qingeditor/eventmgr/method-callable 2
  "`eieio' class type, you can wrap `eieio' method in event handler.")

(defclass qingeditor/eventmgr/event-handler ()
  ((type
    :initarg :type
    :initform (eval qingeditor/eventmgr/normal-callable)
    :type number
    :reader qingeditor/cls/get-type
    :documentation "The type of event handler.")

   (callable-data
    :initarg :callable-data
    :initform nil
    :type list
    :reader qingeditor/cls/get-callable
    :documentaion "the callable object reference."))
  :documentaion "The event handler class, provide actual callable object for event listener.")

(defun qingeditor/eventmgr/event-handler/init (callable &optional type)
  "init event handler object."
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
  "Invoke event handler."
  (cond
   ((= qingeditor/eventmgr/normal-callable (oref this :type))
    (apply (car (oref this :callable-data)) args ))
   ((= qingeditor/eventmgr/method-callable (oref this :type))
    (apply (car (oref this :callable-data)) (cadr (oref this :callable-data)) args))))

(provide 'qingeditor-eventmgr-event-handler)
