(require 'ert)
(load-file (expand-file-name (concat user-emacs-directory "/tests/env-init.el")))

(require 'qingeditor-eventmgr-event-handler)

(require 'eieio)

(defclass event-handler-cls ()
  ((data
    :initarg :data
    :initform nil
    :type list)))

(defmethod event-handler-cls/get-name ((this event-handler-cls) name age)
  (list (car (oref this :data)) name age))

(ert-deftest qingeditor/test/eventmgr/event-collection-test ()
  (let (handler
	(l (lambda (name) (message "i am %s" name)))
	handler-ret
	(cls-hander (event-handler-cls :data '("softboy" "beijing"))) )
    (setq handler (qingeditor/eventmgr/event-handler/init
    		   l))
    (should (= 1 (oref handler :type)))
    (should (eq l (car (oref handler :callable-data))))
    (setq handler (qingeditor/eventmgr/event-handler/init
    		   (lambda (name)
    		     `("hello" ,name))))
    (setq handler-ret (qingeditor/eventmgr/event-handler/call handler "softboy"))
    (should (equal handler-ret '("hello" "softboy")))
    (setq handler (qingeditor/eventmgr/event-handler/init
		   (list #'event-handler-cls/get-name cls-hander)
		   qingeditor/eventmgr/method-callable))
    (setq handler-ret (qingeditor/eventmgr/event-handler/call handler "qs" 12))
    (should (equal handler-ret '("softboy" "qs" 12)))))
