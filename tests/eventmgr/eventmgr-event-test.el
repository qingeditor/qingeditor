;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; 测试数据结构包中的`fast-priority-queue'

(require 'ert)
(load-file (expand-file-name (concat user-emacs-directory "/tests/env-init.el")))

(require 'qingeditor-eventmgr-event)

(defun qingeditor/test/eventmgr/prepare-event (test-func)
  "为测试准备一个`qingeditor/eventmgr/event`对象'。'"
  (let ((event (qingeditor/eventmgr/event/init)))
    (funcall test-func)))


(ert-deftest qingeditor/test/eventmgr/event/constructor-test ()
  :tags '(qingeditor/eventmgr/event/constructor)
  (qingeditor/test/eventmgr/prepare-event
   (lambda ()
    (let ((new-event (qingeditor/eventmgr/event/init
		      "application"
		      "event-target"
		      '(('name "softboy")
			('age 12)))))
      (should (equalp (oref new-event :name) "application"))
      (should (equalp (oref new-event :target) "event-target"))
      (should (equalp (qingeditor/eventmgr/event/get-stop-propagation event) nil))
      (qingeditor/eventmgr/event/set-stop-propagation event t)
      (should (equalp (qingeditor/eventmgr/event/set-stop-propagation event t) t))))))

(ert-deftest qingeditor/test/eventmgr/event/params-test ()
  :tags '(qingeditor/eventmgr/event/params)
  "测试参数获取，设置相关的方法。"
  (qingeditor/test/eventmgr/prepare-event
   (lambda ()
     (qingeditor/eventmgr/event/clear-params event)
     (should (equalp (qingeditor/hash-table/count (oref event :params)) 0))
     (qingeditor/eventmgr/event/set-param event 'name "softboy")
     (should (equalp (qingeditor/hash-table/count (oref event :params)) 1))
     (should (equalp (qingeditor/eventmgr/event/get-param event 'name) "softboy"))
     (should (equalp (qingeditor/eventmgr/event/get-param event 'not-exist "beijing") "beijing"))
     (qingeditor/eventmgr/event/clear-params event)
     (qingeditor/eventmgr/event/set-params-from-alist
      event '((name . "softboy")
	      (age . 12)
	      (address . "beijing")))
     (should (equalp (qingeditor/hash-table/count (oref event :params)) 3))
     (should (equalp (qingeditor/eventmgr/event/get-param event 'name) "softboy"))
     (should (equalp (qingeditor/eventmgr/event/get-param event 'age) 12))
     (should (equalp (qingeditor/eventmgr/event/get-param event 'address) "beijing")))))
