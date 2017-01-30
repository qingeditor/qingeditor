;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; 测试共享事件管理器

(require 'ert)
(load-file (expand-file-name (concat user-emacs-directory "/tests/env-init.el")))

(require 'qingeditor-eventmgr-shared-mgr)

(defun qingeditor/test/eventmgr/prepare-shared-mgr (test-func)
  "为测试准备一个`qingeditor/eventmgr/shared-mgr`对象'。'"
  (let ((mgr (qingeditor/eventmgr/shared-mgr)))
    (funcall test-func)))

(ert-deftest qingeditor/test/eventmgr/shared-mgr-attach-test ()
  :tags '(qingeditor/eventmgr/shared-mgr/attach)
  (qingeditor/test/eventmgr/prepare-shared-mgr
   (lambda ()
     (qingeditor/eventmgr/shared-mgr/attach
      mgr "identifier" "event" (lambda () (message "some string")))
     (should (equalp (qingeditor/hash-table/count (oref mgr :identifiers)) 1))
     (should (equalp (qingeditor/hash-table/has-key (oref mgr :identifiers) "identifier") t))
     (let (identifier-table
	   event-table
	   listener-list)
       (setq identifier-table
	     (qingeditor/hash-table/get (oref mgr :identifiers) "identifier"))
       (should (equalp (qingeditor/hash-table/count identifier-table) 1))
       (should (equalp (qingeditor/hash-table/has-key identifier-table "event") t))
       (setq event-table (qingeditor/hash-table/get identifier-table "event"))
       (should (equalp (qingeditor/hash-table/has-key event-table "1.0") t))
       (setq listener-list (qingeditor/hash-table/get event-table "1.0"))
       (should (equalp (length listener-list) 1))
       (qingeditor/eventmgr/shared-mgr/attach
	mgr "identifier" "event" (lambda () (message "some string")))
       (setq listener-list (qingeditor/hash-table/get event-table "1.0"))
       (should (equalp (length listener-list) 2))
       (qingeditor/eventmgr/shared-mgr/attach
	mgr "identifier" "event" (lambda () (message "some string")) 2)
       (should (equalp (qingeditor/hash-table/count event-table) 2))
       (setq listener-list (qingeditor/hash-table/get event-table "2.0"))
       (qingeditor/eventmgr/shared-mgr/attach
	mgr "identifier1" "event" (lambda () (message "some string")))
       (should (equalp (qingeditor/hash-table/count (oref mgr :identifiers)) 2))
       (should-error
	(qingeditor/eventmgr/shared-mgr/attach
	 mgr 1 "event" (lambda () (message "some string")))
	:type 'error)))))

(ert-deftest qingeditor/test/eventmgr/shared-mgr-detach-test ()
  :tags '(qingeditor/eventmgr/shared-mgr/detach)
  (qingeditor/test/eventmgr/prepare-shared-mgr
   (lambda ()
     (let ((listener (lambda () (message "i am lambda")))
	   (listener1 (lambda () (message "other func")))
	   identifier-table
	   event-table
	   listener-list)
       (qingeditor/eventmgr/shared-mgr/attach mgr "identifier" "event1" listener)
       (qingeditor/eventmgr/shared-mgr/attach mgr "identifier" "event2" listener)
       (qingeditor/eventmgr/shared-mgr/attach mgr "identifier1" "event1" listener)
       (qingeditor/eventmgr/shared-mgr/attach mgr "identifier1" "event2" listener)
       (qingeditor/eventmgr/shared-mgr/attach mgr "identifier1" "event2" listener1)
       (qingeditor/eventmgr/shared-mgr/attach mgr "identifier1" "event3" listener1)
       (qingeditor/eventmgr/shared-mgr/attach mgr "identifier1" "event3" listener 2)
       (qingeditor/eventmgr/shared-mgr/attach mgr "identifier1" "event4" listener)
       (qingeditor/eventmgr/shared-mgr/attach mgr "identifier1" "event1" listener 3)
       (qingeditor/eventmgr/shared-mgr/attach mgr "identifier2" "event1" listener 2)
       (qingeditor/eventmgr/shared-mgr/attach mgr "identifier2" "event1" listener1 2)
       (qingeditor/eventmgr/shared-mgr/attach mgr "identifier3" "event1" listener1 2)
       (qingeditor/eventmgr/shared-mgr/attach mgr "identifier4" "event1" listener1 2)
       (qingeditor/eventmgr/shared-mgr/attach mgr "identifier5" "event2" listener1 2)
       ;; 测试删除
       ;; 抛出错误测试
       (should-error
	(qingeditor/eventmgr/shared-mgr/detach mgr listener 1)
	:type 'error)
       (should-error
	(qingeditor/eventmgr/shared-mgr/detach mgr listener "identifier" 1)
	:type 'error)
       (qingeditor/eventmgr/shared-mgr/detach mgr listener "identifier" "event")
       (setq identifier-table (qingeditor/hash-table/get (oref mgr :identifiers) "identifier"))
       (should (eq (qingeditor/hash-table/count identifier-table) 2))
       (setq event-table (qingeditor/hash-table/get identifier-table "event1"))
       (should (eq (qingeditor/hash-table/count event-table) 1))
       (qingeditor/eventmgr/shared-mgr/detach mgr listener "identifier" "event1" listener)
       (should (eq (qingeditor/hash-table/has-key identifier-table "event1") nil))
       (qingeditor/eventmgr/shared-mgr/detach mgr listener "identifier" "event1" listener)
       (qingeditor/eventmgr/shared-mgr/detach mgr listener "identifier" "event2" listener)
       (should (eq (qingeditor/hash-table/has-key identifier-table "event2") nil))
       (should (eq (qingeditor/hash-table/has-key (oref mgr :identifiers) "identifier") nil))
       (should (eq (qingeditor/hash-table/empty (oref mgr :identifiers)) nil))
       (setq identifier-table (qingeditor/hash-table/get (oref mgr :identifiers) "identifier1"))
       (setq event-table (qingeditor/hash-table/get identifier-table "event1"))
       (setq listener-list (qingeditor/hash-table/get event-table "1.0"))
       (should (eq (length listener-list) 1))
       ;; 测试批量删除
       (qingeditor/eventmgr/shared-mgr/detach mgr listener "identifier1" "*")
       (should (eq (qingeditor/hash-table/count event-table) 0))
       (should (eq (qingeditor/hash-table/count identifier-table) 2))
       (should (eq (qingeditor/hash-table/has-key identifier-table "event4") nil))
       (should (eq (qingeditor/hash-table/has-key identifier-table "event2") t))
       (should (eq (qingeditor/hash-table/has-key identifier-table "event3") t))
       (qingeditor/eventmgr/shared-mgr/detach mgr listener1 "*" "event1")
       (should (eq (qingeditor/hash-table/has-key (oref mgr :identifiers) "identifier2") t))
       (should (eq (qingeditor/hash-table/has-key (oref mgr :identifiers) "identifier3") nil))
       (should (eq (qingeditor/hash-table/has-key (oref mgr :identifiers) "identifier4") nil))
       (should (eq (qingeditor/hash-table/has-key (oref mgr :identifiers) "identifier5") t))
       (should (eq (qingeditor/hash-table/count (oref mgr :identifiers)) 3))))))

(ert-deftest qingeditor/test/eventmgr/shared-mgr-get-listeners-test ()
  :tags '(qingeditor/eventmgr/shared-mgr/get-listeners)
  (qingeditor/test/eventmgr/prepare-shared-mgr
    (lambda ()
      (let ((listener (lambda () (message "i am lambda")))
	    (listener1 (lambda () (message "other func")))
	    identifier-table
	    event-table
	    listener-table
	    listener-list)
	(qingeditor/eventmgr/shared-mgr/attach mgr "identifier" "event1" listener)
	(qingeditor/eventmgr/shared-mgr/attach mgr "identifier" "event1" listener1)
	(qingeditor/eventmgr/shared-mgr/attach mgr "identifier" "event1" listener 2)
	(qingeditor/eventmgr/shared-mgr/attach mgr "identifier" "event1" listener1 2)
	(qingeditor/eventmgr/shared-mgr/attach mgr "identifier1" "event1" listener)
	(qingeditor/eventmgr/shared-mgr/attach mgr "identifier1" "*" listener)
	(qingeditor/eventmgr/shared-mgr/attach mgr "identifier1" "*" listener 2)
	(should-error
	 (qingeditor/eventmgr/shared-mgr/get-listeners mgr "identifier" nil)
	 :type 'error)
      	(should-error
	 (qingeditor/eventmgr/shared-mgr/get-listeners mgr "identifier" "*")
	 :type 'error)
	(should-error
	 (qingeditor/eventmgr/shared-mgr/get-listeners mgr "identifier" "")
	 :type 'error)
	(setq listener-table
	      (qingeditor/eventmgr/shared-mgr/get-listeners mgr "identifier" "event1"))
	(should (eq (qingeditor/hash-table/count listener-table) 2))
	(should (eq (qingeditor/hash-table/has-key listener-table "1.0") t))
	(should (eq (qingeditor/hash-table/has-key listener-table "2.0") t))
	(setq listener-list (qingeditor/hash-table/get listener-table "1.0"))
	(should (equalp listener-list (list listener1 listener)))
	(setq listener-table
	      (qingeditor/eventmgr/shared-mgr/get-listeners mgr "identifier" "event1"))
	(setq listener-list (qingeditor/hash-table/get listener-table "2.0"))
	(should (equalp listener-list (list listener1 listener)))
	;; 测试 `* identifier' 
	(qingeditor/eventmgr/shared-mgr/attach mgr "*" "event1" listener1 2)
	(setq listener-table
	      (qingeditor/eventmgr/shared-mgr/get-listeners mgr "identifier" "event1"))
	(setq listener-list (qingeditor/hash-table/get listener-table "1.0"))
	(should (equalp listener-list (list listener1 listener)))
	(setq listener-list (qingeditor/hash-table/get listener-table "2.0"))
	(should (equalp listener-list (list listener1 listener listener1)))
	(should-error
	 (qingeditor/eventmgr/shared-mgr/get-listeners mgr "*" "event1")
	 :type 'error)
	(setq listener-table
	      (qingeditor/eventmgr/shared-mgr/get-listeners mgr "identifier1" "event1"))
	(should (eq (qingeditor/hash-table/count listener-table) 2))
	(should (eq (qingeditor/hash-table/has-key listener-table "1.0") t))
	(should (eq (qingeditor/hash-table/has-key listener-table "2.0") t))
	(setq listener-list (qingeditor/hash-table/get listener-table "2.0"))
	(should (equalp listener-list (list listener listener1)))
	(setq listener-list (qingeditor/hash-table/get listener-table "1.0"))
	(should (equalp listener-list (list listener listener)))))))

(ert-deftest qingeditor/test/eventmgr/shared-mgr-get-listeners-test ()
  :tags '(qingeditor/eventmgr/shared-mgr/get-listeners)
  (qingeditor/test/eventmgr/prepare-shared-mgr
   (lambda ()
     (let ((listener (lambda () (message "i am lambda")))
	    (listener1 (lambda () (message "other func")))
	    identifier-table
	    event-table
	    listener-table
	    listener-list)
       	(qingeditor/eventmgr/shared-mgr/attach mgr "identifier" "event1" listener)
	(qingeditor/eventmgr/shared-mgr/attach mgr "identifier" "event2" listener1)
	(qingeditor/eventmgr/shared-mgr/attach mgr "identifier1" "event1" listener 2)
	(qingeditor/eventmgr/shared-mgr/attach mgr "identifier1" "event2" listener1 2)
	(qingeditor/eventmgr/shared-mgr/clear-listeners mgr "identifier3")
	(should (equal (qingeditor/hash-table/has-key (oref mgr :identifiers) "identifier") t))
	(qingeditor/eventmgr/shared-mgr/clear-listeners mgr "identifier1" "event1")
	(setq event-table
	      (qingeditor/hash-table/get (oref mgr :identifiers) "identifier1"))
	(should (equal (qingeditor/hash-table/has-key event-table "event2") t))
	(should (equal (qingeditor/hash-table/has-key event-table "event1") nil))
	(qingeditor/eventmgr/shared-mgr/clear-listeners mgr "identifier")
	(should (equal (qingeditor/hash-table/has-key (oref mgr :identifiers) "identifier") nil))
	(qingeditor/eventmgr/shared-mgr/clear-listeners mgr "identifier1")
	(should (equal (qingeditor/hash-table/count (oref mgr :identifiers)) 0))))))
