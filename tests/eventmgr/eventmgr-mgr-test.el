;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; 测试事件管理器

(require 'ert)
(load-file (expand-file-name (concat user-emacs-directory "/tests/env-init.el")))

(require 'qingeditor-eventmgr-mgr)
(require 'qingeditor-eventmgr-shared-mgr)

(defun qingeditor/test/eventmgr/mgr/prepare-mgr (test-func)
  "为测试准备一个`qingeditor/eventmgr`对象'。'"
  (let ((mgr (qingeditor/eventmgr/mgr/init)))
    (funcall test-func)))

(ert-deftest qingeditor/test/eventmgr/mgr-set-identifiers-test ()
  :tags '(qingeditor/test/eventmgr/mgr/prepare-mgr)
  (qingeditor/test/eventmgr/mgr/prepare-mgr
   (lambda ()
     (qingeditor/eventmgr/mgr/set-identifiers
      mgr '("identifier1" "identifier1" "identifier2"))
     (should (equalp (oref mgr :identifiers) '("identifier1" "identifier2"))))))


(ert-deftest qingeditor/test/eventmgr/mgr-add-identifiers-test ()
  :tags '(qingeditor/test/eventmgr/mgr/prepare-mgr)
  (qingeditor/test/eventmgr/mgr/prepare-mgr
   (lambda ()
     (qingeditor/eventmgr/mgr/set-identifiers
      mgr '("identifier1"))
     (qingeditor/eventmgr/mgr/add-identifiers
      mgr '("identifier1" "identifier2" "identifier3"))
     (should (equalp (oref mgr :identifiers) '("identifier1" "identifier2" "identifier3")))
     (qingeditor/eventmgr/mgr/add-identifiers
      mgr '("identifier1" "identifier2" "identifier3"))
     (should (equalp (oref mgr :identifiers) '("identifier1" "identifier2" "identifier3"))))))

(ert-deftest qingeditor/test/eventmgr/mgr-attach-test ()
  :tags '(qingeditor/eventmgr/mgr/attach)
  (qingeditor/test/eventmgr/mgr/prepare-mgr
   (lambda ()
     (let ((listener1 (lambda ()
			(message "listener1")))
	   (listener2 (lambda ()
			(message "listener2")))
	   listener-table
	   listener-list)
       (should (eq (qingeditor/hash-table/empty (oref mgr :events)) t))
       (qingeditor/eventmgr/mgr/attach mgr "event1" listener1)
       (should (eq (qingeditor/hash-table/count (oref mgr :events)) 1))
       (qingeditor/eventmgr/mgr/attach mgr "event1" listener2)
       (should (eq (qingeditor/hash-table/count (oref mgr :events)) 1))
       (setq listener-table (qingeditor/hash-table/get (oref mgr :events) "event1"))
       (should (eq (qingeditor/hash-table/count listener-table) 1))
       (qingeditor/eventmgr/mgr/attach mgr "event1" listener1 2)
       (should (eq (qingeditor/hash-table/count listener-table) 2))
       (setq listener-list (qingeditor/hash-table/get listener-table 1))
       (should (equalp listener-list (list listener2 listener1)))
       (setq listener-list (qingeditor/hash-table/get listener-table 2))
       (setq listener-list (qingeditor/hash-table/get listener-table 1))
       (should-error (qingeditor/eventmgr/mgr/attach mgr "" listener2))
       (should-error (qingeditor/eventmgr/mgr/attach mgr 1 listener2))))))

(ert-deftest qingeditor/test/eventmgr/mgr-detach-test ()
  :tags '(qingeditor/eventmgr/mgr/detach)
  (qingeditor/test/eventmgr/mgr/prepare-mgr
   (lambda ()
     (let ((listener1 (lambda ()
			(message "listener1")))
	   (listener2 (lambda ()
			(message "listener2")))
	   listener-table
	   listener-list)
       (qingeditor/eventmgr/mgr/attach mgr "event1" listener1)
       (qingeditor/eventmgr/mgr/attach mgr "event1" listener1 2)
       (qingeditor/eventmgr/mgr/attach mgr "event1" listener2 2)
       (qingeditor/eventmgr/mgr/attach mgr "event2" listener2)
       (qingeditor/eventmgr/mgr/attach mgr "event2" listener2 2)
       (should-error (qingeditor/eventmgr/mgr/detach mgr listener1 ""))
       (should-error (qingeditor/eventmgr/mgr/detach mgr listener1 12))
       (should-error (qingeditor/eventmgr/mgr/detach mgr listener1 t))
       (setq listener-table (qingeditor/hash-table/get (oref mgr :events) "event1"))
       (should (equal (qingeditor/hash-table/count listener-table) 2))
       (should (equal (qingeditor/hash-table/has-key listener-table 2) t))
       (should (equal (qingeditor/hash-table/has-key listener-table 1) t))
       (qingeditor/eventmgr/mgr/detach mgr listener1 "event1")
       (should (equal (qingeditor/hash-table/count listener-table) 1))
       (should (equal (qingeditor/hash-table/has-key listener-table 2) t))
       (should (equal (qingeditor/hash-table/has-key listener-table 1) nil))
       (qingeditor/eventmgr/mgr/detach mgr listener2 "event2")
       (should (equal (qingeditor/hash-table/has-key (oref mgr :events) "event2") nil))))))

(ert-deftest qingeditor/test/eventmgr/mgr-get-listeners-by-event-name-test ()
  :tags '(qingeditor/eventmgr/mgr/get-listeners-by-event-name)
  (qingeditor/test/eventmgr/mgr/prepare-mgr
   (lambda ()
     (let ((listener1 (lambda ()
			(message "listener1")))
	   (listener2 (lambda ()
			(message "listener2")))
	   (listener3 (lambda ()
			(message "listener3")))
	   (wildcard-listener (lambda ()
				(message "wildcard listener")))
	   (shared-listener (lambda ()
				(message "shared listener")))
	   listener-table
	   listener-list
	   (shared-mgr (qingeditor/eventmgr/shared-mgr)))
        (qingeditor/eventmgr/mgr/attach mgr "event1" listener1)
	(qingeditor/eventmgr/mgr/attach mgr "event1" listener1 2)
	(qingeditor/eventmgr/mgr/attach mgr "event1" listener2 2)
	(qingeditor/eventmgr/mgr/attach mgr "event2" listener2 4)
	(qingeditor/eventmgr/mgr/attach mgr "event2" listener3 2)
	(setq listener-list
	      (qingeditor/eventmgr/mgr/get-listeners-by-event-name mgr "event1"))
	(should (equalp listener-list (list listener1 listener2 listener1)))
	(setq listener-list
	      (qingeditor/eventmgr/mgr/get-listeners-by-event-name mgr "event1"))
	(should (equalp listener-list (list listener1 listener2 listener1)))
	(setq listener-list
	      (qingeditor/eventmgr/mgr/get-listeners-by-event-name mgr "event222"))
	(should (equal listener-list nil))
	(setq listener-list
	      (qingeditor/eventmgr/mgr/get-listeners-by-event-name mgr "event2"))
	(should (equal listener-list (list listener2 listener3)))
	(qingeditor/eventmgr/mgr/attach mgr "*" wildcard-listener 10)
	(setq listener-list
	      (qingeditor/eventmgr/mgr/get-listeners-by-event-name mgr "event2"))
	(should (equal listener-list (list wildcard-listener listener2 listener3)))
	(qingeditor/eventmgr/mgr/set-shared-mgr mgr shared-mgr)
	(qingeditor/eventmgr/shared-mgr/attach
	 shared-mgr "identifier" "event2" shared-listener)
	(setq listener-list
	      (qingeditor/eventmgr/mgr/get-listeners-by-event-name mgr "event2"))
	(should (equal listener-list (list wildcard-listener listener2 listener3)))
	(qingeditor/eventmgr/mgr/set-identifiers mgr '("identifier"))
	(setq listener-list
	      (qingeditor/eventmgr/mgr/get-listeners-by-event-name mgr "event2"))
	(should (equal listener-list (list wildcard-listener listener2 listener3 shared-listener)))
	(qingeditor/eventmgr/shared-mgr/attach
	 shared-mgr "identifier" "event2" shared-listener 100)
	(setq listener-list
	      (qingeditor/eventmgr/mgr/get-listeners-by-event-name mgr "event2"))
	(should (equal listener-list
		       (list shared-listener wildcard-listener
			     listener2 listener3 shared-listener)))))))
