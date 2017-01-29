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
