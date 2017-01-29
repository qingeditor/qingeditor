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
(require 'qingeditor-fast-priority-queue)

(defun qingeditor/test/ds/prepare-fast-priority-queue (test-func)
  "为测试准备一个`fast-priority-queue`对象'。'"
  (let ((queue (qingeditor/fast-priority-queue)))
    (let ((init-data-paires (qingeditor/test/ds/get-fast-priority-queue-data-pairs)))
      (dolist (pair init-data-paires)
	(qingeditor/fast-priority-queue/insert queue (car pair) (cdr pair)))
      (funcall test-func))
    ))

(defun qingeditor/test/ds/get-fast-priority-queue-data-pairs ()
  '(("test3" . -1)
    ("test5" . -10)
    ("test1" . 5)
    ("test2" . 2)
    ("test4" . -1)
    ("test6" . -10)))

(ert-deftest qingeditor/test/ds/fast-priority-queue-insert-test ()
  :tags '(qingeditor/fast-priority-queue/insert)
  (qingeditor/test/ds/prepare-fast-priority-queue
   (lambda ()
     (let ((priority1-list (qingeditor/hash-table/get
			    (oref queue :values) 2))
	   (priority2-list (qingeditor/hash-table/get
			    (oref queue :values) -1)))
       (should (equalp priority1-list '("test2")))
       (should (equalp priority2-list '("test4" "test3")))
       (should (equalp (qingeditor/hash-table/count (oref queue :values)) 4))))))

(ert-deftest qingeditor/test/ds/fast-priority-queue-to-list-test ()
  :tags '(qingeditor/fast-priority-queue/to-list)
  (qingeditor/test/ds/prepare-fast-priority-queue
   (lambda ()
     (let ((list (qingeditor/fast-priority-queue/to-list queue)))
       (should (equalp list '("test1" "test2" "test3" "test4" "test5" "test6")))
       ;; 测试重复调用
       (setq list (qingeditor/fast-priority-queue/to-list queue))
       (should (equalp list '("test1" "test2" "test3" "test4" "test5" "test6")))))))

(ert-deftest qingeditor/test/ds/fats-priority-queue-get-priority-list-test ()
  :tags '(qingeditor/fast-priority-queue-get-priority-list)
  (qingeditor/test/ds/prepare-fast-priority-queue
    (lambda ()
    (let (list)
      (setq list (qingeditor/fast-priority-queue/get-priority-list queue -1))
      (should (not (eq list
		       (qingeditor/hash-table/get (oref queue :values) -1))))
      (should (equalp list '("test4" "test3")))))))

(ert-deftest qingeditor/test/ds/fast-priority-queue-remove-test ()
  :tags '(qingeditor/fast-priority-queue-remove)
  (qingeditor/test/ds/prepare-fast-priority-queue
   (lambda ()
      (qingeditor/fast-priority-queue/remove queue "test3")
      (let (list)
      (setq list (qingeditor/fast-priority-queue/get-priority-list queue -1))
      (should (equalp list '("test4")))
      (should (equal nil (qingeditor/fast-priority-queue/remove queue "test3")))
      (should (equal t (qingeditor/fast-priority-queue/remove queue "test4")))
      (setq list (qingeditor/fast-priority-queue/get-priority-list queue -1))
      (should (equalp list nil))
      ))))

(ert-deftest qingeditor/test/ds/fast-priority-queue-empty-test ()
  :tags '(qingeditor/fast-priority-queue-empty)
  (lambda ()
    (let (new-queue)
      (should (eq (qingeditor/fast-priority-queue/empty queue) nil))
      (setq new-queue (qingeditor/fast-priority-queue))
      (should (eq (qingeditor/fast-priority-queue/empty new-queue) t)))))


(ert-deftest qingeditor/test/ds/fast-priority-queue-contains-test ()
  :tags '(qingeditor/fast-priority-queue/contains)
  (lambda ()
    (should (eq (qingeditor/fast-priority-queue/contains queue "test9") nil))
    (should (eq (qingeditor/fast-priority-queue/contains queue "test3") t))
    (should (eq (qingeditor/fast-priority-queue/contains queue "test4") t))))
