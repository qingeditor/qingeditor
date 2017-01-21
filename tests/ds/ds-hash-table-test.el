;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; 测试数据结构包中的`hash-table'

(require 'ert)
(load-file (expand-file-name (concat user-emacs-directory "/tests/env-init.el")))
(require 'qingeditor-hash-table)

;;(setq ert-batch-backtrace-right-margin 1000)

(defun qingeditor/test/ds/prepare-hash-table (test-func)
  "为测试准备一个`hash-table`对象'。'"
  (let ((ht (qingeditor/hash-table/init)))
    (funcall test-func)))

(ert-deftest qingeditor/test/ds/create-hash-table-test ()
  :tags '(qingeditor/ds/hash-table/create-hash-table)
  (let ((ht (qingeditor/hash-table/init)))
    (should (object-of-class-p ht qingeditor/hash-table))))

(ert-deftest qingeditor/test/ds/hash-table-set-test ()
  :tags '(qingeditor/ds/hash-table/set)
  "`hash-table'的`set'函数测试。"
  (qingeditor/test/ds/prepare-hash-table
   (lambda ()
     (let ((table (oref ht :table))
	   (new-table (make-hash-table)))
       ;; 普通的值
       (qingeditor/hash-table/set ht 'name "softboy")
       (qingeditor/hash-table/set ht 'age 12)
       ;; 存放一下`list'
       (qingeditor/hash-table/set ht 'props '(a b c))
       ;; 存放一下`hash-table'
       (qingeditor/hash-table/set ht 'data new-table)
       (should (equal (hash-table-count table) 4))
       (should (equal (gethash 'name table) "softboy"))
       (should (equal (gethash 'age table) 12))
       (should (equal (gethash 'props table) '(a b c)))
       (should (eq (gethash 'data table) new-table))))))

(ert-deftest qingeditor/test/ds/hash-table-get-test ()
  :tags '(qingeditor/ds/hash-table/get)
  "`hash-table'的`get'函数测试。"
  (qingeditor/test/ds/prepare-hash-table
   (lambda ()
     (let ((table (oref ht :table))
	   (new-table (make-hash-table)))
       ;; 普通的值
       (qingeditor/hash-table/set ht 'name "softboy")
       (qingeditor/hash-table/set ht 'age 12)
       ;; 存放一下`list'
       (qingeditor/hash-table/set ht 'props '(a b c))
       ;; 存放一下`hash-table'
       (qingeditor/hash-table/set ht 'data new-table)
       (should (equal (qingeditor/hash-table/get ht 'name) "softboy"))
       (should (equal (qingeditor/hash-table/get ht 'age) 12))
       (should (equal (qingeditor/hash-table/get ht 'props) '(a b c)))
       (should (eq (qingeditor/hash-table/get ht 'data) new-table))
       (should (equal (qingeditor/hash-table/get ht 'not-exist "haha") "haha"))))))

(ert-deftest qingeditor/test/ds/hash-table-update-from-raw-hash-table-test ()
  :tags '(qingeditor/ds/hash-table/update-from-raw-hash-table)
  "测试从原生的`hash-table'批量导入数据。"
  (qingeditor/test/ds/prepare-hash-table
   (lambda ()
     (let ((raw-table (make-hash-table)))
       (qingeditor/hash-table/set ht 'org-data "org-data")
       ;; 数据设置
       (puthash 'name "softboy" raw-table)
       (puthash 'age 12 raw-table)
       (puthash 'data '(12 2 3) raw-table)
       (qingeditor/hash-table/update-from-raw-hash-table ht raw-table)
       ;; 判断结果
       (should (equal (qingeditor/hash-table/count ht) 4))
       (should (equal (qingeditor/hash-table/get ht 'name) "softboy"))
       (should (equal (qingeditor/hash-table/get ht 'age) 12))
       (should (equal (qingeditor/hash-table/get ht 'data) '(12 2 3)))
       ;; 不能覆盖老数据
       (should (equal (qingeditor/hash-table/get ht 'org-data) "org-data"))))))

(ert-deftest qingeditor/test/ds/hash-table-update-from-hash-table-test ()
  :tags '(qingeditor/ds/hash-table/update-from-hash-table)
  "从其他`qingeditor/hash-table'对象批量设置。"
  (qingeditor/test/ds/prepare-hash-table
   (lambda ()
     (let ((new-table (qingeditor/hash-table/init)))
       (qingeditor/hash-table/set new-table 'name "softboy")
       (qingeditor/hash-table/set new-table 'age 12)
       (qingeditor/hash-table/set new-table 'data '(1 2 3 4))
       ;; 导入数据
       (qingeditor/hash-table/update-from-hash-table ht new-table)
       (should (equal (qingeditor/hash-table/count ht) 3))
       (should (equal (qingeditor/hash-table/get ht 'name) "softboy"))
       (should (equal (qingeditor/hash-table/get ht 'age) 12))
       (should (equal (qingeditor/hash-table/get ht 'data) '(1 2 3 4)))))))



