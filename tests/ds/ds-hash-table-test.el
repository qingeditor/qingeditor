;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;; 
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
  (let ((ht (qingeditor/hash-table/init))
	(num 1))
    (should (object-of-class-p ht qingeditor/hash-table))))




