;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;; 
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; 定义一个hash table类

;; for dolist
(eval-when-compile (require 'cl))

(defclass qingeditor/hash-table ()
  ((table
    :initarg :table
    :initform nil
    :type (satisfies
	   (lambda (data)
	     (or (null data)
		 (hash-table-p data))))
    :protection private
    :documentaion "底层的`hash-table'对象。"))
  :documentation "对原生的`hash table'做简单的封装。")

(defun qingeditor/hash-table/init (&optional test)
  "初始化一个`hash table'实例，`test'参数指定使用的比较函数。
默认为`equal'，你可以指定为`eq', `eql', `equal'或者通过函数
`define-hash-table-test'定义的测试函数。"
  (qingeditor/hash-table
   :table (make-hash-table :test (or test 'equal))))

(defmethod qingeditor/hash-table/get ((this qingeditor/hash-table) key &optional default)
  "通过指定的`key'获取哈希表的项，如果没找到返回`default'参数指定的值。"
  (gethash key (oref this :table) default))

(defmethod qingeditor/hash-table/set ((this qingeditor/hash-table) key value)
  "设置哈希表的项，如果指定的`key'已经存在，则替换老的值，函数返回`nil'。"
  (puthash key value (oref this :table))
  nil)

(defmethod qingeditor/hash-table/update-from-hash-table
  ((this qingeditor/hash-table) from-table)
  "从指定的`qingeditor/hash-table'对象设置本哈希表。"
  (qingeditor/hash-table/update-from-raw-hash-table this (oref from-table :table)))

(defmethod qingeditor/hash-table/update-from-raw-hash-table
  ((this qingeditor/hash-table) from-table)
  "使用指定的原生的哈希对象来设置本身的键值对数据。"
  (maphash (lambda (key value)
	     (qingeditor/hash-table/set this key value))
	   from-table)
  nil)

(defmethod qingeditor/hash-table/merge-from-hash-tables
  ((this qingeditor/hash-table) &rest tables)
  "合并其他`qingeditor/hash-table'的实例。"
  (mapc (lambda (table)
	  (qingeditor/hash-table/update-from-raw-hash-table this (oref table :table))))
  this)

(defmethod qingeditor/hash-table/merge-from-raw-hash-tables
  ((this qingeditor/hash-table) &rest tables)
   "合并参数指定的原生"
  (mapc (lambda (table)
	  (qingeditor/hash-table/update-from-raw-hash-table this table)))
  this)

(defmethod qingeditor/hash-table/remove ((this qingeditor/hash-table) key)
  "删除哈希表中名称为`key'的项。"
  (remhash key (oref this :table))
  this)

(defmethod qingeditor/hash-table/clear ((this qingeditor/hash-table))
  "清除哈希表数据。"
  (clrhash (oref this :table)))

(defmethod qingeditor/hash-table/map ((this qingeditor/hash-table) func)
  "对哈希表的所有的项执行`func'函数，并且收集函数返回值，然后将结果列表返回。
`func'传入`key'跟`value'的值。"
  (let (results)
    (maphash
     (lambda (key value)
       (push (funcall func key value) results))
     (oref this :table))
    results))

(defmacro qingeditor/hash-table/eval-with-entry (table form)
  "在键值对的环境下执行指定的`form'。"
  `(qingeditor/hash-table/map ,table (lambda (key value) ,form)))

(defmethod qingeditor/hash-table/keys ((this qingeditor/hash-table))
  "获取当前`hash-table'的`keys'。"
  (qingeditor/hash-table/eval-with-entry this key))

(defmethod qingeditor/hash-table/values ((this qingeditor/hash-table))
  "获取`hash-table'的`values'。"
  (qingeditor/hash-table/eval-with-entry this value))

(defmethod qingeditor/hash-table/items (table)
  "返回`hash-table'的项的集合。"
  (qingeditor/hash-table/eval-with-entry this (list key value) table))

(provide 'qingeditor-hash-table)
