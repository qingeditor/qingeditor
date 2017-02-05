;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; Commentary:
;;
;; 定义一个hash table类
;;
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
    :documentaion "底层的`hash-table'对象。"))
  :documentation "对原生的`hash table'做简单的封装。")

(defun qingeditor/hash-table/init (&optional test)
  "初始化一个`hash table'实例，`test'参数指定使用的比较函数。
默认为`equal'，你可以指定为`eq', `eql', `equal'或者通过函数
`define-hash-table-test'定义的测试函数。"
  (make-instance 'qingeditor/hash-table
   :table (make-hash-table :test (or test 'equal))))

(defmacro qingeditor/cls/make-table-from-paires (&rest pairs)
  "通过制定一系列的`pair'来初始化一个`hash-table'对象

\(fn (KEY-1 VALUE-1) (KEY-2 VALUE-2) (KEY-3 VALUE-3))"
  (let* ((table-symbol (make-symbol "hash-tabel-temp"))
	 (assignments
	  (mapcar
	   (lambda (pair)
	     `(qingeditor/cls/set ,table-symbol ,@pair))
	   pairs)))
    `(let ((,table-symbol (qingeditor/hash-table/init)))
       ,@assignments
       ,table-symbol)))

(defmethod qingeditor/cls/get ((this qingeditor/hash-table) key &optional default)
  "通过指定的`key'获取哈希表的项，如果没找到返回`default'参数指定的值。"
  (gethash key (oref this :table) default))

(defmethod qingeditor/cls/set ((this qingeditor/hash-table) key value)
  "设置哈希表的项，如果指定的`key'已经存在，则替换老的值，函数返回`nil'。"
  (puthash key value (oref this :table))
  nil)

(defmethod qingeditor/cls/count ((this qingeditor/hash-table))
  "获取当前`hash-table'的元素的个数。"
  (hash-table-count (oref this :table)))

(defmethod qingeditor/cls/update-from-hash-table
  ((this qingeditor/hash-table) from-table)
  "从指定的`qingeditor/hash-table'对象设置本哈希表。"
  (qingeditor/cls/update-from-raw-hash-table this (oref from-table :table)))

(defmethod qingeditor/cls/update-from-raw-hash-table
  ((this qingeditor/hash-table) from-table)
  "使用指定的原生的哈希对象来设置本身的键值对数据。"
  (maphash (lambda (key value)
	     (qingeditor/cls/set this key value))
	   from-table)
  nil)

(defmethod qingeditor/cls/merge-from-hash-tables
  ((this qingeditor/hash-table) &rest tables)
  "合并其他`qingeditor/hash-table'的实例。"
  (mapc (lambda (table)
	  (qingeditor/cls/update-from-raw-hash-table this (oref table :table)))
	tables)
  this)

(defmethod qingeditor/cls/merge-from-raw-hash-tables
  ((this qingeditor/hash-table) &rest tables)
   "合并参数指定的原生"
  (mapc (lambda (table)
	  (qingeditor/cls/update-from-raw-hash-table this table))
	tables)
  this)

(defmethod qingeditor/cls/remove ((this qingeditor/hash-table) key)
  "删除哈希表中名称为`key'的项。"
  (remhash key (oref this :table))
  this)

(defmethod qingeditor/cls/clear ((this qingeditor/hash-table))
  "清除哈希表数据。"
  (clrhash (oref this :table)))

(defmethod qingeditor/cls/map ((this qingeditor/hash-table) func)
  "对哈希表的所有的项执行`func'函数，并且收集函数返回值，然后将结果列表返回。
`func'传入`key'跟`value'的值。"
  (let (results)
    (maphash
     (lambda (key value)
       (push (funcall func key value) results))
     (oref this :table))
    results))

(defmacro qingeditor/cls/iterate-items-with-result (table form)
  "在键值对的环境下执行指定的`form'，并且收集`form'的返回值。"
  `(qingeditor/cls/map ,table (lambda (key value) ,form)))

(defmethod qingeditor/cls/keys ((this qingeditor/hash-table))
  "获取当前`hash-table'的`keys'。"
  (qingeditor/cls/iterate-items-with-result this key))

(defmethod qingeditor/cls/values ((this qingeditor/hash-table))
  "获取`hash-table'的`values'。"
  (qingeditor/cls/iterate-items-with-result this value))

(defmethod qingeditor/cls/items ((this qingeditor/hash-table))
  "返回`hash-table'的项的集合。"
  (qingeditor/cls/iterate-items-with-result this (list key value)))

(defmacro qingeditor/cls/iterate-items (table form)
  "在键值对的环境下执行指定的`form'，不收集`form'的返回值。"
  `(maphash (lambda (key value) ,form) (oref ,table :table)))

(defmethod qingeditor/cls/find ((this qingeditor/hash-table) func)
  "寻找回调函数`func'返回真值的元素。
`func'传入参数`key'和`value'。"
  (catch 'break
    (maphash
     (lambda (key value)
       (when (funcall func key value)
	 (throw 'break (list key value))))
     (oref this :table))))

(defmethod qingeditor/cls/empty ((this qingeditor/hash-table))
  "判断当前的`hash-table'是否为空。"
  (zerop (qingeditor/cls/count this)))

(defmethod qingeditor/cls/set-from-alist ((this qingeditor/hash-table) alist)
  "使用`alist'来设置当前`hash-table'的值。出现在`alist'后面的优先级高。"
  (dolist (pair alist this)
    (let ((key (car pair))
	  (value (cdr pair)))
      (qingeditor/cls/set this key value))))

(defun qingeditor/cls/get-pair-list-from-plist (plist)
  "通过一个`plist'获取`cons'集合。如果`plist'的元素不是偶数抛出`error'。"
  (let ((results)
	(sublist)
	(len 0))
    (while plist
      (setq sublist (cons (car plist) sublist))
      (setq plist (cdr plist))
      (setq len (1+ len))
      (when (= len 2)
	(setq results (cons (nreverse sublist) results))
	(setq sublist nil)
	(setq len 0)))
    (when sublist
      (error "Expect an even number of elements"))
    (nreverse results)))

(defmethod qingeditor/cls/set-from-plist ((this qingeditor/hash-table) plist)
  "使用`alist'来设置当前`hash-table'的值。出现在`alist'后面的优先级高。"
  (dolist (pair (qingeditor/cls/get-pair-list-from-plist plist))
    (let ((key (car pair))
	  (value (cadr pair)))
      (qingeditor/cls/set this key value))))

(defmethod qingeditor/cls/to-plist ((this qingeditor/hash-table))
  "将当前的`hash-table'转换成`plist'，注意这个方法跟`qingeditor/hash-table/set-from-plist'
不是可逆的函数。一下操作不一定成功：
\(let (data '(a b c d))
    (qingeditor/cls/set-from-plist ht data)
    (equalp data (qingeditor/cls/to-plist ht)))"
  (apply 'append (qingeditor/cls/items this)))

(defmethod qingeditor/cls/to-alist ((this qingeditor/hash-table))
  "将当前的`hash-table'转换成`alist'，注意这个方法跟`qingeditor/cls/set-from-alist'
不是可逆的函数。一下操作不一定成功：
\(let (data '(a b c d))
    (qingeditor/cls/set-from-alist ht data)
    (equalp data (qingeditor/cls/to-alist ht)))"
  (qingeditor/cls/iterate-items-with-result this (cons key value)))

(defmethod qingeditor/cls/has-key ((this qingeditor/hash-table) key)
  "判断当前的`hash-table'是否含有键为`key'的项，如果有返回`t'，否则返回`nil'"
  (not (eq (gethash key (oref this :table) 'qingeditor/hash-table--not-found)
	   'qingeditor/hash-table--not-found)))

(defmethod qingeditor/cls/select ((this qingeditor/hash-table) func)
  "返回一个新的`hash-table'，包含所有`func'返回真值的元素
`func'传入参数`key'和`value'。"
  (let ((result (qingeditor/hash-table/init)))
    (maphash (lambda (key value)
	       (when (funcall func key value)
		 (qingeditor/cls/set result key value))) (oref this :table))
    result))

(defmethod qingeditor/cls/remove-item-by ((this qingeditor/hash-table) func)
  "对`hash-table'中的元素调用函数`func'如果返回`t'就删除当前的元素。"
  (let ((table (oref this :table)))
    (maphash (lambda (key value)
	       (when (funcall func key value)
		 (remhash key table)))
	     table)
    this))

(defmethod qingeditor/cls/clone ((this qingeditor/hash-table))
  "克隆当前的`hash-table'对象。"
  (let ((new-table (qingeditor/hash-table/init)))
    (oset new-table :table (copy-hash-table (oref this :table)))
    new-table))

(provide 'qingeditor-hash-table)
