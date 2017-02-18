;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; Commentary:
;;
;; define a simple hash table class
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
    :documentaion "The raw `elisp' hash table object."))
  :documentation "A simple hash table class

provide some useful method that manipulate the raw hash table object.")

(defun qingeditor/hash-table/init (&optional test)
  "Init a `qingeditor/hash-table' object, `test' arg specify the test function.
default: `equal', supported options: `equal', `eq' and `eql', or you can use
`define-hash-table-test' to define a new test function."
  (make-instance 'qingeditor/hash-table
                 :table (make-hash-table :test (or test 'equal))))

(defmacro qingeditor/cls/make-table-from-paires (&rest pairs)
  "use `pair' arg to init a `qingeditor/hash-table' object.

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
  "Get value by `key', if the `key' doesn't exist return the `default'."
  (gethash key (oref this :table) default))

(defmethod qingeditor/cls/set ((this qingeditor/hash-table) key value)
  "Set the value specified by `key', if the `key' already exist,
the old value replaced by `value'."
  (puthash key value (oref this :table))
  nil)

(defmethod qingeditor/cls/count ((this qingeditor/hash-table))
  "Get the count of the hash table."
  (hash-table-count (oref this :table)))

(defmethod qingeditor/cls/update-from-hash-table
  ((this qingeditor/hash-table) from-table)
  "Use other `qingeditor/hash-bale' object to set current object values."
  (qingeditor/cls/update-from-raw-hash-table this (oref from-table :table)))

(defmethod qingeditor/cls/update-from-raw-hash-table
  ((this qingeditor/hash-table) from-table)
  "Use raw `hash table' to set current object values."
  (maphash (lambda (key value)
             (qingeditor/cls/set this key value))
           from-table)
  nil)

(defmethod qingeditor/cls/merge-from-hash-tables
  ((this qingeditor/hash-table) &rest tables)
  "Merge other `qingeditor/hash-table' key-values object."
  (mapc (lambda (table)
          (qingeditor/cls/update-from-raw-hash-table this (oref table :table)))
        tables)
  this)

(defmethod qingeditor/cls/merge-from-raw-hash-tables
  ((this qingeditor/hash-table) &rest tables)
  "Merge raw `hash table' key-values into current object."
  (mapc (lambda (table)
	  (qingeditor/cls/update-from-raw-hash-table this table))
	tables)
  this)

(defmethod qingeditor/cls/remove ((this qingeditor/hash-table) key)
  "Delete the `key' from hash-table."
  (remhash key (oref this :table))
  this)

(defmethod qingeditor/cls/clear ((this qingeditor/hash-table))
  "Clear hash table."
  (clrhash (oref this :table)))

(defmethod qingeditor/cls/map ((this qingeditor/hash-table) func)
  "Iterate hash table and apply function `func', pass the `key' and `value'
arguments, the function collect the return value from `func' and return."
  (let (results)
    (maphash
     (lambda (key value)
       (push (funcall func key value) results))
     (oref this :table))
    results))

(defmacro qingeditor/cls/iterate-items-with-result (table form)
  "In the iterate function context, in the context, you can use
`key' and `value', execute the `form'."
  `(qingeditor/cls/map ,table (lambda (key value) ,form)))

(defmethod qingeditor/cls/keys ((this qingeditor/hash-table))
  "Get all the keys of the hash table."
  (qingeditor/cls/iterate-items-with-result this key))

(defmethod qingeditor/cls/values ((this qingeditor/hash-table))
  "Get all the values of the hash table."
  (qingeditor/cls/iterate-items-with-result this value))

(defmethod qingeditor/cls/items ((this qingeditor/hash-table))
  "Return the list of hash table key-value pair. return the
result."
  (qingeditor/cls/iterate-items-with-result this (list key value)))

(defmacro qingeditor/cls/iterate-items (table form)
  "In the iterate context, execute the `form', you can use `key' and `value'
of the every item of hash table. This macro doesn't return result."
  `(maphash (lambda (key value) ,form) (oref ,table :table)))

(defmethod qingeditor/cls/find ((this qingeditor/hash-table) func)
  "Find item by the function `func', the `func' has `key' and `value' arguments."
  (catch 'break
    (maphash
     (lambda (key value)
       (when (funcall func key value)
         (throw 'break (list key value))))
     (oref this :table))))

(defmethod qingeditor/cls/empty ((this qingeditor/hash-table))
  "If current hash table is empty, return `nil', otherwise return `t'."
  (zerop (qingeditor/cls/count this)))

(defmethod qingeditor/cls/set-from-alist
  ((this qingeditor/hash-table) alist)
  "Use `alist' to set current hash table, the later has higher priority
in the `alist'."
  (dolist (pair alist this)
    (let ((key (car pair))
	  (value (cdr pair)))
      (qingeditor/cls/set this key value))))

(defun qingeditor/cls/get-pair-list-from-plist (plist)
  "Get `cons' collection from `plist', if the count of `plist' is odd, throw
`error'."
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
  "Use `plist' to set the current hash-table object, the later has the
higher priority."
  (dolist (pair (qingeditor/cls/get-pair-list-from-plist plist))
    (let ((key (car pair))
	  (value (cadr pair)))
      (qingeditor/cls/set this key value))))

(defmethod qingeditor/cls/to-plist ((this qingeditor/hash-table))
  "Convert current hash table into `plist', Note: this method is not inverse of
`qingeditor/hash-table/set-from-plist', below is not guarantee:
\(let (data '(a b c d))
    (qingeditor/cls/set-from-plist ht data)
    (equalp data (qingeditor/cls/to-plist ht)))"
  (apply 'append (qingeditor/cls/items this)))

(defmethod qingeditor/cls/to-alist ((this qingeditor/hash-table))
  "Convert current hash table into `alist', Note: this method is not inverse of
`qingeditor/cls/set-from-alist', below is not guarantee:
\(let (data '(a b c d))
    (qingeditor/cls/set-from-alist ht data)
    (equalp data (qingeditor/cls/to-alist ht)))"
  (qingeditor/cls/iterate-items-with-result this (cons key value)))

(defmethod qingeditor/cls/has-key ((this qingeditor/hash-table) key)
  "If current hash table has `key' return `t' otherwise return `nil'."
  (not (eq (gethash key (oref this :table) 'qingeditor/hash-table--not-found)
	   'qingeditor/hash-table--not-found)))

(defmethod qingeditor/cls/select ((this qingeditor/hash-table) func)
  "Select the item that function `func' return `t', function `func' has two
arguments `key' and `value'."
  (let ((result (qingeditor/hash-table/init)))
    (maphash (lambda (key value)
	       (when (funcall func key value)
		 (qingeditor/cls/set result key value))) (oref this :table))
    result))

(defmethod qingeditor/cls/remove-item-by ((this qingeditor/hash-table) func)
  "Delete the item that function `func' return `t', function `func' passed two
arguments `key' and `value'."
  (let ((table (oref this :table)))
    (maphash (lambda (key value)
	       (when (funcall func key value)
           (remhash key table)))
             table)
    this))

(defmethod qingeditor/cls/clone ((this qingeditor/hash-table))
  "Clone current hash table object."
  (let ((new-table (qingeditor/hash-table/init)))
    (oset new-table :table (copy-hash-table (oref this :table)))
    new-table))

(provide 'qingeditor-hash-table)
