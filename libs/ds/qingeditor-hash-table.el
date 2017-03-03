;;; qingeditor --- a distribution of Emacs editor
;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; define a simple hash table class
;;
;; for dolist
(eval-when-compile (require 'cl))

(defmacro qingeditor/hash-table/make-table-from-paires (&rest pairs)
  "use `pair' arg to init a `hash-table' object.

\(fn (KEY-1 VALUE-1) (KEY-2 VALUE-2) (KEY-3 VALUE-3))"
  (let* ((table-symbol (make-symbol "hash-tabel-temp"))
         (assignments
          (mapcar
           (lambda (pair)
             `(qingeditor/hash-table/set ',table-symbol ,@pair))
           pairs)))
    `(let ((,table-symbol (make-hash-table)))
       ,@assignments
       ,table-symbol)))

(defun qingeditor/hash-table/get (table key &optional default)
  "Get value by `key', if the `key' doesn't exist return the `default'."
  (gethash key table default))

(defun qingeditor/hash-table/set (table key value)
  "Set the value specified by `key', if the `key' already exist,
the old value replaced by `value'."
  (puthash key value table))

(defun qingeditor/hash-table/count (table)
  "Get the count of the hash table."
  (hash-table-count table))

(defun qingeditor/hash-table/merge-other-table (table from-table)
  "Use other `hash table' to set current table values."
  (maphash (lambda (key value)
             (puthash key value table))
           from-table))

(defun qingeditor/hash-table/merge-other-tables (table from-tables)
  "Merge other `hash-table' key-values object."
  (mapc (lambda (from-table)
          (qingeditor/hash-table/merge-other-table table from-table))
        from-tables))

(defun qingeditor/hash-table/remove (table key)
  "Delete the `key' from hash-table."
  (remhash key table))

(defun qingeditor/hash-table/clear (table)
  "Clear hash table."
  (clrhash table))

(defun qingeditor/hash-table/map (table func)
  "Iterate hash table and apply function `func', pass the `key' and `value'
arguments, the function collect the return value from `func' and return."
  (let (results)
    (maphash
     (lambda (key value)
       (push (funcall func key value) results))
     table)
    results))

(defun qingeditor/hash-table/mapc (table func)
  "Iterate hash table and apply function `func', pass the `key' and `value'
arguments, just side effect."
  (maphash
   (lambda (key value)
     (funcall func key value))
   table))

(defun qingeditor/hash-table/keys (table)
  "Get all the keys of the hash table."
  (qingeditor/hash-table/map
   table
   (lambda (key value)
     key)))

(defun qingeditor/hash-table/values (table)
  "Get all the values of the hash table."
  (qingeditor/hash-table/map
   table
   (lambda (key value)
     value)))

(defun qingeditor/hash-table/items (table)
  "Return the list of hash table key-value pair. return the
result."
  (qingeditor/hash-table/map
   table
   (lambda (key value)
     (list key value))))

(defun qingeditor/hash-table/find (table func)
  "Find item by the function `func', the `func' has `key' and `value' arguments."
  (catch 'break
    (maphash
     (lambda (key value)
       (when (funcall func key value)
         (throw 'break (list key value))))
     table)))

(defun qingeditor/hash-table/empty (table)
  "If current hash table is empty, return `nil', otherwise return `t'."
  (zerop (qingeditor/hash-table/count table)))

(defun qingeditor/hash-table/from-alist (table alist)
  "Use `alist' to set current hash table, the later has higher priority
in the `alist'."
  (dolist (pair alist this)
    (let ((key (car pair))
          (value (cdr pair)))
      (qingeditor/hash-table/set table key value))))

(defun qingeditor/hash-table/get-pair-list-from-plist (plist)
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

(defun qingeditor/hash-table/from-plist (table plist)
  "Use `plist' to set the current hash-table object, the later has the
higher priority."
  (dolist (pair (qingeditor/hash-table/get-pair-list-from-plist plist))
    (let ((key (car pair))
          (value (cadr pair)))
      (qingeditor/hash-table/set table key value))))

(defun qingeditor/hash-table/to-plist (table)
  "Convert current hash table into `plist', Note: this method is not inverse of
`qingeditor/hash-table/from-plist', below is not guarantee:
\(let (data '(a b c d))
    (qingeditor/hash-table/from-plist ht data)
    (equalp data (qingeditor/hash-table/to-plist ht)))"
  (apply 'append (qingeditor/hash-table/items table)))

(defun qingeditor/hash-table/to-alist (table)
  "Convert current hash table into `alist', Note: this method is not inverse of
`qingeditor/hash-table/from-alist', below is not guarantee:
\(let (data '(a b c d))
    (qingeditor/hash-table/set-from-alist ht data)
    (equalp data (qingeditor/hash-table/to-alist ht)))"
  (qingeditor/hash-table/map
   table
   (lambda (key value)
     (cons key value))))

(defun qingeditor/hash-table/has-key (table key)
  "If current hash table has `key' return `t' otherwise return `nil'."
  (not (eq (gethash key table 'qingeditor/hash-table--not-found)
           'qingeditor/hash-table--not-found)))

(defun qingeditor/hash-table/select (table func)
  "Select the item that function `func' return `t', function `func' has two
arguments `key' and `value'."
  (let ((result (make-hash-table :test (or test 'equal))))
    (maphash (lambda (key value)
               (when (funcall func key value)
                 (qingeditor/hash-table/set result key value))) table)
    result))

(defun qingeditor/hash-table/remove-item-by (table func)
  "Delete the item that function `func' return `t', function `func' passed two
arguments `key' and `value'."
  (maphash (lambda (key value)
             (when (funcall func key value)
               (remhash key table)))
           table))

(defun qingeditor/hash-table/clone (table)
  "Clone current hash table object."
  (copy-hash-table table))

(provide 'qingeditor-hash-table)
