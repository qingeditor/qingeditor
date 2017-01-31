;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;; 
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;; 
;; 定义事件管理器

(require 'qingeditor-eventmgr-shared-mgr)
(require 'qingeditor-eventmgr-event)

(defclass qingeditor/eventmgr/mgr ()
  ((events
   :initarg :events
   :initform (qingeditor/hash-table/init)
   :type qingeditor/hash-table
   :documentation "事件数据存储的地方。")

   (identifiers
    :initarg :identifiers
    :initform nil
    :type list
    :reader qingeditor/eventmgr/get-identifiers
    :documentation "用于从共享的事件对象管理器获取事件监听的`identifiers'名字集合。")

   (shared-mgr
    :initarg :shared-mgr
    :initform nil
    :type (satisfies (lambda (mgr) (or (null mgr) (object-of-class-p mgr))))
    :reader qingeditor/eventmgr/get-shared-mgr
    :documentation "共享事件管理器对象的引用。"))
  :documentation "事件管理器，为单个对象提供事件派发能力。")

(defun qingeditor/eventmgr/mgr/init (&optional shared-mgr identifiers)
  "初始化一个事件管理器对象。"
  (let ((mgr (qingeditor/eventmgr/mgr)))
    (when (and (not (stringp identifiers))
	       (not (listp identifiers)))
      (error "Identifier names passed to `%s' must be a string or list, but %s given."
	     "qingeditor/eventmgr/init" (type-of identifiers)))
    (when shared-mgr
      (oset this :shared-mgr shared-mgr)
      (qingeditor/eventmgr/set-identifiers this identifiers))
    mgr))

(defmethod qingeditor/eventmgr/mgr/attach
  ((this qingeditor/eventmgr/mgr) event-name listener &optional priority)
  "给当前事件管理器指定的`event-name'添加事件监听器。"
  (when (null priority)
    (setq priority 1))
  (when (or (not (stringp event-name))
	    (eq (length event-name) 0))
    (error "event name passed to `%s' must be a string and can be empty, but %s given."
	   "qingeditor/eventmgr/mgr/attach" (type-of event-name))))

(defmethod qingeditor/eventmgr/mgr/set-identifiers
  ((this qingeditor/eventmgr/mgr) identifiers)
  "设置获取共享事件管理对象中的监听器的`identifier'键集合。"
  (delete-dups identifiers)
  (oset this :identifiers identifiers)
  this)

(defmethod qingeditor/eventmgr/mgr/add-identifiers
  ((this qingeditor/eventmgr/mgr) identifiers)
  "添加`identifiers'。"
  (oset this :identifiers
	(delete-dups (append identifiers (oref this :identifiers))))
  this)

(defmethod qingeditor/eventmgr/mgr/trigger
  ((this qingeditor/eventmgr/mgr) event-name &optional target argv)
  "触发一个事件`event-name'，调用所有相关的回调函数。"
  (let ((event (qingeditor/eventmgr/event/init (name target argv))))
    (qingeditor/eventmgr/mgr/trigger-listeners this event)))

(defmethod qingeditor/eventmgr/mgr/get-listeners-by-event-name
  ((this qingeditor/eventmgr/mgr) event-name)
  "获取指定事件`event-name'的监听对象。"
  (let ((merged-listener-table (qingeditor/hash-table/init))
	(listener-table
	 wildcard-listener-table
	 shared-listener-table
	 listener-alist
	 listeners))
    (if (qingeditor/hash-table/has-key (oref this :events) event-name)
	(setq listener-table (qingeditor/hash-table/get (oref this :events) event-name))
      (setq listener-table (qingeditor/hash-table/init)))
    (if (qingeditor/hash-table/has-key (oref this :events) "*")
	(setq wildcard-listener-table (qingeditor/hash-table/get (oref this :events) "*"))
      (setq wildcard-listener-table (qingeditor/hash-table/init)))
    (if (not (null (oref this :shared-mgr)))
	(setq shared-listener-table
	      (qingeditor/eventmgr/shared-mgr/get-listeners
	       (oref this :shared-mgr) (oref this :identifiers) event-name))
      (setq shared-listener-table (qingeditor/hash-table/init)))
    (setq merged-listener-table
	  (qingeditor/eventmgr/mgr/merge
	   this merged-listener-table listener-table wildcard-listener-table shared-listener-table))
    (setq listener-alist (qingeditor/hash-table/to-alist merged-listener-table))
    
    ))

(defmethod qingeditor/eventmgr/mgr/merge
  ((this qingeditor/eventmgr/mgr) &rest tables)
  "递归合并指定的事件`hash-table'。"
  (let ((target (qingeditor/hash-table/init)))
    (dolist (table tables)
      (qingeditor/hash-table/iterate-items
       table
       (progn
	 (if (not (qingeditor/hash-table/has-key target key))
	     (qingeditor/hash-table/set target key (copy-sequence value))
	   (let ((source-listeners (copy-sequence value))
		 (target-listeners (qingeditor/hash-table/get target key)))
	     (setq target-listeners (append target-listeners source-listeners))
	     (qingeditor/hash-table/set target key target-listeners))))))
    target))

(provide 'qingeditor-eventmgr-mgr)
