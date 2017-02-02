;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;; 
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;; 
;; 封装共享的事件管理器

(require 'qingeditor-hash-table)
;; for defun* return-from
(require 'cl)

(defclass qingeditor/eventmgr/shared-mgr ()
  ((identifiers
    :initarg :identifiers
    :initform (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "事件数据存储的地方。")
   )
  :documentation "让需要事件派发能力的对象不需要实例化一个事件管理器对象就能派发事件。")

(defmethod qingeditor/cls/attach
  ((this qingeditor/eventmgr/shared-mgr) identifier event listener &optional priority)
  "给事件对象添加事件监听器。"
  (when (eq priority nil)
    (setq priority 1))
  (when (or (not (stringp identifier)) (eq (length identifier) 0))
    (error "Invalid identifier provided; must be a string; received `%s'"
           (type-of identifier)))
  (when (or (not (stringp event)) (eq (length event) 0))
    (error "Invalid event provided; must be a non-empty string; received `%s'"
           (type-of event)))
  (let ((listener-index priority)
        event-table
        listener-table
        listener-list)
    (if (qingeditor/cls/has-key (oref this :identifiers) identifier)
        (setq event-table (qingeditor/cls/get (oref this :identifiers) identifier))
      (setq event-table (qingeditor/hash-table/init))
      (qingeditor/cls/set (oref this :identifiers) identifier event-table))
    (if (qingeditor/cls/has-key event-table event)
        (setq listener-table (qingeditor/cls/get event-table event))
      (setq listener-table (qingeditor/hash-table/init))
      (qingeditor/cls/set event-table event listener-table))
    (when (qingeditor/cls/has-key listener-table priority)
      (setq listener-list (qingeditor/cls/get listener-table priority)))
    (push listener listener-list)
    (qingeditor/cls/set listener-table priority listener-list)))

(defmethod qingeditor/cls/detach
  ((this qingeditor/eventmgr/shared-mgr) listener &optional identifier event-name force)
  "删除指定的监听函数，这个监听函数可能绑定到了很多的事件上面。"
  (catch 'qingeditor-eventmgr-shared-mgr-detach
    (let (event-table
          listener-table
          listener-list)
      (when (or (not identifier)
                (and (string= identifier "*") (not force)))
        (qingeditor/cls/iterate-items
         (oref this :identifiers)
         (qingeditor/cls/detach this listener key event-name t))
        (throw 'qingeditor-eventmgr-shared-mgr-detach t))
      (when (or (not (stringp identifier))
                (eq (length identifier) 0))
        (error "Invalid identifier provided; must be a string, received `%s'" (type-of identifier)))
      (when (not (qingeditor/cls/has-key (oref this :identifiers) identifier))
        (throw 'qingeditor-eventmgr-shared-mgr-detach nil))
      (setq event-table (qingeditor/cls/get (oref this :identifiers) identifier))
      (when (or (not event-name)
                (and (stringp event-name) (string= event-name "*") (not force)))
        (qingeditor/cls/iterate-items
         event-table
         (qingeditor/cls/detach this listener identifier key t))
        (throw 'qingeditor-eventmgr-shared-mgr-detach t))
      (when (or (not (stringp event-name))
                (eq (length event-name) 0))
        (error "Invalid event name provided; must be a string, received `%s'" (type-of event-name)))
      (unless (qingeditor/cls/has-key event-table event-name)
        (throw 'qingeditor-eventmgr-shared-mgr-detach nil))
      (setq listener-table (qingeditor/cls/get event-table event-name))
      (qingeditor/cls/iterate-items
       listener-table
       (progn
         (setq listener-list value)
         (dolist (evaluated-listener listener-list)
           (when (eq evaluated-listener listener)
             ;; 找到了指定的监听对象，删除
             (setq listener-list (delete evaluated-listener listener-list))))
         (if (eq (length listener-list) 0)
             (qingeditor/cls/remove listener-table key)
           (qingeditor/cls/set listener-table key listener-list))))
      (when (qingeditor/cls/empty listener-table)
        (qingeditor/cls/remove event-table event-name))
      (when (qingeditor/cls/empty event-table)
        (qingeditor/cls/remove (oref this :identifiers) identifier)))))

(defmethod qingeditor/cls/get-listeners
  ((this qingeditor/eventmgr/shared-mgr) identifiers event-name)
  "获取指定的`identifiers'集合下的所有的回调函数。

`identifiers'可以是单个字符串，也可以是一个字符串`list'
`event-name'事件的名称。"
  (when (or (not (stringp event-name))
            (string= "*" event-name)
            (eq (length event-name) 0))
    (error "Event name passed to `%s' must be a non-empty, non-wildcard string"
           "qingeditor/eventmgr/shared-mgr/get-listeners"))
  (when (and (not (stringp identifiers))
             (not (listp identifiers)))
    (error "Identifier names passed to `%s' must be a string or list, but %s given."
           "qingeditor/eventmgr/shared-mgr/get-listeners" (type-of identifiers)))
  (when (stringp identifiers)
    (setq identifiers (make-list 1 identifiers)))
  (let (event-table
        wildcard-event-table
        listener-table
        wildcard-listener-table
        (merged-listener-table (qingeditor/hash-table/init)))
    (dolist (identifier identifiers)
      (when (or (not (stringp identifier))
                (eq (length identifier) 0))
        (error "Identifier names passed to `%s' must be a string and can be empty, but %s given."
               "qingeditor/eventmgr/shared-mgr/get-listeners" (type-of identifiers)))
      ;; 不能是`*'
      (when (string= "*" identifier)
        (error "Identifier name can not be `*'"))
      (if (qingeditor/cls/has-key (oref this :identifiers) identifier)
          (setq event-table (qingeditor/cls/get (oref this :identifiers) identifier))
        (setq event-table (qingeditor/hash-table/init)))
      (if (qingeditor/cls/has-key event-table event-name)
          (setq listener-table (qingeditor/cls/get event-table event-name))
        (setq listener-table (qingeditor/hash-table/init)))
      (if (qingeditor/cls/has-key event-table "*")
          (setq wildcard-listener-table (qingeditor/cls/get event-table "*"))
        (setq wildcard-listener-table (qingeditor/hash-table/init)))
      (setq merged-listener-table
            (qingeditor/cls/merge
             this merged-listener-table listener-table wildcard-listener-table)))
    ;; 复制`* identifier'的事件监听器对象
    (when (qingeditor/cls/has-key (oref this :identifiers) "*")
      (setq wildcard-event-table
            (qingeditor/cls/get (oref this :identifiers) "*"))
      (if (qingeditor/cls/has-key wildcard-event-table event-name)
          (setq listener-table (qingeditor/cls/get wildcard-event-table event-name))
        (setq listener-table (qingeditor/hash-table/init)))
      (if (qingeditor/cls/has-key wildcard-event-table "*")
          (setq wildcard-listener-table (qingeditor/cls/get wildcard-event-table "*"))
        (setq wildcard-listener-table (qingeditor/hash-table/init)))
      (setq merged-listener-table (qingeditor/cls/merge
                                   this merged-listener-table listener-table wildcard-listener-table)))
    merged-listener-table))

(defmethod qingeditor/cls/merge
  ((this qingeditor/eventmgr/shared-mgr) &rest tables)
  "递归合并指定的事件`hash-table'。"
  (let ((target (qingeditor/hash-table/init)))
    (dolist (table tables)
      (qingeditor/cls/iterate-items
       table
       (progn
         (if (not (qingeditor/cls/has-key target key))
             (qingeditor/cls/set target key (copy-sequence value))
           (let ((source-listeners (copy-sequence value))
                 (target-listeners (qingeditor/cls/get target key)))
             (setq target-listeners (append target-listeners source-listeners))
             (qingeditor/cls/set target key target-listeners))))))
    target))

(defmethod qingeditor/cls/clear-listeners
  ((this qingeditor/eventmgr/shared-mgr) identifier &optional event-name)
  "清除指定的`identifier'的监听对象，如果`event-name'不为`nil'则只清除`event-name'下的
监听对象，否则清除`identifier'下所有的监听对象。"
  (catch 'qingeditor-eventmgr-shared-mgr-clear-listeners
    (let (event-table)
      ;; 不管什么类型，测试失败就pass
      (unless (qingeditor/cls/has-key (oref this :identifiers) identifier)
        (throw 'qingeditor-eventmgr-shared-mgr-clear-listeners nil))
      (when (null event-name)
        (qingeditor/cls/remove (oref this :identifiers) identifier)
        (throw 'qingeditor-eventmgr-shared-mgr-clear-listeners t))
      (setq event-table
            (qingeditor/cls/get (oref this :identifiers) identifier))
      (unless (qingeditor/cls/has-key event-table event-name)
        (throw 'qingeditor-eventmgr-shared-mgr-clear-listeners nil))
      (qingeditor/cls/remove event-table event-name))))

(provide 'qingeditor-eventmgr-shared-mgr)
