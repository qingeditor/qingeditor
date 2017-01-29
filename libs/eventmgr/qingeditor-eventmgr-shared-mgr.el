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

(defclass qingeditor/eventmgr/shared-mgr ()
  ((identifiers
    :initarg :identifiers
    :initform  (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "事件数据存储的地方。")
   )
  :documentation "让需要事件派发能力的对象不需要实例化一个事件管理器对象就能派发事件。")

(defmethod qingeditor/eventmgr/shared-mgr/attach
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
  (let (identifier-table
	event-table
	(listener-index (format "%d.0" priority))
	listener-list)
    (when (not (qingeditor/hash-table/has-key (oref this :identifiers) identifier))
      (setq identifier-table (qingeditor/hash-table/init))
      (qingeditor/hash-table/set (oref this :identifiers) identifier identifier-table))
    (unless identifier-table
      (setq identifier-table (qingeditor/hash-table/get (oref this :identifiers) identifier)))
    (when (not (qingeditor/hash-table/has-key identifier-table event))
      (setq event-table (qingeditor/hash-table/init))
      (qingeditor/hash-table/set identifier-table event event-table))
    (unless event-table
      (setq event-table (qingeditor/hash-table/get identifier-table event)))
    (when (not (qingeditor/hash-table/has-key event-table listener-index))
      (setq listener-list nil)
      (qingeditor/hash-table/set event-table listener-index listener-list))
    (unless listener-list
      (setq listener-list (qingeditor/hash-table/get event-table listener-index)))
    (push listener listener-list)
    (qingeditor/hash-table/set event-table listener-index listener-list)))

(provide 'qingeditor-eventmgr-shared-mgr)
