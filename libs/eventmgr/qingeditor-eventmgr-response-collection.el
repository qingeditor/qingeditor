;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;; 
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;; 
;; 封装一个事件结果集合对象

(require 'qingeditor-stack)

(defclass qingeditor/eventmgr/response-collection (qingeditor/stack)
  ((stoped
    :initarg :stopped
    :initform nil
    :type boolean
    :reader qingeditor/eventmgr/response-collection/stopped
    :writer qingeditor/eventmgr/response-collection/set-stopped
    :documentation "事件派发是否被停止。"))
  :documentation "事件派发结果栈。")

(defmethod qingeditor/eventmgr/response-collection/first
  ((this qingeditor/eventmgr/response-collection))
  (qingeditor/stack/bottom this))

(defmethod qingeditor/eventmgr/response-collection/last
  ((this qingeditor/eventmgr/response-collection))
  (when (not (qingeditor/stack/empty this))
    (qingeditor/stack/top this)))

(defmethod qingeditor/eventmgr/response-collection/contains
  ((this qingeditor/eventmgr/response-collection) value)
  (if (member value (oref this :data)) t nil))

(provide 'qingeditor-eventmgr-response-collection)


