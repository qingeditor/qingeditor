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
    :reader qingeditor/cls/stopped
    :writer qingeditor/cls/set-stopped
    :documentation "事件派发是否被停止。"))
  :documentation "事件派发结果栈。")

(defmethod qingeditor/cls/first
  ((this qingeditor/eventmgr/response-collection))
  (qingeditor/cls/bottom this))

(defmethod qingeditor/cls/last
  ((this qingeditor/eventmgr/response-collection))
  (when (not (qingeditor/cls/empty this))
    (qingeditor/cls/top this)))

(defmethod qingeditor/cls/contains
  ((this qingeditor/eventmgr/response-collection) value)
  (if (member value (oref this :data)) t nil))

(provide 'qingeditor-eventmgr-response-collection)
