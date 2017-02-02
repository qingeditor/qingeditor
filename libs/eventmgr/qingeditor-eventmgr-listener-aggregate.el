;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; 抽象的`listener'类

(defmethod qingeditor/eventmgr/listener-aggregate ()
  ((listeners
    :initarg :listeners
    :initform nil
    :type list
    :documentation "`qingeditor/eventmgr/event-handler'集合。"))
  :abstract t
  :documentation "事件监听类。")

(defmethod qingeditor/cls/listener-aggregate/detach
  ((this qingeditor/eventmgr/listener-aggregate) (eventmgr qingeditor/eventmgr/mgr))
  "移除事件监听处理器。"
  (dolist (handler (oref this :listeners))
    (qingeditor/cls/detach eventmgr handler))
  (oset this :listeners nil))

(provide 'qingeditor-eventmgr-listener-aggregate)
