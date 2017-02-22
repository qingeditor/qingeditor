;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; abstract `listener' class

(defclass qingeditor/eventmgr/listener-aggregate ()
  ((listeners
    :initarg :listeners
    :initform nil
    :type list
    :documentation "`qingeditor/eventmgr/event-handler' collection"))
  :abstract t
  :documentation "The abstract listener class.")

(defmethod qingeditor/cls/detach
  ((this qingeditor/eventmgr/listener-aggregate) (eventmgr qingeditor/eventmgr/mgr))
  "Remove all listeners of event manager."
  (dolist (handler (oref this :listeners))
    (qingeditor/cls/detach eventmgr handler))
  (oset this :listeners nil))

(provide 'qingeditor-eventmgr-listener-aggregate)
