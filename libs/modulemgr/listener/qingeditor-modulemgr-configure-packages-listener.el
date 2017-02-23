;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The package configure listeners

(require 'qingeditor-eventmgr-listener-aggregate)

(defclass qingeditor/modulemgr/configure-packages-listener
  (qingeditor/eventmgr/listener-aggregate)
  ()
  :documentaion "configure packages listener aggregation.")

(defmethod qingeditor/cls/attach
  ((this qingeditor/modulemgr/configure-packages-listener) eventmgr)
  "Attach configure packages listener handler for `eventmgr'."
  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/modulemgr/before-configure-packages-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/cls/prepare-configure-packages this))))

  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/modulemgr/after-configure-packages-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/cls/after-configure-packages this)))))

(defmethod qingeditor/cls/prepare-configure-packages
  ((this qingeditor/modulemgr/configure-packages-listener) event)
  "prepare configure packages."
  )

(defmethod qingeditor/cls/after-configure-packages
  ((this qingeditor/modulemgr/configure-packages-listener) event)
  )

(provide 'qingeditor-modulemgr-configure-packages-listener)
