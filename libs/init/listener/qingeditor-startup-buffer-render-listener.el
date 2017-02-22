;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define the startup renderer listener
(require 'qingeditor-eventmgr-listener-aggregate)
(require 'qingeditor-eventmgr-event-handler)

(defclass qingeditor/init/startup-buffer-render-listener
  (qingeditor/eventmgr/listener-aggregate)
  ()
  :documentation "The startup buffer renderer listener")

(defmethod qingeditor/cls/attach
  ((this qingeditor/init/startup-buffer-render-listener) eventmgr)
  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/init/event/render-event-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/cls/render-startup-screen this)))))

(defmethod qingeditor/cls/render-startup-screen
  ((this qingeditor/init/startup-buffer-render-listener) event)
  (qingeditor/startup-buffer/render))

(provide 'qingeditor-startup-buffer-render-listener)
