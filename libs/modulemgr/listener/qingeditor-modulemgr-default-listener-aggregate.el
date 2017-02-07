;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The default module manager aggregate listener

(require 'qingeditor-eventmgr-listener-aggregate)
(require 'qingeditor-modulemgr-module-resolve-listener)

(defclass qingeditor/modulemgr/default-listener-aggregate (qingeditor/eventmgr/listener-aggregate)
  ()
  :documentaion "The default module manager aggregate, set listeners for the
event of the module manager.")

(defmethod qingeditor/cls/attach
  ((this qingeditor/modulemgr/default-listener-aggregate) eventmgr)
  "Attach listener handler for `eventmgr'."
  (qingeditor/cls/attach
   eventmgr
   qingeditor/modulemgr/load-module-resolve-event
   (qingeditor/eventmgr/event-handler/init
    (list #'qingeditor/modulemgr/module-resolve)
    qingeditor/eventmgr/normal-callable))
  )

(provide 'qingeditor-modulemgr-default-listener-aggregate)
