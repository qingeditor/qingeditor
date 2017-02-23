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
    qingeditor/modulemgr/before-configure-package-cycle-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/cls/before-configure-package this))))

  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/modulemgr/configure-package-cycle-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/cls/configure-package this))))

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
  (let ((modulemgr (qingeditor/cls/get-modulemgr event)))
    (setq qingeditor/startup-buffer/loading-total-count
          (qingeditor/cls/count (oref modulemgr :used-packages)))
    (setq qingeditor/startup-buffer/loading-dots-chunk-threshold
          (max 1 (/ qingeditor/startup-buffer/loading-total-count
                    qingeditor/startup-buffer/loading-dots-count)))))

(defmethod qingeditor/cls/before-configure-package
  ((this qingeditor/modulemgr/configure-packages-listener) event)
  (let ((stage (qingeditor/cls/get-param event 'configure-stage)))
    (cond
     ((eq stage 'bootstrap)
      (qingeditor/startup-buffer/message "+ Configuring bootstrap packages..."))
     ((eq stage 'pre)
      (qingeditor/startup-buffer/message "+ Configuring pre packages..."))
     ((null stage)
      (qingeditor/startup-buffer/message "+ Configuring packages...")))
    (qingeditor/startup-buffer/loading-animation)))

(defmethod qingeditor/cls/configure-package
  ((this qingeditor/modulemgr/configure-packages-listener) event)
  "Configure package."
  )

(defmethod qingeditor/cls/after-configure-packages
  ((this qingeditor/modulemgr/configure-packages-listener) event)
  (qingeditor/startup-buffer/message "+ Configure finished"))

(provide 'qingeditor-modulemgr-configure-packages-listener)
