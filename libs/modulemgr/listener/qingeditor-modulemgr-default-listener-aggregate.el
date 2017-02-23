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
(require 'qingeditor-modulemgr-feature-listeners)
(require 'qingeditor-modulemgr-builtin-feature-listeners)
(require 'qingeditor-modulemgr-resolve-listener)
(require 'qingeditor-modulemgr-install-packages-listener)

(defclass qingeditor/modulemgr/default-listener-aggregate
  (qingeditor/eventmgr/listener-aggregate)
  ()
  :documentaion "The default module manager aggregate, set listeners for the
event of the module manager.")

(defmethod qingeditor/cls/attach
  ((this qingeditor/modulemgr/default-listener-aggregate) eventmgr)
  "Attach listener handler for `eventmgr'."

  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/modulemgr/load-module-resolve-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/modulemgr/module-resolve))))

  ;; bind before load module
  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/modulemgr/before-load-module-cycle-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/modulemgr/extra-func-defs-handler))))

  ;; bind before load module
  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/modulemgr/before-load-module-cycle-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/modulemgr/extra-files-loader-handler))))

  ;; keypmap setter
  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/modulemgr/after-load-module-cycle-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/modulemgr/keymap-provider-handler))))

  ;; extra module config setup
  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/modulemgr/after-load-module-cycle-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/modulemgr/keymap-provider-handler))))

  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/modulemgr/before-load-module-cycle-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/modulemgr/loadpath-provider-handler))))

  ;; service object register
  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/modulemgr/after-load-module-cycle-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/modulemgr/service-provider-handler))))

  ;; dependency indicator
  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/modulemgr/after-load-module-cycle-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/modulemgr/dependency-indicator-handler))))

  (let (install-packages-listener)
    (setq install-packages-listener
          (make-instance 'qingeditor/modulemgr/install-packages-listener))
    (qingeditor/cls/attach install-packages-listener eventmgr)
    (object-add-to-list this :listeners install-packages-listener)))

(provide 'qingeditor-modulemgr-default-listener-aggregate)
