;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT

(require 'qingeditor-eventmgr-listener-aggregate)

(defclass qingeditor/modulemgr/install-packages-listener
  (qingeditor/eventmgr/listener-aggregate)
  ()
  :documentaion "Install packages listener aggregation.")

(defmethod qingeditor/cls/attach
  ((this qingeditor/modulemgr/install-packages-listener) eventmgr)
  "Attach listener handler for `eventmgr'."
  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/modulemgr/before-install-packages-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/cls/prepare-install-packages this)))))

(defmethod qingeditor/cls/prepare-install-packages
  ((this qingeditor/modulemgr/install-packages-listener) event)
  "We figure out packages need to be installed."
  (let* ((target-packages (qingeditor/cls/get-param event 'target-packages))
         (not-inst-count (length target-packages)))
    (qingeditor/startup-buffer/append
     (format "Found %s new package(s) to install...\n"
             not-inst-count))
    (prin1 target-packages)))

(provide 'qingeditor-modulemgr-install-packages-listener)
