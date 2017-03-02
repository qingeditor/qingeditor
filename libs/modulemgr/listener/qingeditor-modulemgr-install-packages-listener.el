;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

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
     (list #'qingeditor/cls/prepare-install-packages this))))

  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/modulemgr/install-package-cycle-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/cls/install-package-handler this))))

  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/modulemgr/after-install-packages-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/cls/finish-install-packages this)))))

(defmethod qingeditor/cls/prepare-install-packages
  ((this qingeditor/modulemgr/install-packages-listener) event)
  "We figure out packages need to be installed."
  (let* ((target-packages (qingeditor/cls/get-param event 'target-packages))
         (not-inst-count (length target-packages)))
    (when (> not-inst-count 0)
      (qingeditor/startup-buffer/append
       (format "Found %s new package(s) to install...\n"
               not-inst-count))
      ;;(qingeditor/modulemgr/installer/refresh-package-archives)
      (qingeditor/cls/set-param event 'installed-count 0)
      (qingeditor/cls/set-param event 'total-install-count not-inst-count)
      (qingeditor/redisplay))))

(defmethod qingeditor/cls/install-package-handler
  ((this qingeditor/modulemgr/install-packages-listener) event)
  (let* ((installed-count (qingeditor/cls/get-param event 'installed-count))
         (total-install-count (qingeditor/cls/get-param event 'total-install-count))
         (modulemgr (qingeditor/gmodulemgr))
         (package (qingeditor/cls/get-param event 'target-package))
         (module (car (qingeditor/cls/get-owners package)))
         (package-name (qingeditor/cls/get-name package)))
    (setq installed-count (1+ installed-count))
    (qingeditor/cls/set-param event 'installed-count installed-count)
    (qingeditor/startup-buffer/replace-last-line
     (format "--> installing %s: %s%s... [%s/%s]"
             (if module "package" "dependency")
             package-name (if module (format "@%S" (qingeditor/cls/get-name module)) "")
             installed-count total-install-count) t)
    (condition-case-unless-debug err
        (qingeditor/modulemgr/installer/install-package package)
      ('error
       (qingeditor/cls/increment-error-count modulemgr)
       (qingeditor/startup-buffer/append
        (format (concat "\nAn error occurred while installing %s "
                        "(error: %s)\n") err))
       (qingeditor/redisplay)))))

(defmethod qingeditor/cls/finish-install-packages
  ((this qingeditor/modulemgr/install-packages-listener) event)
  (qingeditor/startup-buffer/append "\n"))

(provide 'qingeditor-modulemgr-install-packages-listener)
