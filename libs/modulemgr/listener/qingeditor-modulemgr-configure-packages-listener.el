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
    (setq qingeditor/startup-buffer/loading-dots-chunk-size
          (/ qingeditor/startup-buffer/loading-dots-count
             qingeditor/startup-buffer/loading-total-count))))

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
  (let* ((package (qingeditor/cls/get-param event 'target-package))
         (package-name (qingeditor/cls/get-name package))
         (modulemgr (qingeditor/cls/get-modulemgr event)))
    (cond
     ((oref package :lazy-install)
      (qingeditor/startup-buffer/message
       (format "%S ignored since it can be lazily installed." package-name)))
     ((and (oref package :excluded)
           (not (oref package :protected)))
      (qingeditor/startup-buffer/message
       (format "%S ignored since it has been excluded." package-name)))
     ((null (qingeditor/cls/get-owners package))
      (qingeditor/startup-buffer/message
       (format "%S ignored since it has no owner module." package-name)))
     ((not (qingeditor/cls/enabledp package t))
      (qingeditor/startup-buffer/message (format "%S is toggled off." package-name)))
     (t
      ;; ok we begin configure package
      (let ((location (qingeditor/cls/get-location package)))
        (cond
         ((stringp location)
          (if (file-directory-p location)
              (push (file-name-as-directory location) load-path)
            (qingeditor/cls/warning
             modulemgr "Location path for package %S does not exists (value: %s)."
             package-name location)))
         ((and (eq 'local location)
               (eq 'config (qingeditor/cls/get-from-source package)))
          (push (file-name-as-directory
                 (concat qingeditor/modulemgr/module-private-directory "local/"
                         (symbol-name package-name)))
                load-path))
         ((eq 'local location)
          (let* ((owner (car (qingeditor/cls/get-owners package)))
                 (dir (when owner (qingeditor/cls/get-module-dir owner))))
            (push (format "%slocal/%S/" dir package-name) load-path)))))
      ;; configuration
      (unless (memq (qingeditor/cls/get-location package) '(local site built-in))
        (qingeditor/modulemgr/installer/activate-package package-name))
      (cond
       ((eq 'config (qingeditor/cls/get-from-source package))
        (qingeditor/startup-buffer/message
         (format "%S is configured in the config file." package-name)))
       (t
        (qingeditor/cls/do-configure-package this package modulemgr)))))))

(defmethod qingeditor/cls/do-configure-package
  ((this qingeditor/modulemgr/configure-packages-listener) package modulemgr)
  "Do actually configure `package'."
  (let* ((package-name (qingeditor/cls/get-name package))
         (owner (car (qingeditor/cls/get-owners package))))
    (qingeditor/startup-buffer/message
     (format "> Configuring %S..." package-name))
    ;; pre-init
    (mapc
     (lambda (module-name)
       (when (qingeditor/cls/module-usedp modulemgr module-name)
         (let ((module (qingeditor/cls/get (oref modulemgr :used-modules) module-name))
               pre-init-method)
           (if (not (qingeditor/cls/package-enabled-p package module-name))
               (qingeditor/startup-buffer/message
                (format " -> ignore pre-init (%S)..." module-name))
             (qingeditor/startup-buffer/message
              (format " -> pre-init (%S)..." module-name))
             (condition-case-unless-debug err
                 (funcall (intern (format "qingeditor/cls/pre-init-%S" package-name)) module)
               ('error
                (qingeditor/cls/increment-error-count modulemgr)
                (qingeditor/startup-buffer/append
                 (format
                  (concat "\nAn error occurred while pre-configuring %S "
                          "in module %S (error: %s)\n"
                          package-name module-name err)))))))))
     (qingeditor/cls/get-pre-init-modules package))

    ;; init
    (when (qingeditor/cls/has-init-for-package owner package-name)
      (qingeditor/startup-buffer/message (format " -> init (%S)..." (qingeditor/cls/get-name owner)))
      (funcall (intern (format "qingeditor/cls/init-%S" package-name)) owner))

    ;; post init
    (mapc
     (lambda (module-name)
       (when (qingeditor/cls/module-usedp modulemgr module-name)
         (let ((module (qingeditor/cls/get (oref modulemgr :used-modules) module-name))
               pre-init-method)
           (if (not (qingeditor/cls/package-enabled-p package module-name))
               (qingeditor/startup-buffer/message
                (format " -> ignore post-init (%S)..." module-name))
             (qingeditor/startup-buffer/message
              (format " -> post-init (%S)..." module-name))
             (condition-case-unless-debug err
                 (funcall (intern (format "qingeditor/cls/post-init-%S" package-name)) module)
               ('error
                (qingeditor/cls/increment-error-count modulemgr)
                (qingeditor/startup-buffer/append
                 (format
                  (concat "\nAn error occurred while post-configuring %S "
                          "in module %S (error: %s)\n"
                          package-name module-name err)))))))))
     (qingeditor/cls/get-pre-init-modules package))))

(defmethod qingeditor/cls/after-configure-packages
  ((this qingeditor/modulemgr/configure-packages-listener) event)
  (qingeditor/startup-buffer/message "> Configure finished"))

(provide 'qingeditor-modulemgr-configure-packages-listener)
