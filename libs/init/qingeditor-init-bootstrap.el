;; second stage init script
;; in this stage we instance initializer class, setup listeners and load global functions.
(require 'eieio)
(require 'package)
(require 'qingeditor-macros)
(require 'qingeditor-gvars)
(require 'qingeditor-funcs)
(require 'qingeditor-types)
(require 'qingeditor-commands)
(require 'qingeditor-key-binder)
(require 'qingeditor-toggle)
(require 'qingeditor-hash-table)
(require 'qingeditor-hidden-mode-line)
(require 'qingeditor-page-break-lines)
(require 'qingeditor-eventmgr-shared-mgr)
(require 'qingeditor-eventmgr-mgr)
(require 'qingeditor-stddir)
(require 'qingeditor-config)
(require 'qingeditor-initializer)
(require 'qingeditor-startup-buffer)
(require 'qingeditor-emacs-setup-listener)
(require 'qingeditor-startup-buffer-render-listener)
(require 'qingeditor-modulemgr-installer)
(require 'qingeditor-theme)
(require 'qingeditor-font)
(require 'qingeditor-modulemgr-default-listener-aggregate)

(defvar qingeditor/servicemgr (qingeditor/hash-table/init)
  "the global service manager.")

(defvar qingeditor/geventmgr (qingeditor/eventmgr/mgr/init)
 "`qingeditor' global event mgr, some global event trigger by this manager.")

(defvar qingeditor/shared-eventmgr (make-instance 'qingeditor/eventmgr/shared-mgr)
 "The global shared event manager object.")

(defvar qingeditor/modulemgr
  (let ((mgr (make-instance 'qingeditor/modulemgr/mgr))
        (eventmgr (qingeditor/eventmgr/mgr/init))
        (listener-aggregate (make-instance 'qingeditor/modulemgr/default-listener-aggregate)))
    (qingeditor/cls/attach listener-aggregate eventmgr)
    (qingeditor/cls/set-eventmgr mgr eventmgr)
    mgr))

;; setup shared eventmgr
;; we attach some important global event listeners
(defadvice server-create-window-system-frame
   (after qingeditor/advice/init-display activate)
 "After emacs server create a frame, we dispatch global event
`qingeditor/display-system-ready-event', you can attach listener
to this event if you want to do some that needs to have the display system
initialized."
 (progn
   (qingeditor/cls/trigger
    qingeditor/geventmgr qingeditor/display-system-ready-event)
   (ad-disable-advice 'server-create-window-system-frame
                      'after
                      'qingeditor/advice/init-display)
   (ad-activate 'server-create-window-system-frame)))

(defvar qingeditor/initializer-ref (qingeditor/initializer)
  "The global initializer object.")

;; setup default event listeners
(qingeditor/cls/add-default-listener
 qingeditor/initializer-ref "emacs-setup" (make-instance 'qingeditor/init/emacs-setup-listener))

;; setup the startup renderer listeners
(qingeditor/cls/add-default-listener
 qingeditor/initializer-ref "startup-renderer" (make-instance 'qingeditor/init/startup-buffer-render-listener))

(let ((eventmgr (qingeditor/eventmgr/mgr/init qingeditor/shared-eventmgr))
      (initializer qingeditor/initializer-ref))

  (qingeditor/cls/set-eventmgr initializer eventmgr)
  (qingeditor/cls/set-modulemgr initializer qingeditor/modulemgr)
  (qingeditor/cls/init initializer)
  (qingeditor/cls/bootstrap initializer)
  (qingeditor/cls/notify-finished-init initializer))

(provide 'qingeditor-init-bootstrap)
