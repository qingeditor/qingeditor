;; second stage init script
;; in this stage we instance initializer class, setup listeners and load global functions.
(require 'eieio)
(require 'package)
(require 'qingeditor-macros)
(require 'qingeditor-gvars)
(require 'qingeditor-funcs)
(require 'qingeditor-transient-state)
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
(require 'qingeditor-modulemgr)
(require 'qingeditor-theme)
(require 'qingeditor-font)
(require 'qingeditor-modulemgr-default-listener-aggregate)

;; setup shared eventmgr
;; we attach some important global event listeners
(defadvice server-create-window-system-frame
   (after qingeditor/advice/init-display activate)
 "After emacs server create a frame, we dispatch global event
`qingeditor/display-system-ready-event', you can attach listener
to this event if you want to do some that needs to have the display system
initialized."
 (progn
   (run-hooks 'qingeditor/display-system-ready-hook)
   (ad-disable-advice 'server-create-window-system-frame
                      'after
                      'qingeditor/advice/init-display)
   (ad-activate 'server-create-window-system-frame)))

(qingeditor/initializer/init)
(qingeditor/initializer/bootstrap)
(qingeditor/initializer/notify-init-finished)

(provide 'qingeditor-init-bootstrap)
