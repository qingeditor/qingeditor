;;; qingeditor --- a distribution of Emacs editor
;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

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
(require 'qingeditor-auto-completion)
(require 'qingeditor-jump)
(require 'qingeditor-hash-table)
(require 'qingeditor-hidden-mode-line)
(require 'qingeditor-page-break-lines)
(require 'qingeditor-stddir)
(require 'qingeditor-config)
(require 'qingeditor-initializer)
(require 'qingeditor-startup-buffer)
(require 'qingeditor-modulemgr-installer)
(require 'qingeditor-modulemgr)
(require 'qingeditor-theme)
(require 'qingeditor-font)

;; setup shared eventmgr
;; we attach some important global event listeners
(defadvice server-create-window-system-frame
   (after qingeditor/advice/init-display activate)
 "After emacs server create a frame, we dispatch global event
`qingeditor/display-system-ready-event', you can attach listener
to this event if you want to do some that needs to have the display system
initialized."
 (progn
   (dolist (fn (reverse qingeditor/display-system-ready-init-list))
     (funcall fn))
   (ad-disable-advice 'server-create-window-system-frame
                      'after
                      'qingeditor/advice/init-display)
   (ad-activate 'server-create-window-system-frame)))

(qingeditor/initializer/init)
(qingeditor/initializer/bootstrap)
(qingeditor/initializer/notify-init-finished)

(provide 'qingeditor-init-bootstrap)
