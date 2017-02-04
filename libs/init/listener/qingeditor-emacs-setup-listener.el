;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The emacs setup listener, The duty of this listener
;; is setup `emacs' UI elements, setup some global variable
;; and load `emacs' theme.

(require 'qingeditor-eventmgr-listener-aggregate)
(require 'qingeditor-eventmgr-event-handler)

(defclass qingeditor/init/emacs-setup-listener
  (qingeditor/eventmgr/listener-aggregate)
  ()
  :documentation "The emacs setup listene")

(defmethod qingeditor/cls/attach
  ((this qingeditor/init/emacs-setup-listener) eventmgr)
  ;; ui handler
  (qingeditor/cls/attach
   eventmgr
   qingeditor/init/event/editor-cfg-ready-event
   (qingeditor/eventmgr/event-handler/init
    (list #'qingeditor/cls/setup-emacs-ui this)))
  
  (qingeditor/cls/attach
   eventmgr
   qingeditor/init/event/editor-cfg-ready-event
   (qingeditor/eventmgr/event-handler/init
    (list #'qingeditor/cls/call-cfg-ready-callback this)))
  ;; some global initialize
  (qingeditor/cls/attach
   eventmgr
   qingeditor/init/event/editor-cfg-ready-event
   (qingeditor/eventmgr/event-handler/init
    (list #'qingeditor/cls/invoke-global-intialize-funcs this)))
  ;; load theme handler
  (qingeditor/cls/attach
   eventmgr
   qingeditor/init/event/editor-cfg-ready-event
   (qingeditor/eventmgr/event-handler/init
    (list #'qingeditor/cls/load-default-theme this)))
  )

(defmethod qingeditor/cls/setup-emacs-ui
  ((this qingeditor/init/emacs-setup-listener) event)
  ;; silence ad-handle-definition about advised functions getting redefined
  (setq ad-redefinition-action 'accept)
  ;; this is for a smoother UX at startup (i.e. less graphical glitches)
  (qingeditor/hidden-mode-line-mode)
  (qingeditor/cls/removes-gui-elements this)
  ;; explicitly set the prefered coding systems to avoid annoying prompt
  ;; from emacs (especially on Microsoft Windows)
  )

(defmethod qingeditor/cls/removes-gui-elements
  ((this qingeditor/init/emacs-setup-listener))
  "Remove the menu bar, tool bar and scroll bars."
  ;; removes the GUI elements
  (unless (qingeditor/window-system-is-mac)
    (when (and (fboundp 'menu-bar-mode)
               (not (eq menu-bar-mode -1)))
      (menu-bar-mode -1)))
  (when (and (fboundp 'scroll-bar-mode)
             (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1)))

(defmethod qingeditor/cls/call-cfg-ready-callback
  ((this qingeditor/init/emacs-setup-listener) event)
  (qingeditor/call-func qingeditor/config/init
                        "Call user configuration init...")
  (qingeditor/call-func qingeditor/config/user-init
                        "Call user configuration custom init..."))

(defmethod qingeditor/cls/invoke-global-intialize-funcs
  ((this qingeditor/init/emacs-setup-listener) event)
  (qingeditor/modulemgr/installer/initialize))

(defmethod qingeditor/cls/load-default-theme
  ((this qingeditor/init/emacs-setup-listener) event)
  )

(provide 'qingeditor-emacs-setup-listener)
