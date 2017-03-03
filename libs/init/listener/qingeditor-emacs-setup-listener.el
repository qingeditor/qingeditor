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
  :documentation "The emacs setup listener")

(defmethod qingeditor/cls/attach
  ((this qingeditor/init/emacs-setup-listener) eventmgr)
  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/init/event/editor-cfg-ready-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/cls/call-cfg-ready-callback this))))

  ;; ui handler
  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/init/event/editor-cfg-ready-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/cls/setup-emacs-ui this))))

  ;; some global initialize
  (object-add-to-list
   this :listeners
   (qingeditor/cls/attach
    eventmgr
    qingeditor/init/event/editor-cfg-ready-event
    (qingeditor/eventmgr/event-handler/init
     (list #'qingeditor/cls/invoke-global-intialize-funcs this)))))

(defmethod qingeditor/cls/setup-emacs-ui
  ((this qingeditor/init/emacs-setup-listener) event)
  ;; silence ad-handle-definition about advised functions getting redefined
  (setq ad-redefinition-action 'accept)
  ;; this is for a smoother UX at startup (i.e. less graphical glitches)
  (qingeditor/hidden-mode-line-mode)
  (qingeditor/cls/removes-gui-elements this)
  ;; explicitly set the prefered coding systems to avoid annoying prompt
  ;; from emacs (especially on Microsoft Windows)
  (let ((default-theme (car qingeditor/config/themes)))
    (qingeditor/theme/load-theme default-theme)
    ;; protect used themes from delection as orphans
    (setq qingeditor/modulemgr/installer/protected-packages
          (append
           (delq nil (mapcar 'qingeditor/theme/get-theme-package
                             qingeditor/config/themes))
           qingeditor/modulemgr/installer/protected-packages))
    (setq-default qingeditor/theme/cur-theme default-theme)
    (setq-default qingeditor/theme/cycle-themes (cdr qingeditor/config/themes)))
  ;; setting fonts
  (qingeditor/do-after-display-system-ready
   ;; If you are thinking to remove this call to `message', think twice. You'll
   ;; break the life of several Spacemacser using Emacs in daemon mode. Without
   ;; this, their chosen font will not be set on the *first* instance of
   ;; emacsclient, at least if different than their system font. You don't
   ;; believe me? Go ahead, try it. After you'll have notice that this was true,
   ;; increase the counter bellow so next people will give it more confidence.
   ;; Counter = 1
   (message "(qingeditor) Setting the font...")
   (unless (qingeditor/font/set-default-font qingeditor/config/default-font)
     (qingeditor/startup-buffer/warning
      (concat "Cannot find any of the specified fonts (%s)! Font"
              "settings may not be correct.")
      (if (listp (car qingeditor/config/default-font))
          (mapconcat 'car qingeditor/config/default-font ", ")
        (car qingeditor/config/default-font)))))
  ;; set this for new version detect.
  ;; (if qingeditor/config/show-mode-line-unicode-symbols
  ;; (set-default qingeditor/upgrade/version-check-lighter "[⇪]")
  ;; )
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
    (scroll-bar-mode -1))
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  ;; tooltip in echo area
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1))
  (setq inhibit-startup-screen t)
  ;; This is set to nil during startup to allow `qingeditor' to show buffers opened
  ;; as command line arguments.
  (setq initial-buffer-choice nil)
  (unless (fboundp 'tool-bar-mode)
    (qingeditor/startup-buffer/message
     (concat "No graphical support detected, "
             "you won't be able to launch a "
             "graphical instance of Emacs "
             "with this build."))))

(defmethod qingeditor/cls/call-cfg-ready-callback
  ((this qingeditor/init/emacs-setup-listener) event)
  (qingeditor/call-func qingeditor/config/init  "Call user configuration init...")
  (qingeditor/call-func qingeditor/config/user-init "Call user configuration custom init..."))

(defmethod qingeditor/cls/invoke-global-intialize-funcs
  ((this qingeditor/init/emacs-setup-listener) event)
  (qingeditor/modulemgr/installer/initialize)
  (prefer-coding-system 'utf-8)
  )

(provide 'qingeditor-emacs-setup-listener)
