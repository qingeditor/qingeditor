;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define global initializer class
(require 'eieio-base)
(require 'qingeditor-stddir)
(require 'qingeditor-init-event)
(require 'qingeditor-eventmgr-mgr)
(require 'qingeditor-hash-table)
(require 'qingeditor-modulemgr-mgr)

(defvar qingeditor/initializer/error-count nil
  "The number of error during the `qingeditor' startup.")

(defvar qingeditor/initializer/editor-config-ready-hook nil
  "when `qingeditor' load configuration file, run this hook.")

(defvar qingeditor/initializer/bootstrap-hook nil
  "after load configuration, run this hook.")

(defvar qingeditor/initializer/before-load-modules-hook nil
  "before `qingeditor' load modules, run this hook.")

(defvar qingeditor/initializer/adter-load-modules-hook nil
  "when all configured modules loaded, run this hook.")

(defvar qingeditor/initializer/render-hook nil
  "before loaded target modules, `qingeditor' render starup screen. but
if you invoke `qingeditor' from terminal with a file path, this hook will not
been run.")

(defclass qingeditor/initializer ()
  ((default-listeners
     :initarg :default-listeners
     :initform (qingeditor/hash-table/init)
     :type qingeditor/hash-table
     :documentation "default event listeners")

   (eventmgr
     :initarg :eventmgr
     :initform nil
     :type (satisfies (lambda (obj)
                        (or (null obj)
                            (object-of-class-p obj qingeditor/eventmgr/mgr))))
     :documentation "The event manager of `qingeditor/initializer' object")

   (modulemgr
    :initarg :modulemgr
    :initform nil
    :type (satisfies (lambda (obj)
                       (or (null obj)
                           (object-of-class-p obj qingeditor/modulemgr/mgr))))
    :documentation "The modulemgr of `qingeditor/initializer' object")

   (error-count
    :initarg :error-count
    :initform 0
    :type number
    :documentation "The number of error during the `qingeditor' startup.")

   (event
    :initarg :event
    :initform nil
    :type (satisfies (lambda (event)
                       (or (null event)
                           (object-of-class-p event qingeditor/init/event))))
    :documentation "Init event reference.")
   )
  :documentation "global initializer class")

(defun qingeditor/initializer/init ()
  "init `qingeditor' in this method, we first find the configuration
file in load-path, if the configuration not exist, `qingeditor' will
first generate it, then load it normally. after load the configuration file,
we finally process `qingeditor' modules."
  (qingeditor/initializer/load-editor-cfg-file)
  (qingeditor/initializer/setup-emacs-ui)
  (qingeditor/modulemgr/installer/initialize)
  (prefer-coding-system 'utf-8))

(defun qingeditor/initializer/bootstrap ()
  "bootstrap `qingeditor', run some hooks."
  (run-hooks 'qingeditor/initializer/bootstrap-hook)
  (qingeditor/initializer/render-startup-screen)
  (qingeditor/modulemgr/initialize)
  (qingeditor/modulemgr/process))

(defun qingeditor/initializer/notify-init-finished ()
  "Do something to notify the qingeditor finish init."
  (add-hook
   'emacs-startup-hook
   (lambda ()
     ;; This is set here so that emacsclient will show the startup buffer (and
     ;; so that it can be changed in user-config if necessary). It was set to
     ;; nil earlier in the startup process to properly handle command line
     ;; arguments.
     (setq initial-buffer-choice (lambda () (get-buffer qingeditor/startup-buffer/name)))
     (qingeditor/call-func qingeditor/config/user-config-setup "Call user configuration setup func...")
     (run-hooks 'qingeditor/user-config-setup-finished-hook)
     (setq qingeditor/user-config-setup-hook-invoked t)
     (when (fboundp qingeditor/config/scratch-mode)
       (with-current-buffer "*scratch*"
         (funcall qingeditor/config/scratch-mode)))
     (setq-default qingeditor/initialized t)
     (run-hooks 'qingeditor/editor-ready-hook)
     (qingeditor/display-startup-echo-area-message))))

(defun qingeditor/initializer/setup-emacs-ui ()
  ;; silence ad-handle-definition about advised functions getting redefined
  (setq ad-redefinition-action 'accept)
  ;; this is for a smoother UX at startup (i.e. less graphical glitches)
  (qingeditor/hidden-mode-line-mode)
  (qingeditor/initializer/removes-gui-elements)
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

(defmethod qingeditor/initializer/removes-gui-elements ()
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

(defun qingeditor/initializer/load-editor-cfg-file ()
  "load `qingeditor' configuration file."
  (unless (file-exists-p qingeditor/config/target-cfg-filename)
    (qingeditor/initializer/generate-new-cfg-filename-from-tpl 'with-wizard))
  (when (load-file qingeditor/config/target-cfg-filename)
    (qingeditor/call-func qingeditor/config/init  "Call user configuration init...")
    (qingeditor/call-func qingeditor/config/user-init "Call user configuration custom init...")
    (run-hooks 'qingeditor/initializer/editor-config-ready-hook)))

(defun qingeditor/initializer/render-startup-screen ()
  )

(defun qingeditor/initializer/generate-new-cfg-filename-from-tpl (&optional arg)
  "Generate a new configuration file for `qingeditor'."
  ;; preferences is alist where the key is the text to replace
  ;; the value in the configuration template file
  (let ((preferences
         (when arg
           `(("distribution 'editor-standard"
              ,(format
                "distribution '%S"
                (qingeditor/initializer/ido-completing-read
                 "What distribution of qingeditor would you like to start with?"
                 `(("The standard distribution, recommended (editor-standard)"
                    editor-standard)
                   (,(concat "A minimalist distribution that you can build on"
                             " (editor-base)")
                    editor-base)))))
             ("helm"
              ,(qingeditor/initializer/ido-completing-read
                "What type of completion framework di you want? "
                '(("A heavy one but full featured (helm)"
                   "helm")
                  ("A lighter one but still powerfull (ivy)"
                   "ivy")
                  ;; for now, None works only if the user selected
                  ;; the `editor-base' distribution
                  ("None (not recommended)" ""))))))))
    (with-current-buffer (find-file-noselect
                          (concat qingeditor/template-dir ".qingeditor.template"))
      (dolist (p preferences)
        (goto-char (point-min))
        (re-search-forward (car p))
        (replace-match (cadr p)))
      (let ((install
             (if (file-exists-p qingeditor/config/target-cfg-filename)
                 (y-or-n-p
                  (format "%s already exists. Do you want to overwrite it ? "
                          qingeditor/config/target-cfg-filename))
               t)))
        (when install
          (write-file qingeditor/config/target-cfg-filename)
          (message "%s has been generate success."
                   qingeditor/config/target-cfg-filename)
          t)))))

(defun qingeditor/initializer/ido-completing-read (prompt candidates)
  "Call `ido-completing-read' with a CANDIDATES alist where the key is
a display string and the value is the actual to return."
  (let ((ido-max-window-height (1+ (length candidates))))
    (cadr (assoc (ido-completing-read prompt (mapcar 'car candidates)) candidates))))

(defmethod qingeditor/cls/increase-error-count ()
  "Increase start up error count."
  (setq qingeditor/initializer/error-count
        (1+ qingeditor/initializer/error-count)))

(provide 'qingeditor-initializer)
