;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; editor completion module extra function defs

(defun qing-helm-faces ()
  "Describe face."
  (interactive)
  (require 'helm-elisp)
  (let ((default (or (face-at-point) (thing-at-point 'symbol))))
    (helm :sources (helm-def-source--emacs-faces
                    (format "%s" (or default "default")))
          :buffer "*helm faces*")))

;; Helm Window position

(defvar qingeditor/editor-completion/helm-display-help-buffer-regex '("*.*Helm.*Help.**"))
(defvar qingeditor/editor-completion/helm-display-buffer-regex
  `("*.*helm.**"
    (display-buffer-in-side-window)
    (inhibit-same-window . t)
    (side . ,qingeditor/config/helm-position)
    (window-width . 0.6)
    (window-height . 0.4)))

(defun qingeditor/editor-completion/display-helm-window (buffer)
  "Display the Helm window respecting `qingeditor/config/helm-position'."
  (let ((display-buffer-alist
         (list qingeditor/editor-completion/helm-display-help-buffer-regex
               ;; this or any specialized case of Helm buffer must be
               ;;  added AFTER `qingeditor/editor-completion/helm-display-buffer-regexp'.
               ;; Otherwise, `qingeditor/editor-completion/helm-display-buffer-regexp' will
               ;; be used before
               ;; `qingeditor/editor-completion/helm-display-help-buffer-regexp' and display
               ;; configuration for normal Helm buffer is applied for helm
               ;; help buffer, making the help buffer unable to be
               ;; displayed.
               qingeditor/editor-completion/helm-display-buffer-regex)))
    (helm-default-display-buffer buffer)))

;; Helm Header line

(defun qingeditor/editor-completion/helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(defun qingeditor/editor-completion/helm-toggle-header-line ()
  "Hide the `helm' header if there is only one source."
  (when qingeditor/config/helm-no-header
    (if (> (length helm-sources) 1)
        (set-face-attribute
         'helm-source-header
         nil
         :foreground helm-source-header-default-foreground
         :background helm-source-header-default-background
         :box helm-source-header-default-box
         :height helm-source-header-default-height)
      (set-face-attribute
       'helm-source-header
       nil
       :foreground (face-attribute 'default :background)
       :background (face-attribute 'default :background)
       :box nil
       :height 0.1))))

(defun qingeditor/editor-completion/hide-cursor-in-helm-buffer ()
  "Hide the cursor in helm buffers."
  (with-helm-buffer
    (setq cursor-in-non-selected-windows nil)))

(defun qingeditor/editor-completion/set-dotted-directory ()
  "Set the face of directories for `.' and `..'"
  (set-face-attribute 'helm-ff-dotted-directory
                      nil
                      :foreground nil
                      :background nil
                      :inherit 'helm-ff-directory))

(defun qingeditor/editor-completion/helm-make-source (f &rest args)
  "Function to be used as advice to activate fuzzy matching for all sources."
  (let ((source-type (cadr args))
        (props (cddr args)))
    ;; fuzzy matching is not supported in async sources
    (unless (child-of-class-p source-type helm-source-async)
      (plist-put props :fuzzy-match (eq 'always qingeditor/config/helm-use-fuzzy))))
  (apply f args))

(defun qingeditor/editor-completion/helm-hjkl-navigation ()
  "Set navigation on `hjkl' for the given editor `style'."
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-h") 'helm-next-source)
  (define-key helm-map (kbd "C-S-h") 'describe-key)
  (define-key helm-map
    (kbd "C-l") 'helm-recenter-top-bottom-other-window)
  (with-eval-after-load 'helm-files
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
      (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
      ;; rebind `describe-key' for convenience
      (define-key keymap (kbd "C-S-h") 'describe-key))))

(defun qingeditor/editor-completion/define-helm-action-functions ()
  "Define qingeditor functions to pick actions."
  (dotimes (n 10)
    (let ((func (intern (format "qing-helm-action-%d" n)))
          (doc (format "Select helm action #%d" n)))
      (eval `(defun ,func ()
               ,doc
               (interactive)
               (helm-select-nth-action ,(1- n)))))))

(defun qing-helm-ts-edit ()
  "Switch in edit mode depending on the current helm buffer."
  (interactive)
  (cond
   ((string-equal "*helm-ag*" helm-buffer)
    (helm-ag-edit))))

(defun qingeditor/editor-completion/helm-navigation-ts-on-enter ()
  "Initialization of helm transient state."
  ;; face
  (qingeditor/editor-completion/helm-navigation-ts-set-face)
  (setq qingeditor/editor-completion/helm-navigation-ts-face-cookie-minibuffer
        (face-remap-add-relative
         'minibuffer-prompt
         'qingeditor/editor-completion/helm-navigation-ts-face)))

(defun qingeditor/editor-completion/helm-navigation-ts-set-face ()
  "Set the face for helm header in helm navigation transient-state."
  (with-helm-window
    (setq qingeditor/editor-completion/helm-navigation-ts-face-cookie-header
          (face-remap-add-relative
           'helm-header
           'qingeditor/editor-completion/helm-navigation-ts-face))))

(defun qingeditor/editor-completion/helm-navigation-ts-on-exit ()
  "Action to perform when exiting helm transient-state."
  (with-helm-window
    (face-remap-remove-relative
     qingeditor/editor-completion/helm-navigation-ts-face-cookie-header))
  (face-remap-remove-relative
   qingeditor/editor-completion/helm-navigation-ts-face-cookie-minibuffer))

(defun qing-helm-transient-state-select-action ()
  "Display the Helm actions page."
  (interactive)
  (call-interactively 'helm-select-action)
  (qingeditor/editor-completion/helm-navigation-ts-set-face))
