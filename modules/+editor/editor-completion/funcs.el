;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; editor completion module extra function defs

(defun qing-helm-face ()
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
