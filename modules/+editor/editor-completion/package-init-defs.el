;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; editor-completion init method defs

(defun qingeditor/editor-completion/init-default-helm-config ()
  (setq helm-prevent-escaping-from-minibuffer t)
  (setq helm-bookmark-show-location t)
  (setq helm-display-header-line nil)
  (setq helm-split-window-in-side-p t)
  (setq helm-always-two-windows t)
  (setq helm-echo-input-in-header-line t)
  (setq helm-imenu-execute-action-at-once-if-one nil)
  (setq helm-org-format-outline-path t)
  ;(setq helm-display-function 'qingeditor/editor-completion/display-helm-window)
  (with-eval-after-load 'helm
    (qingeditor/font/hide-lighter helm-mode)
    (when (and qingeditor/config/helm-resize
               (or (eq qingeditor/config/helm-position 'bottom)
                   (eq qingeditor/config/helm-position 'top)))
      (setq helm-autoresize-min-height 10)
      (helm-autoresize-mode 1))
    ;; setup hooks
    ;;(add-hook 'helm-minibuffer-set-up-hook
   ;;           'qingeditor/editor-completion/helm-hide-minibuffer-maybe)
    (add-hook 'helm-before-initialize-hook 'qingeditor/editor-completion/helm-toggle-header-line)
    )
  )

(defun qingeditor/editor-completion/init-default-ivy-config ()
  )

(defun qingeditor/editor-completion/init-ido ()
  )

(defun qingeditor/editor-completion/init-ido-vertical-mode ()
  )
