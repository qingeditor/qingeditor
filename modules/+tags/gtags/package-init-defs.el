;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qingeditor/htags/init-ggtags ()
  (use-package ggtags
    :defer t
    :init
    (progn
      ;; modes that do not have config module, add here.
      (add-hook #'awk-mode-local-vars-hook #'qingeditor/gtags/ggtags-mode-enbale)
      (add-hook #'shell-mode-local-vars-hook #'qingeditor/gtags/ggtags-mode-enbale)
      (add-hook #'tcl-mode-local-vars-hook #'qingeditor/gtags/ggtags-mode-enbale)
      (add-hook #'vhdl-mode-local-vars-hook #'qingeditor/gtags/ggtags-mode-enbale))
    :config
    (progn
      (when (qingeditor/modulemgr/package-usedp 'helm-gtags)
        ;; If anyone uses helm-gtags, they would want to use these key bindings.
        ;; These are bound in `ggtags-mode-map', since the functionality of
        ;; `helm-gtags-mode' is basically entirely contained within
        ;; `ggtags-mode-map' --- this way we don't have to enable both.
        ;; Note: all of these functions are autoloadable.
        (define-key ggtags-mode-map (kbd "M-.") #'helm-gtags-dwin)
        (define-key ggtags-mode-map (kbd "C-x 4 .") #'helm-gtags-find-tag-other-window)
        (define-key ggtags-mode-map (kbd "M-,") #'helm-gtags-pop-stack)
        (define-key ggtags-mode-map (kbd "M-*") #'helm-gtags-pop-stack)))))

(defun qingeditor/init-helm-gtags ()
  (use-package helm-gtags
    :defer t
    :init
    (progn
      (setq helm-tags-ignore-case t)
      (setq helm-tags-auto-update t)
      (setq helm-tags-use-input-at-cursor t)
      (setq helm-tags-pulse-at-cursor t)
      ;; modes that do not have a config module, define here
      (qingeditor/gtags/helm-gtags-define-keys-for-mode 'tcl-mode)
      (qingeditor/gtags/helm-gtags-define-keys-for-mode 'vhdl-mode)
      (qingeditor/gtags/helm-gtags-define-keys-for-mode 'dired-mode)
      (qingeditor/gtags/helm-gtags-define-keys-for-mode 'awk-mode)
      (qingeditor/gtags/helm-gtags-define-keys-for-mode 'compilation-mode)
      (qingeditor/gtags/helm-gtags-define-keys-for-mode 'shell-mode))))
