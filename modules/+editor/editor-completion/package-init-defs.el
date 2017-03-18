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
  (setq helm-display-function 'qingeditor/editor-completion/display-helm-window)
  (with-eval-after-load 'helm
    (qingeditor/font/hide-lighter helm-mode)
    (when (and qingeditor/config/helm-resize
               (or (eq qingeditor/config/helm-position 'bottom)
                   (eq qingeditor/config/helm-position 'top)))
      (setq helm-autoresize-min-height 10)
      (helm-autoresize-mode 1))
    ;; setup hooks
    (add-hook 'helm-minibuffer-set-up-hook
              #'qingeditor/editor-completion/helm-hide-minibuffer-maybe)
    (add-hook 'helm-before-initialize-hook #'qingeditor/editor-completion/helm-toggle-header-line)
    (add-hook
     'helm-after-initialize-hook
     'qingeditor/editor-completion/hide-cursor-in-helm-buffer)

    (add-hook 'helm-find-files-before-init-hook
              #'qingeditor/editor-completion/set-dotted-directory)
    (qingeditor/editor-completion/helm-hjkl-navigation)
    ;; setup advices
    ;; fuzzy matching for all the sources
    (unless (eq qingeditor/config/helm-use-fuzzy 'source)
      (advice-add 'helm-make-source
                  :around #'qingeditor/editor-completion/helm-make-source))
    (defadvice qingeditor/theme/notify-theme-loaded
        (after qingeditor/editor-completion/helm-header-line-adv activate)
      "Update defaults for `helm' header line whenever a new theme is loaded"
      ;; TODO factorize face definition with those defined in config.el
      (setq helm-source-header-default-foreground
            (face-attribute 'helm-source-header :foreground)
            helm-source-header-default-background
            (face-attribute 'helm-source-header :background)
            helm-source-header-default-box
            (face-attribute 'helm-source-header :box)
            helm-source-header-default-height
            (face-attribute 'helm-source-header :height)))
    ;; Transient state
    (qingeditor/editor-completion/define-helm-action-functions)
    (qingeditor/define-transient-state helm-navigation
      :title "Helm Transient State"
      :doc "
 [_j_/_k_]  next/prev condidate  [_v_]^^     persistent-action  [_e_]^^   edit occurrences
 [_h_/_l_]  prev/next source     [_1_.._0_]  action 1..10       [_t_/_T_] toggle visible/all mark
 [_q_]^^    quit                 [_a_]^^     action selection pg"
      :foreign-keys run
      :on-enter (qingeditor/editor-completion/helm-navigation-ts-on-enter)
      :on-exit (qingeditor/editor-completion/helm-navigation-ts-on-exit)
      :bindings
      ("1" qing-helm-action-1 :exit t)
      ("2" qing-helm-action-2 :exit t)
      ("3" qing-helm-action-3 :exit t)
      ("4" qing-helm-action-4 :exit t)
      ("5" qing-helm-action-5 :exit t)
      ("6" qing-helm-action-6 :exit t)
      ("7" qing-helm-action-7 :exit t)
      ("8" qing-helm-action-8 :exit t)
      ("9" qing-helm-action-9 :exit t)
      ("0" qing-helm-action-10 :exit t)
      ("<tab>" helm-select-action :exit t)
      ("TAB" helm-select-action :exit t)
      ("<RET>" helm-maybe-exit-minibuffer :exit t)
      ("a" qing-helm-transient-state-select-action)
      ("e" qing-helm-ts-edit)
      ("g" helm-beginning-of-buffer)
      ("G" helm-end-of-buffer)
      ("h" helm-previous-source)
      ("j" helm-next-line)
      ("k" helm-previous-line)
      ("l" helm-next-source)
      ("q" nil :exit t)
      ("t" helm-toggle-visible-mark)
      ("T" helm-toggle-all-marks)
      ("v" helm-execute-persistent-action))
    (define-key helm-map (kbd "M-SPC")
      'qing-helm-navigation-transient-state/body)
    ;; Swap default TAB and C-z commands
    ;; for GUI
    (with-eval-after-load 'helm-files
      ;(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
      (define-key helm-find-files-map
        (kbd "S-<tab>") 'helm-find-files-up-one-level)
      (define-key helm-find-files-map
        (kbd "<backtab>") 'helm-find-files-up-one-level)
      ;; For terminal.
      (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
      (define-key helm-find-files-map
        (kbd "S-TAB") 'helm-find-files-up-one-level)
      (define-key helm-map (kbd "C-z") 'helm-select-action))))

(defun qingeditor/editor-completion/init-default-ivy-config ()
  )

(defun qingeditor/editor-completion/init-ido ()
  )

(defun qingeditor/editor-completion/init-ido-vertical-mode ()
  )
