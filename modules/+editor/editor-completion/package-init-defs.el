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

(defun qingeditor/editor-completion/init-ido ()
  (setq ido-save-directory-list-file
        (concat qingeditor/cache-dir "ido.last")
        ido-enable-flex-matching t)
  (ido-mode t))

(defun qingeditor/editor-completion/init-ido-vertical-mode ()
  (use-package ido-vertical-mode
    :init
    (progn
      (ido-vertical-mode t)
      (defun qingeditor/editor-completion/ido-minibuffer-setup ()
        "Setup the minibuffer."
        ;; Since ido is implemented in a while loop where each
        ;; iteration setup a whole new minibuffer, we have to keep
        ;; track of any activated ido navigation transient-state and force
        ;; the reactivation at each iteration.
        (when qingeditor/editor-completion/ido-navigation-ms-enabled
          (qingeditor/ido-navigation-micro-state)))
      (add-hook 'ido-minibuffer-setup-hook 'qingeditor/editor-completion/ido-minibuffer-setup)

      (defvar qingeditor/editor-completion/ido-navigation-ms-enabled nil
        "Flag which is non nil when ido navigation transient-state is enabled.")

      (defvar qingeditor/editor-completion/ido-navigation-ms-face-cookie-minibuffer nil
        "Cookie pointing to the local face remapping.")

      (defface qingeditor/editor-completion/ido-navigation-ms-face
        `((t :background ,(face-attribute 'error :foreground)
             :foreground "black"
             :weight bold))
        "Face for ido minibuffer prompt when ido transient-state is activated."
        :group 'qingeditor)

      (defun qing-ido-invoke-in-new-frame ()
        "signals ido mode to create a new frame after exiting."
        (interactive)
        (setq ido-exit-minibuffer-target-window 'frame)
        (ido-exit-minibuffer))

      (defadvice ido-read-internal
          (around ido-read-internal-with-minibuffer-other-window activate)
        (let* (ido-exit-minibuffer-target-window
               (this-buffer (current-buffer))
               (result ad-do-it))
          (cond
           ((equal ido-exit-minibuffer-target-window 'other)
            (if (=1 (count-windows))
                (qing-split-window-horizontally-and-switch)
              (other-window 1)))
           ((equal ido-exit-minibuffer-target-window 'horizontal)
            (qing-split-window-horizontally-and-switch))
           ((equal ido-exit-minibuffer-target-window 'vertical)
            (qing-split-window-vertical-and-switch))
           ((equal ido-exit-minibuffer-target-window 'frame)
            (make-frame)))
          ;; why? same ido commands, such as textmate.el's
          ;; textmate-goto-symbol don't switch the current buffer
          (switch-to-buffer this-buffer)
          result))

      (defun qingeditor/editor-completion/ido-navigation-ms-set-face ()
        "Set faces for ido navigation transient-state."
        (setq qingeditor/editor-completion/ido-navigation-ms-face-cookie-minibuffer
              (face-remap-add-relative
               'minibuffer-prompt
               'qingeditor/editor-completion/ido-navigation-ms-face)))

      (defun qingeditor/editor-completion/ido-navigation-ms-on-enter ()
        "Initialization of ido transient-state."
        (setq qingeditor/editor-completion/ido-navigation-ms-enabled t)
        (qingeditor/editor-completion/ido-navigation-ms-set-face))

      (defun qingeditor/editor-completion/ido-navigation-ms-on-exit ()
        "Action to perform when exiting ido transient-state."
        (face-remap-remove-relative
         qingeditor/editor-completion/ido-navigation-ms-face-cookie-minibuffer))

      (defun qingeditor/editor-completion/ido-navigation-ms-full-doc ()
        "Full documentation for ido navigation transient-state."
        "
  [?]           display this help
  [e]           enter dired
  [j] [k]       next/previous match
  [h]           delete backward or parent directory
  [l]           select match
  [n] [p]       next/previous directory in history
  [o]           open in other window
  [s]           open in a new horizontal split
  [t]           open in other frame
  [v]           open in a new vertical split
  [q]           quit")

      (qingeditor/define-transient-state ido-navigation
        :title "ido Transient State"
        :foreign-keys run
        :on-enter (qingeditor/editor-completion/ido-navigation-ms-on-enter)
        :on-exit  (qingeditor/editor-completion/ido-navigation-ms-on-exit)
        :bindings
        ;;("?" nil (qingeditor/editor-completion/ido-navigation-ms-full-doc))
        ("<RET>" ido-exit-minibuffer :exit t)
        ("<escape>" nil :exit t)
        ("e" ido-select-text :exit t)
        ("h" ido-delete-backward-updir)
        ("j" ido-next-match)
        ("J" ido-next-match-dir)
        ("k" ido-prev-match)
        ("K" ido-prev-match-dir)
        ("l" ido-exit-minibuffer :exit t)
        ("n" ido-next-match-dir)
        ("o" qing-ido-invoke-in-other-window :exit t)
        ("p" ido-prev-match-dir)
        ("q" nil :exit t)
        ("s" qing-ido-invoke-in-vertical-split :exit t)
        ("t" qing-ido-invoke-in-new-frame :exit t)
        ("v" qing-ido-invoke-in-horizontal-split :exit t)))))
