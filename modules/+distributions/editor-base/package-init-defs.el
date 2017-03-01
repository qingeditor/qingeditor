;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; editor-base init method defs

(defmethod qingeditor/cls/init-abbrev ((this qingeditor/module/editor-base))
  "init abbrev."
  (qingeditor/font/hide-lighter abbrev-mode))

(defmethod qingeditor/cls/init-ace-window ((this qingeditor/module/editor-base))
  "init ace window."
  (use-package ace-window
    :defer t
    :init
    (progn
      (qingeditor/key-binder/set-leader-keys
        "bD" 'spacemacs/ace-kill-this-buffer
        ;; FIXME: Needs new binding.
        ;; "wC" 'spacemacs/ace-center-window
        "wD" 'spacemacs/ace-delete-window
        "wM" 'ace-swap-window
        "wW" 'ace-window)
      ;; set ace-window keys to home-row
      (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))))

(defmethod qingeditor/cls/init-archive-mode ((this qingeditor/module/editor-base))
  "init archive mode."
  (use-package archive-mode
    :commands archive-mode))

(defmethod qingeditor/cls/init-bookmark ((this qingeditor/module/editor-base))
  (use-package bookmark
    :defer t
    :init
    (progn
      (setq bookmark-default-file (concat qingeditor/cache-dir "bookmarks"))
      ;; autosave each change
      (setq bookmark-save-flag 1)
      (qingeditor/key-binder/set-leader-keys "fb" 'bookmark-jump))))

(defmethod qingeditor/cls/init-conf-mode ((this qingeditor/module/editor-base))
  ;; explicitly derive conf-mode from text-mode major-mode
  (add-hook 'conf-mode-hook 'qingeditor/run-text-mode-hooks))

(defmethod qingeditor/cls/init-dired ((this qingeditor/module/editor-base))
  (qingeditor/key-binder/set-leader-keys
   "ad" 'dired
   "fj" 'dired-jump
   "jd" 'dired-jump
   "jD" 'dired-jump-other-window))

(defmethod qingeditor/cls/init-dired-x ((this qingeditor/module/editor-base))
  (use-package dired-x
    :commands (dired-jump
               dired-jump-other-window
               dired-omit-mode)))

(defmethod qingeditor/cls/init-electric-indent-mode ((this qingeditor/module/editor-base))
  (electric-indent-mode))

(defmethod qingeditor/cls/init-visual-line-mode ((this qingeditor/module/editor-base))
  (qingeditor/font/diminish visual-line-mode " Ⓛ" " L"))

(defmethod qingeditor/cls/init-ediff ((this qingeditor/module/editor-base))
  (use-package ediff
    :defer t
    :init
    (progn
      ;; first we set some sane defaults
      (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
      (setq-default ediff-split-window-function 'split-window-horizontally)
      (setq-default ediff-merge-split-window-function 'split-window-horizontally)
      ;; show org ediffs ufolded
      (require 'outline)
      (add-hook 'ediff-prepare-buffer-hook #'show-all)
      ;; restore window layout then done
      (add-hook 'ediff-quit-hook #'winner-undo))))

(defmethod qingeditor/cls/init-eldoc ((this qingeditor/module/editor-base))
  (use-package eldoc
    :defer t
    :config
    (progn
      ;; enable eldoc in `eval-expression'
      (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
      ;; enable eldoc in IELM
      (add-hook 'ielm-mode-hook #'eldoc-mode)
      ;; don't display eldoc on modeline
      (qingeditor/font/hide-lighter eldoc-mode))))

(defmethod qingeditor/cls/init-exec-path-from-shell ((this qingeditor/module/editor-base))
  (use-package exec-path-from-shell
    :init (when (or (qingeditor/system-is-mac)
                    (qingeditor/system-is-linux))
            (exec-path-from-shell-initialize))))

(defmethod qingeditor/cls/init-help-fns+ ((this qingeditor/module/editor-base))
  (use-package help-fns+
    :commands (describe-keymap)
    :init
    (qingeditor/key-binder/set-leader-keys
      "hdK" 'describe-keymap)))

(defmethod qingeditor/cls/init-hi-lock ((this qingeditor/module/editor-base))
  (with-eval-after-load 'hi-lock
    (qingeditor/font/hide-lighter hi-lock-mode)))

(defmethod qingeditor/cls/init-image-mode ((this qingeditor/module/editor-base))
  (use-package image-mode
    :defer t))

(defmethod qingeditor/cls/init-imenu ((this qingeditor/module/editor-base))
  (use-package imenu
    :defer t
    :init (qingeditor/key-binder/set-leader-keys
            "ji" 'imenu)))

(defmethod qingeditor/cls/init-linum ((this qingeditor/module/editor-base))
  (when qingeditor/config/line-numbers
    (add-hook 'prog-mode-hook 'linum-mode)
    (add-hook 'text-mode-hook 'linum-mode))
  (setq linum-format "%4d")
  (qingeditor/toggle/add-toggle line-numbers
    :mode linum-mode
    :documentation "Show the line numbers."))

(defmethod qingeditor/cls/init-occur-mode ((this qingeditor/module/editor-base))
  (use-package occur-mode
    :defer t))

(defmethod qingeditor/cls/init-package-menu ((this qingeditor/module/editor-base))
  (use-package package-menu-mode
    :defer t))

(defmethod qingeditor/cls/init-qingeditor/page-break-lines ((this qingeditor/module/editor-base))
  (require 'qingeditor-page-break-lines)
  (qingeditor/global-page-break-lines-mode t)
  (qingeditor/font/hide-lighter qingeditor/page-break-lines-mode))

(defmethod qingeditor/cls/init-pcre2el ((this qingeditor/module/editor-base))
  (use-package pcre2el
    :defer t
    :init
    (progn
      (qingeditor/key-binder/declare-prefix "xr" "regular expressions")
      (qingeditor/key-binder/declare-prefix "xre" "elisp")
      (qingeditor/key-binder/declare-prefix "xrp" "pcre")
      (qingeditor/key-binder/set-leader-keys
        "xr/"    'rxt-explain
        "xr'"    'rxt-convert-to-strings
        "xrt"    'rxt-toggle-elisp-rx
        "xrx"    'rxt-convert-to-rx
        "xrc"    'rxt-convert-syntax
        "xre/"   'rxt-explain-elisp
        "xre'"   'rxt-elisp-to-strings
        "xrep"   'rxt-elisp-to-pcre
        "xret"   'rxt-toggle-elisp-rx
        "xrex"   'rxt-elisp-to-rx
        "xrp/"   'rxt-explain-pcre
        "xrp'"   'rxt-pcre-to-strings
        "xrpe"   'rxt-pcre-to-elisp
        "xrpx"   'rxt-pcre-to-rx))))

(defmethod qingeditor/cls/init-process-menu ((this qingeditor/module/editor-base))
  (use-package process-menu-mode
    :defer t))

(defmethod qingeditor/cls/init-projectile ((this qingeditor/module/editor-base))
  (use-package projectile
    :commands (projectile-ack
               projectile-ag
               projectile-compile-project
               projectile-dired
               projectile-find-dir
               projectile-find-file
               projectile-find-tag
               projectile-test-project
               projectile-grep
               projectile-invalidate-cache
               projectile-kill-buffers
               projectile-multi-occur
               projectile-project-p
               projectile-project-root
               projectile-recentf
               projectile-regenerate-tags
               projectile-replace
               projectile-replace-regexp
               projectile-run-async-shell-command-in-root
               projectile-run-shell-command-in-root
               projectile-switch-project
               projectile-switch-to-buffer
               projectile-vc)
    :init
    (progn
      ;; note for Windows: GNU find or Cygwin find must be in path to enabled
      ;; fater indexing
      (when (and (qingeditor/system-is-mswindows) (executable-find "find"))
        (setq projectile-indexing-method 'alien)
        (setq projectile-generic-command "find . -type f"))
      (setq projectile-sort-order 'recentf)
      (setq projectile-cache-file (concat qingeditor/cache-dir "projectile.cache"))
      (setq projectile-known-projects-file (concat qingeditor/cache-dir
                                                   "projectile-bookmarks.eld"))
      (qingeditor/key-binder/set-leader-keys
        "pb" 'projectile-switch-to-buffer
        "pd" 'projectile-find-dir
        "pf" 'projectile-find-file
        "pF" 'projectile-find-file-dwin
        "ph" 'helm-projectile
        "pr" 'projectile-recentf
        "pp" 'projectile-switch-project
        "pv" 'projectile-vc)

      (qingeditor/key-binder/set-leader-keys
        "p!"     'projectile-run-shell-command-in-root
        "p&"     'projectile-run-async-shell-command-in-root
        "p%"     'projectile-replace-regexp
        "pa"     'projectile-toggle-between-implementation-and-test
        "pc"     'projectile-compile-project
        "pD"     'projectile-dired
        "pg"     'projectile-find-tag
        "p C-g"  'projectile-regenerate-tags
        "pI"     'projectile-invalidate-cache
        "pk"     'projectile-kill-buffers
        "pR"     'projectile-replace
        "pT"     'projectile-test-project))
    :config
    (progn
      (projectile-global-mode)
      (qingeditor/font/hide-lighter projectile-mode))))

(defmethod qingeditor/cls/init-recentf ((this qingeditor/module/editor-base))
  (use-package recentf
    :defer t
    :init
    (progn
      ;; lazy load recentf
      (add-hook 'find-file-hook (lambda ()
                                  (unless recentf-mode
                                    (recentf-mode)
                                    (recentf-track-opened-file))))
      (setq recentf-save-file (concat qingeditor/cache-dir "recentf"))
      (setq recentf-max-saved-items 1000)
      (setq recentf-auto-cleanup 'nerver)
      (setq recentf-auto-save-timer
            (run-with-idle-timer 600 t 'recentf-save-list)))
    :config
    (progn
      (add-to-list 'recentf-exclude
                   (expand-file-name qingeditor/cache-dir))
      (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
      (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))))

(defmethod qingeditor/cls/init-savehist ((this qingeditor/module/editor-base))
  (use-package savehist
    :init
    (progn
      ;; Minibuffer history
      (setq savehist-file (concat qingeditor/cache-dir "savehist"))
      (setq enable-recursive-minibuffers t)
      (setq history-length 1000)
      (setq savehist-additional-variables '(mark-ring
                                            global-mark-ring
                                            search-ring
                                            regexp-search-ring
                                            extended-command-history))
      (setq savehist-autosave-interval 60)
      (savehist-mode t))))

(defmethod qingeditor/cls/init-saveplace ((this qingeditor/module/editor-base))
  (use-package saveplace
    :init
    (progn
      (if (fboundp 'save-place-mode)
          ;; Emacs 25 has proper mode for `save-place'
          (save-place-mode)
        (setq save-place t))
      ;; Save point position between sessions
      (setq save-place-file (concat qingeditor/cache-dir "places")))))

(defmethod qingeditor/cls/init-spacemacs-theme ((this qingeditor/module/editor-base))
  (use-package spacemacs-theme
    :defer t
    :init
    (progn
      (setq spacemacs-theme-comment-bg t)
      (setq spacemacs-theme-org-height t))))

(defmethod qingeditor/cls/init-subword ((this qingeditor/module/editor-base))
  (use-package subword
    :defer t
    :init
    (progn
      (unless (category-docstring ?U)
        (define-category ?U "Uppercase")
        (define-category ?u "Lowercase"))
      (modify-category-entry (cons ?A ?Z) ?U)
      (modify-category-entry (cons ?a ?z) ?u)
      (make-variable-frame-local 'qingeditor/cjk/word-separating-categories)
      (defun qingeditor/subword-enable-camel-case ()
        "Add support for camel case to subword."
        (if subword-mode
            (push '(?u . ?U) qingeditor/cjk/word-separating-categories)
          (setq qingeditor/cjk/word-separating-categories
                (default-value 'qingeditor/cjk/word-separating-categories))))
      (add-hook 'subword-mode-hook 'qingeditor/subword-enable-camel-case)
      (qingeditor/toggle/add-toggle camel-case-motion
        :mode subword-mode
        :documentation "Toggle CamelCase motions.")
      (qingeditor/toggle/add-toggle camel-case-motion-globally
        :mode global-subword-mode
        :documentation "Globally toggle CamelCase motions."))
    :config
    (qingeditor/font/diminish subword-mode " ⓒ" " c")))

(defmethod qingeditor/cls/init-tar-mode ((this qingeditor/module/editor-base))
  (use-package tar-mode
    :defer t))

(defmethod qingeditor/cls/init-uniquify ((this qingeditor/module/editor-base))
  (require 'uniquify)
  ;; When having windows with repeated filenames, uniquify them
  ;; by the folder they are in rather those annoying <2>,<3>,.. etc
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  ;; don't screw special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(defmethod qingeditor/cls/init-url ((this qingeditor/module/editor-base))
  ;; gravatars from magit use this to store their cache
  (setq url-configuration-directory (concat qingeditor/cache-dir "url/")))

(defmethod qingeditor/cls/init-whitespace ((this qingeditor/module/editor-base))
  (use-package whitespace
    :defer t
    :init
    (progn
      (setq qingeditor/editor-base/show-trailing-whitespace t)
      (defun qingeditor/editor-base/show-trailing-whitespace ()
        (when qingeditor/editor-base/show-trailing-whitespace
          (set-face-attribute 'trailing-whitespace nil
                              :background
                              (face-attribute 'font-lock-comment-face
                                              :foreground))
          (setq show-trailing-whitespace 1)))
      (add-hook 'prog-mode-hook 'qingeditor/editor-base/show-trailing-whitespace)

      (qingeditor/toggle/add-toggle whitespace
        :mode whitespace-mode
        :documentation "Display whitespace.")
      (qingeditor/toggle/add-toggle whitespace-globally
        :mode global-whitespace-mode
        :documentation "Display whitespace globally.")

      (defun qingeditor/editor-base/set-whitespace-style-for-diff ()
        "Whitespace configuration for `diff-mode'."
        (setq-local whitespace-style '(face
                                       tabs
                                       tab-mark
                                       spaces
                                       space-mark
                                       trailing
                                       indentation::space
                                       indentation::tab
                                       newline
                                       newline-mark)))
      (add-hook 'diff-mode-hook 'whitespace-mode)
      (add-hook 'diff-mode-hook 'qingeditor/editor-base/set-whitespace-style-for-diff))
    :config
    (progn
      (set-face-attribute 'whitespace-space nil
                          :background nil
                          :foreground (face-attribute 'font-lock-warning-face
                                                      :foreground))
      (set-face-attribute 'whitespace-tab nil
                          :background nil)
      (set-face-attribute 'whitespace-indentation nil
                          :background nil)
      (qingeditor/font/diminish whitespace-mode " ⓦ" " w")
      (qingeditor/font/diminish global-whitespace-mode " ⓦ" " w"))))

(defmethod qingeditor/cls/init-winner ((this qingeditor/module/editor-base))
  (use-package winner
    :init
    (progn
      (winner-mode t)
      (setq qingeditor/editor-base/winner-boring-buffers
            '("*Completions*"
              "*Compile-Log*"
              "*inferior-lisp*"
              "*Fuzzy Completions*"
              "*Apropos*"
              "*Help*"
              "*cvs*"
              "*Buffer List*"
              "*IBuffer*"
              "*esh command on file*"))
      (setq winner-boring-buffers
            (append winner-boring-buffers qingeditor/editor-base/winner-boring-buffers))
      (winner-mode t))))

