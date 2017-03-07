;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; editor-base init method defs

(defun qingeditor/editor-base/init-abbrev ()
  "init abbrev."
  (qingeditor/font/hide-lighter abbrev-mode))

(defun qingeditor/editor-base/init-ace-window ()
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

(defun qingeditor/editor-base/init-archive-mode ()
  "init archive mode."
  (use-package archive-mode
    :commands archive-mode))

(defun qingeditor/editor-base/init-bookmark ()
  (use-package bookmark
    :defer t
    :init
    (progn
      (setq bookmark-default-file (concat qingeditor/cache-dir "bookmarks"))
      ;; autosave each change
      (setq bookmark-save-flag 1)
      (qingeditor/key-binder/set-leader-keys "fb" 'bookmark-jump))))

(defun qingeditor/cls/init-conf-mode ((this qingeditor/module/editor-base))
  ;; explicitly derive conf-mode from text-mode major-mode
  (add-hook 'conf-mode-hook 'qingeditor/run-text-mode-hooks))

(defun qingeditor/editor-base/init-dired ()
  (qingeditor/key-binder/set-leader-keys
   "ad" 'dired
   "fj" 'dired-jump
   "jd" 'dired-jump
   "jD" 'dired-jump-other-window))

(defun qingeditor/editor-base/init-dired-x ()
  (use-package dired-x
    :commands (dired-jump
               dired-jump-other-window
               dired-omit-mode)))

(defun qingeditor/editor-base/init-electric-indent-mode ()
  (electric-indent-mode))

(defun qingeditor/editor-base/init-visual-line-mode ()
  (qingeditor/font/diminish visual-line-mode " Ⓛ" " L"))

(defun qingeditor/editor-base/init-ediff ()
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

(defun qingeditor/editor-base/init-eldoc ()
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

(defun qingeditor/editor-base/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :init (when (or (qingeditor/system-is-mac)
                    (qingeditor/system-is-linux))
            (exec-path-from-shell-initialize))))

(defun qingeditor/editor-base/init-help-fns+ ()
  (use-package help-fns+
    :commands (describe-keymap)
    :init
    (qingeditor/key-binder/set-leader-keys
      "hdK" 'describe-keymap)))

(defun qingeditor/editor-base/init-hi-lock ()
  (with-eval-after-load 'hi-lock
    (qingeditor/font/hide-lighter hi-lock-mode)))

(defun qingeditor/editor-base/init-image-mode ()
  (use-package image-mode
    :defer t))

(defun qingeditor/editor-base/init-imenu ()
  (use-package imenu
    :defer t
    :init (qingeditor/key-binder/set-leader-keys
            "ji" 'imenu)))

(defun qingeditor/editor-base/init-linum ()
  (when qingeditor/config/line-numbers
    (add-hook 'prog-mode-hook 'linum-mode)
    (add-hook 'text-mode-hook 'linum-mode))
  (setq linum-format "%4d")
  (qingeditor/toggle/add-toggle line-numbers
    :mode linum-mode
    :documentation "Show the line numbers."))

(defun qingeditor/editor-base/init-occur-mode ()
  (use-package occur-mode
    :defer t))

(defun qingeditor/editor-base/init-package-menu ()
  (use-package package-menu-mode
    :defer t))

(defun qingeditor/editor-base/init-qingeditor/page-break-lines ()
  (require 'qingeditor-page-break-lines)
  (qingeditor/global-page-break-lines-mode t)
  (qingeditor/font/hide-lighter qingeditor/page-break-lines-mode))

(defun qingeditor/editor-base/init-pcre2el ()
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

(defun qingeditor/editor-base/init-process-menu ()
  (use-package process-menu-mode
    :defer t))

(defun qingeditor/editor-base/init-projectile ()
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

(defun qingeditor/editor-base/init-recentf ()
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

(defun qingeditor/editor-base/init-savehist ()
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

(defun qingeditor/editor-base/init-saveplace ()
  (use-package saveplace
    :init
    (progn
      (if (fboundp 'save-place-mode)
          ;; Emacs 25 has proper mode for `save-place'
          (save-place-mode)
        (setq save-place t))
      ;; Save point position between sessions
      (setq save-place-file (concat qingeditor/cache-dir "places")))))

(defun qingeditor/editor-base/init-spacemacs-theme ()
  (use-package spacemacs-theme
    :defer t
    :init
    (progn
      (setq spacemacs-theme-comment-bg t)
      (setq spacemacs-theme-org-height t))))

(defun qingeditor/editor-base/init-subword ()
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

(defun qingeditor/editor-base/init-tar-mode ()
  (use-package tar-mode
    :defer t))

(defun qingeditor/editor-base/init-uniquify ()
  (require 'uniquify)
  ;; When having windows with repeated filenames, uniquify them
  ;; by the folder they are in rather those annoying <2>,<3>,.. etc
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  ;; don't screw special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(defun qingeditor/editor-base/init-url ()
  ;; gravatars from magit use this to store their cache
  (setq url-configuration-directory (concat qingeditor/cache-dir "url/")))

(defun qingeditor/editor-base/init-whitespace ()
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

(defun qingeditor/editor-base/init-winner ()
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

