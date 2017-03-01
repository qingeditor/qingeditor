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
