;; ;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;; ;;
;; ;; Author: zzu_softboy <zzu_softboy@163.com>
;; ;; Github: https://www.github.com/qingeditor/qingeditor
;; ;;
;; ;; This file is not part of GNU Emacs.
;; ;; License: GPLv3
;; ;;
;; ;; The editor base module extra config script

;; ;; Navigation config begin

;; ;; Auto refresh
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it.
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Make dired "guess" target direcotry for some operations, like copy to
;; direcotry visited in other split buffer.
(setq dired-dwin-target t)

;; Regexp for useful and useless buffers for smarter buffer switching
(defvar qingeditor/editor-base/useless-buffers-regexp '("*\.\+")
  "Regexp used to determine if a buffer is not useful.")

(defvar qingeditor/editor-base/useful-buffers-regexp '("\\*scratch\\*")
  "Regexp used to define buffers thsta are usefull despite matching
 `qingeditor/editor-base/useless-buffers-regexp'.")

;; no beep pleeeeeease ! (and no visual blinking too please)
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Hack to fix a bug with tabulated-list.el
;; see: http://redd.it/2dgy52
(defun tabulated-list-revert (&rest ignore)
  "The `revert-buffer-function' for `tabulated-list-mode'.
It runs `tabulated-list-revert-hook', then calls `tabulated-list-print'."
  (interactive)
  (unless (derived-mode-p 'tabulated-list-mode)
    (error "The current buffer is not in Tabulated list mode"))
  (run-hooks 'tabulated-list-revert-hook)
  ;; hack is here
  ;; (tabulated-list-print t)
  (tabulated-list-print))

;; Mouse cursor in terminal mode
(xterm-mouse-mode 1)

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
;; Highlight and follow bug refrences in comments and strings
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;; Keep focus while navigating help buffers
(setq help-window-select t)

;; Scrolll compilation to first error or end
(setq compilation-scroll-output 'fisrt-error)

;; Don't try to ping things that look loke domain names
(setq ffap-machine-p-known 'reject)

;; Navigation config end

;; Edit config begin

;; start scratch in text mode (usefull to get a faster Emacs load time
;; because it avoid autoload of elisp modes)
(setq initial-major-mode 'text-mode)

;; use only spaces and no tabs
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Text
(setq longlines-show-hard-newlines t)

;; Use system trash for file deletion
;; should work on Windows and Linux distros
;; on OS X, see contrib/osx module
(setq delete-by-moving-to-trash t)

;; auto fill breaks line beyond buffer's fill-column
(setq-default fill-column 80)
(qingeditor/font/diminish auto-fill-function " Ⓕ" " F")

;; persistent abbreviation file
(setq abbrev-file-name (concat qingeditor/cache-dir "abbrev_defs"))

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Single space between sentences is more widespread than double 
(setq-default sentence-end-double-space nil)

;; The C-d rebinding that most shell-like buffers inherit from
;; comint-mode assume non-evil configuration with its
;; `comint-delchar-or-maybe-eof' function, so we disable it
(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-d") nil))

;; Prompt to open file literally if large file
(add-hook 'find-file-hook 'qingeditor/editor-base/check-large-file)
;; Edit config end

;; UI config begin
;; important for golden-ratio to better work
(setq window-combination-resize t)
;; Show column number in mode line
(setq column-number-mode t)
;; Highlight current line
(global-hl-line-mode t)
;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)
;; draw underline lower
(setq x-underline-at-descent-line t)
;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
;;  Fullscreen/maximize frame on startup
(if qingeditor/config/fullscreen-at-startup
    ;; qingeditor/toggle-fullscreen-frame-on is not available during the startup
    ;; but is available during the sequence config reloads
    (if (fboundp 'qingeditor/toggle-fullscreen-frame-on)
        (qingeditor/toggle-fullscreen-frame-on)
      (qingeditor/toggle-frame-fullscreen))
  (if qingeditor/config/maximized-at-startup
      (add-hook 'window-setup-hook 'toggle-frame-maximized)))

(setq ns-use-native-fullscreen (not qingeditor/config/fullscreen-use-non-native))

;; make `next-buffer', `other buffer', etc. ignore useless buffer (see
;; `qingeditor/editor-base/useless-buffer-p')
(let ((buf-pred-entry (assq 'buffer-predicate default-frame-alist)))
  (if buf-pred-entry
      ;; `buffer-predicate' entry exists, modify it
      (setcdr buf-pred-entry #'qingeditor/editor-base/useful-buffer-p)
    ;; `buffer-predicate' entry doesn't exist, create it
    (push '(buffer-predicate . qingeditor/editor-base/useful-buffer-p) default-frame-alist)))

;; UI config end

;; Session config begin
;; save custom variables in ~/.qingeditor
(unless (bound-and-true-p custom-file)
  (setq custom-file qingeditor/config/target-cfg-filename))
(setq initial-scratch-message nil)
;; don't create backup~ files
(setq make-backup-files nil)

;; Auto save file
(setq auto-save-default (not (null qingeditor/config/auto-save-file-location)))
(setq auto-save-list-file-prefix (concat qingeditor/auto-save-dir))
;; aways save `tramp' urls to create direcotry no matter what is the value
;; of `qingeditor/config/auto-save-file-location'
(let ((autosave-dir (concat qingeditor/auto-save-dir "dist/")))
  (setq auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,autosave-dir t)))
  (unless (or (file-exists-p autosave-dir)
              (null qingeditor/config/auto-save-file-location))
    (make-directory autosave-dir t)))
;; Choose auto-save location
(cl-case qingeditor/config/auto-save-file-location
  (cache (let ((autosave-dir (concat qingeditor/auto-save-dir "site/")))
           (add-to-list 'auto-save-file-name-transforms
                        `(".*" ,autosave-dir t) 'append)
           (unless (file-exists-p autosave-dir)
             (make-directory autosave-dir t))))
  (original (setq auto-save-visited-file-name t))
  (_ (setq auto-save-default nil
           auto-save-list-file-prefix nil)))
;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;; cache files
(setq tramp-persistency-file-name (concat qingeditor/cache-dir "tramp"))

;; seems pointless to warn. There's always undo
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; remove prompt if the file is opened in other clients
(defun qingeditor/editor-base/server-remove-kill-buffer-hook ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'qingeditor/editor-base/server-remove-kill-buffer-hook)
;; Session config end

;; other config begin
;; hook into `hack-local-variables' in order to allow switching qingeditor
;; configurations based on local variables
(add-hook 'hack-local-variables-hook #'qingeditor/editor-base/run-local-vars-mode-hook)
;; other config end
