;;; -*- lexical-binding: t -*-
;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; Define which-key keymap prefixes
(setq qingeditor/editor-base/key-binding-prefixes
      '(("a"    "applications")
        ("ai"   "irc")
        ("as"   "shells")
        ("b"    "buffers")
        ("c"    "compile/comments")
        ("C"    "capture/colors")
        ("e"    "errors")
        ("f"    "files")
        ("fC"   "files/convert")
        ("fe"   "emacs(qingeditor)")
        ("fv"   "variables")
        ("g"    "git/version-control")
        ("h"    "help")
        ("hd"   "help-describe")
        ("i"    "insertion")
        ("j"    "jump/join/split")
        ("k"    "lisp")
        ("kd"   "delete")
        ("kD"   "delete-backward")
        ("n"    "narrow/numbers")
        ("p"    "projects")
        ("p$"   "projects/shell")
        ("q"    "quit")
        ("r"    "registers/rings/resume")
        ("Re"   "elisp")
        ("Rp"   "pcre")
        ("s"    "search/symbol")
        ("sa"   "ag")
        ("sg"   "grep")
        ("sk"   "ack")
        ("st"   "pt")
        ("sw"   "web")
        ("t"    "toggles")
        ("tC"   "colors")
        ("th"   "highlight")
        ("tm"   "modeline")
        ("T"    "UI toggles/themes")
        ("C-t"  "other toggles")
        ("w"    "windows")
        ("wp"   "popup")
        ("x"    "text")
        ("xa"   "align")
        ("xd"   "delete")
        ("xg"   "google-tanslate")
        ("xl"   "lines")
        ("xm"   "move")
        ("xt"   "transpose")
        ("xw"   "words")
        ("z"    "zoom")))

(mapc (lambda (x) (apply #'qingeditor/key-binder/declare-prefix x))
      qingeditor/editor-base/key-binding-prefixes)

(setq echo-keystrokes 0.02)
;; auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; improve delete-other-windows
(define-key global-map (kbd "C-x 1") 'qingeditor/editor-base/toggle-maximize-buffer)

;; alternate binding to search next occurrence with isearch without
;; exiting isearch
(define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)
;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
(define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)

;; Make <escape> quit as much as possible
(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

;; linum margin bindings
(global-set-key (kbd "<left-margin> <down-mouse-1>") 'qing-md-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>") 'qing-mu-select-linum)
(global-set-key (kbd "<left-margin <double-mouse-1>") 'qing-select-current-block)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'qing-mu-select-linum)

;; qingeditor leader key bindings
;; universal argument
(qingeditor/key-binder/set-leader-keys "u" 'universal-argument)

;; shell command
(qingeditor/key-binder/set-leader-keys "!" 'shell-command)

;; applications
(qingeditor/key-binder/set-leader-keys
  "ac"   'calc-dispatch
  "ap"   'list-processes
  "aP"   'proced
  "au"   'undo-tree-visualize)

;; buffers
(qingeditor/key-binder/set-leader-keys
 "TAB"      'qing-alternate-buffer
 "bd"       'qing-kill-this-buffer
 "be"       'qing-safe-erase-buffer
 "bh"       'qing-home
 "b C-d"    'qing-kill-matching-buffers-rudely
 "bn"       'next-buffer
 "bm"       'qing-kill-other-buffers
 "bN"       'qing-new-empty-buffer
 "bP"       'qing-copy-clipboard-to-whole-buffer
 "bp"       'previous-buffer
 "bR"       'qing-safe-revert-buffer
 "bs"       'qing-switch-to-scratch-buffer
 "bY"       'qing-copy-whole-buffer-to-clipboard
 "bw"       'read-only-mode)

;; Cycling settings
(qingeditor/key-binder/set-leader-keys "Tn" 'qing-cycle-qingeditor-theme)

;; errors
(qingeditor/key-binder/set-leader-keys
  "en" 'qing-next-error
  "eN" 'qing-previous-error
  "ep" 'qing-previous-error)

(qingeditor/define-transient-state error
  :title "Error transient state"
  :hint-is-doc t
  :dynamic-hint
  (let ((sys (qingeditor/error-delegate)))
    (cond
     ((eq 'flycheck sys)
      "\nBrowsing flycheck errors from this buffer.")
     ((eq 'emacs sys)
      (let ((buf (next-error-find-buffer)))
        (if buf
            (concat "\nBrowsing entries from \""
                    (buffer-name buf)
                    "\""
                    (with-current-buffer buf
                      (when qingeditor/gne-line-func
                        (format " (%d of %d)"
                                (max 1 (1+ (- qingeditor/gne-cur-line
                                              qingeditor/gne-min-line)))
                                (1+ (- qingeditor/gne-max-line
                                       qingeditor/gne-min-line))))))
          "\nNo next-error capable buffer found.")))))
  :bindings
  ("n" qing-next-error "next")
  ("p" qing-next-error "previous")
  ("q" nil "quit" :exit t)
  :leader "e.")

;; file
(qingeditor/key-binder/set-leader-keys
 "fc"    'qing-copy-file
 "fD"    'qing-delete-current-buffer-file
 "fei"   'qing-find-user-init-filename
 "fed"   'qing-find-user-config-file
 "feD"   'qing-ediff-user-config-and-template
 "fev"   'qing-display-and-copy-version
 "fCd"   'qing-unix2dos
 "fCu"   'qing-dos2unix
 "fg"    'rgrep
 "fl"    'find-file-literally
 "fE"    'qing-sudo-edit
 "fo"    'qing-open-file-or-directory-in-external-app
 "fR"    'qing-rename-current-buffer-file
 "fS"    'qing-write-all
 "fs"    'save-buffer
 "fvd"   'add-dir-local-variable
 "fvf"   'add-file-local-variable
 "fvp"   'add-file-local-variable-prop-line
 "fy"    'qing-show-and-copy-buffer-filename)

;; help
(qingeditor/key-binder/set-leader-keys
 "hdb"   'describe-bindings
 "hdc"   'describe-char
 "hdf"   'describe-function
 "hdk"   'describe-key
 "hdl"   'qing-describe-last-keys
 "hdp"   'describe-package
 "hdP"   'qing-describe-package
 "hds"   'qing-describe-system-info
 "hdt"   'describe-theme
 "hdv"   'describe-variable
 "hI"    'qing-report-issue
 "hn"    'view-emacs-news)
;; insert stuff
(qingeditor/key-binder/set-leader-keys
 "iJ" 'qing-insert-line-below-no-indent
 "iK" 'qing-insert-line-above-no-indent
 "ik" 'qing-insert-line-above
 "ij" 'qing-insert-line-below)

;; format
(qingeditor/key-binder/set-leader-keys
 "jo" 'open-line
 "j=" 'qing-indent-region-or-buffer
 "jS" 'qing-split-and-new-line
 "jk" 'qing-goto-next-line-and-indent)

;; navigation/jumping
(qingeditor/key-binder/set-leader-keys
 "j0" 'qing-push-mark-and-goto-beginning-of-line
 "j$" 'qing-push-mark-and-goto-end-of-line
 "jf" 'find-function
 "jv" 'find-variable)

;; Compliation
(qingeditor/key-binder/set-leader-keys
 "cC" 'compile
 "ck" 'kill-compilation
 "cr" 'recompile
 "cd" 'qing-close-compilation-window)

(with-eval-after-load 'compile
  (define-key compilation-mode-map "r" 'recompile)
  (define-key compilation-mode-map "g" nil))

;; Narrow and widen
(qingeditor/key-binder/set-leader-keys
 "nr" 'narrow-to-region
 "np" 'narrow-to-page
 "nf" 'narrow-to-defun
 "nw" 'widen)

;; toggle
(qingeditor/add-toggle highlight-current-line-globally
  :mode global-hl-line-mode
  :documentation "Globally highlight the current line."
  :leader "thh")

(qingeditor/add-toggle truncate-lines
  :status truncate-lines
  :on (toggle-truncate-lines)
  :off (toggle-truncate-lines -1)
  :documentation "Truncate long lines (no wrap)."
  :leader "tl")

(qingeditor/add-toggle visual-line-mode
  :status visual-line-mode
  :on
  (progn
    (visual-line-mode))
  :off
  (progn
    (visual-line-mode -1))
  :documentation "Move point according to visual lines."
  :leader "tL")

(qingeditor/add-toggle auto-fill-mode
  :status auto-fill-function
  :on (auto-fill-mode)
  :off (auto-fill-mode -1)
  :documentation "Break line beyond `current-fill-column' while editing."
  :leader "tF")

(qingeditor/add-toggle debug-on-error
  :status debug-on-error
  :on (setq debug-on-error t)
  :off (setq debug-on-error nil)
  :documentation "Toggle display of backtrace when an error happens."
  :leader "tD")

(qingeditor/add-toggle fringe
  :status (not (equal fringe-mode 0))
  :on (call-interactively 'fringe-mode)
  :off (fringe-mode 0)
  :documentation "Display the fringe in GUI mode."
  :leader "Tf")

(qingeditor/add-toggle fullscreen-frame
  :status (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
  :on (qing-toggle-frame-fullscreen)
  :off (qing-toggle-frame-fullscreen)
  :documentation "Display the current frame in full screen."
  :leader "TF")

(qingeditor/add-toggle maximize-frame
  :status (eq (frame-parameter nil 'fullscreen) 'maximized)
  :on (toggle-frame-maximized)
  :off (toggle-frame-maximized)
  :documentation "Maximize the current frame."
  :leader "TM")

(qingeditor/add-toggle mode-line
  :status (not qingeditor/hidden-mode-line-mode)
  :on (qingeditor/hidden-mode-line-mode -1)
  :off (qingeditor/hidden-mode-line-mode)
  :documentation "Toggle the visibility of modeline."
  :leader "tmt")

(qingeditor/add-toggle transparent-frame
  :status nil
  :on (qing-toggle-transparency)
  :documentation "Make the current frame non-opaque."
  :leader "TO")

(qingeditor/define-transient-state scale-transparency
  :title "Frame Transparency Transient State"
  :doc "\n[_+_/_=_] increase transparency [_-_] decrease [_T_] toggle [_q_] quit"
  :bindings
  ("+" qing-increase-transparency)
  ("=" qing-increase-transparency)
  ("-" qing-decrease-transparency)
  ("T" qing-toggle-transparency)
  ("q" nil :exit t))

(qingeditor/key-binder/set-leader-keys
 "TT"
 'qing-scale-transparency-transient-state/qing-toggle-transparency)

(qingeditor/add-toggle tool-bar
  :if window-system
  :mode tool-bar-mode
  :documentation "Display the toolbar in GUI mode."
  :leader "Tt")

(qingeditor/add-toggle menu-bar
  :if window-system
  :mode menu-bar-mode
  :documentation "Display the menu bar."
  :leader "Tm")

(qingeditor/add-toggle semantic-stickyfunc
  :mode semantic-stickyfunc-mode
  :documentation "Enable semantic stickyfunc mode."
  :leader "TS")

(qingeditor/add-toggle semantic-stickyfunc-globally
  :mode global-semantic-stickyfunc-mode
  :documentation "Enable semantic stickyfunc globally."
  :leader "T C-S")

;; quit
(qingeditor/key-binder/set-leader-keys
 "qs" 'qing-save-buffers-kill-emacs
 "qq" 'qing-prompt-kill-emacs
 "qQ" 'qing-kill-emacs
 "qz" 'qing-frame-killer)
