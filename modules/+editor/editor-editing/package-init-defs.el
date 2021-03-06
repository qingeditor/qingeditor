;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qingeditor/editor-editing/init-aggressive-indent ()
  (use-package aggressive-indent
    :defer t
    :init
    (progn
      (qingeditor/add-toggle aggressive-indent
        :mode aggressive-indent-mode
        :documentation "Always keep code indented."
        :leader "tI")
      (qingeditor/add-toggle aggressive-indent-globally
        :mode global-aggressive-indent-mode
        :documentation "Always keep code indented globally."
        :leader "t C-I")
      (qingeditor/font/diminish aggressive-indent-mode " Ⓘ" " I"))
    :config
    (progn
      (add-hook 'diff-auto-refine-mode-hook
                #'qing-toggle-aggressive-indent-off))))

(defun qingeditor/editor-editing/init-avy ()
  (use-package avy
    :defer t
    :commands (qing-avy-open-url qing-avy-goto-url avy-pop-mark)
    :init
    (progn
      (setq avy-all-windows 'all-frames)
      (setq avy-background t)
      (qingeditor/key-binder/set-leader-keys
       "jb" 'avy-pop-mark
       "jj" 'avy-goto-char
       "jJ" 'avy-goto-char-2
       "jl" 'avy-goto-line
       "ju" 'qing-avy-goto-url
       "jw" 'avy-goto-word-or-subword-1
       "xo" 'qing-avy-open-url))
    :config
    (progn
      (defun qing-avy-goto-url ()
        "Use avy to go to an URL in the buffer."
        (interactive)
        (avy--generic-jump "https?://" nil 'pre))

      (defun qing-avy-open-url ()
        "Use avy to select an URL in the buffer and open it."
        (interactive)
        (save-excursion
          (qing-avy-goto-url)
          (browse-url-at-point))))))

(defun qingeditor/editor-editing/init-bracketed-paste ()
  (use-package bracketed-paste
    :defer t
    :init
    ;; Enable bracketed-paste for tty
    (add-hook 'tty-setup-hook 'bracketed-paste-enable)))

(defmethod qingeditor/editor-editing/init-clean-aindent-mode ()
  (use-package clean-aindent-mode
    :config (clean-aindent-mode)))

(defmethod qingeditor/editor-editing/init-eval-sexp-fu ()
  ;; ignore obsolete function warning generated on startup
  (let ((byte-compile-not-obsolete-funcs (append byte-compile-not-obsolete-funcs '(preceding-sexp))))
    (require 'eval-sexp-fu)))

(defun qingeditor/editor-editing/init-expand-region ()
  (use-package expand-region
    :defer t
    :init (qingeditor/key-binder/set-leader-keys "v" 'er/expand-region)
    :config
    (progn
      ;; add search capability to expand-region
      (when (qingeditor/modulemgr/package-usedp 'helm-ag)
        (defadvice er/prepare-for-more-expansions-internal
            (around helm-ag/prepare-for-more-expansions-internal activate)
          ad-do-it
          (let ((new-msg (concat (car ad-return-value)
                                 ", / to search in project, "
                                 "f to search in files, "
                                 "b to search in opened buffers"))
                (new-bindings (cdr ad-return-value)))
            (cl-pushnew
             '("/" (lambda ()
                     (call-interactively
                      'qing-helm-project-smart-do-search-region-or-symbol)))
             new-bindings)

            (cl-pushnew
             '("f" (lambda ()
                     (call-interactively
                      'qing-helm-files-smart-do-search-region-or-symbol)))
             new-bindings)

            (cl-pushnew
             '("b" (lambda ()
                     (call-interactively
                      'qing-helm-buffers-smart-do-search-region-or-symbol)))
             new-bindings)

            (setq ad-return-value (cons new-msg new-bindings)))))
      (setq expand-region-contract-fast-key "V"
            expand-region-reset-fast-key "r"))))

(defun qingeditor/editor-editing/init-hexl ()
  (use-package hexl
    :defer t
    :init
    (progn
      (qingeditor/key-binder/set-leader-keys "fh" 'hexl-find-file)
      (qingeditor/key-binder/set-leader-keys-for-major-mode
       'hexl-mode
       "d" 'hexl-insert-decimal-char
       "c" 'hexl-insert-octal-char
       "x" 'hexl-insert-hex-char
       "X" 'hexl-insert-hex-string
       "g" 'hexl-goto-address
       "]" 'hexl-end-of-1k-page
       "[" 'hexl-beginning-of-1k-page
       "h" 'hexl-backward-char
       "l" 'hexl-forward-char
       "j" 'hexl-next-line
       "k" 'hexl-previous-line
       "$" 'hexl-end-of-line
       "^" 'hexl-beginning-of-line
       "0" 'hexl-beginning-of-line))))

(defun qingeditor/editor-editing/init-hungry-delete ()
  (use-package hungry-delete
    :defer t
    :init
    (qingeditor/add-toggle hungry-delete
      :mode hungry-delete-mode
      :documentation "Delete consecutive horizonatal whitespace with a signal key."
      :leader "td")
    :config
    (progn
      (setq-default hungry-delete " \t\f\v") ; only horizonatal whitespace
      (define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
      (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char))))

(defun qingeditor/editor-editing/init-link-hint ()
  (use-package link-hint
    :defer t
    :init
    (progn
      (qingeditor/key-binder/set-leader-keys
        "xO" 'link-hint-open-multiple-links
        "xo" 'link-hint-open-link))))

(defun qingeditor/editor-editing/init-lorem-ipsum ()
  (use-package lorem-ipsum
    :commands (lorem-ipsum-insert-list
               lorem-ipsum-insert-paragraphs
               lorem-ipsum-insert-sentences)
    :init
    (progn
      (qingeditor/key-binder/declare-prefix "il" "lorem ipsum")
      (qingeditor/key-binder/set-leader-keys
        "ill" 'lorem-ipsum-insert-list
        "ilp" 'lorem-ipsum-insert-paragraphs
        "ils" 'lorem-ipsum-insert-sentences))))

(defun qingeditor/editor-editing/init-move-text ()
  (use-package move-text
    :defer t
    :init
    (qingeditor/define-transient-state move-text
        :title "Move Text Transient State"
        :bindings
        ("J" move-text-down "move down")
        ("K" move-text-up "move up"))
    (qingeditor/key-binder/set-leader-keys
      "xJ" 'qing-move-text-transient-state/move-text-down
      "xK" 'qing-move-text-transient-state/move-text-up)))

(defun qingeditor/editor-editing/init-origami ()
  (use-package origami
    :defer t
    :init
    (progn
      (global-origami-mode)
      (qingeditor/define-transient-state fold
        :title "Code Fold Transient State"
        :doc "
 Close^^            Open^^             Toggle^^           Goto^^             Other^^
 ───────^^───────── ───────^^───────── ───────^^───────── ───────^^───────── ───────^^─────────
 [_c_] at point     [_o_] at point     [_a_] at point     [_n_] next         [_s_] single out
 [_C_] recursively  [_O_] recursively  [_A_] all          [_p_] previous     [_R_] reset
 [_m_] all          [_r_] all          [_TAB_] like org ^^                   [_q_] quit"
        :foreign-keys run
        :on-enter (unless (bound-and-true-p origami-mode) (origami-mode 1))
        :bindings
        ("a" origami-forward-toggle-node)
        ("A" origami-toggle-all-nodes)
        ("c" origami-close-node)
        ("C" origami-close-node-recursively)
        ("o" origami-open-node)
        ("O" origami-open-node-recursively)
        ("r" origami-open-all-nodes)
        ("m" origami-close-all-nodes)
        ("n" origami-next-fold)
        ("p" origami-previous-fold)
        ("s" origami-show-only-node)
        ("R" origami-reset)
        ("TAB" origami-recursively-toggle-node)
        ("<tab>" origami-recursively-toggle-node)
        ("q" nil :exit t)
        ("C-g" nil :exit t)
        ("<SPC>" nil :exit t))
      (qingeditor/key-binder/declare-prefix "to" "Origami toggles")
      (qingeditor/key-binder/set-leader-keys
        "toa" 'qing-fold-transient-state/origami-forward-toggle-node
        "toc" 'qing-fold-transient-state/origami-close-node
        "toC" 'qing-fold-transient-state/origami-close-node-recursively
        "toO" 'qing-fold-transient-state/origami-open-node-recursively
        "too" 'qing-fold-transient-state/origami-open-node
        "tor" 'qing-fold-transient-state/origami-open-all-nodes
        "tom" 'qing-fold-transient-state/origami-close-all-nodes
        "tos" 'qing-fold-transient-state/origami-show-only-node
        "ton" 'qing-fold-transient-state/origami-next-fold
        "top" 'qing-fold-transient-state/origami-previous-fold
        "toR" 'qing-fold-transient-state/origami-reset
        "to <tab>" 'qing-fold-transient-state/origami-recursively-toggle-node
        "to TAB" 'qing-fold-transient-state/origami-recursively-toggle-node))))

(defun qingeditor/editor-editing/init-smartparens ()
  (use-package smartparens
    :defer t
    :commands (sp-split-sexp sp-newline sp-up-sexp)
    :init
    (progn
      ;; settings
      (setq sp-show-pair-delay 0.2
            ;; fix paren highlighting in normal mode
            sp-show-pair-from-inside t
            sp-cancel-autoskip-on-backward-movement nil
            sp-highlight-pair-overlay nil
            sp-highlight-wrap-overlay nil
            sp-highlight-wrap-tag-overlay nil)
      (qingeditor/add-to-hooks '(prog-mode-hook comint-mode-hook)
                               (if qingeditor/config/smartparens-strict-mode
                                   'smartparens-strict-mode
                                 'smartparens-mode))
      ;; enable smartparens-mode in `eval-expression'
      (add-hook 'minibuffer-setup-hook 'qingeditor/editor-editing/conditionally-enable-smartparens-mode)
      ;; toggles
      (qingeditor/add-toggle smartparens
        :mode smartparens-mode
        :documentation "Enable smartparens."
        :leader "tp")

      (qingeditor/add-toggle smartparens-globally
        :mode smartparens-global-mode
        :documentation "Enable smartparens globally."
        :leader "t C-p")
      ; key bindings
      (qingeditor/key-binder/set-leader-keys
       "js" 'sp-split-sexp
       "jn" 'sp-newline)
      (qingeditor/font/diminish smartparens-mode " ⓟ" " p"))
    :config
    (progn
      (require 'smartparens-config)
      (qingeditor/editor-editing/adaptive-smartparent-pair-overlay-face)
      (add-hook 'qingeditor/post-theme-change-hook
                'qingeditor/editor-editing/adaptive-smartparent-pair-overlay-face)
      (show-smartparens-global-mode +1)
      ;; don't create a pair with single quote in minibuffer
      (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
      (sp-pair "{" nil :post-handlers
               '(:add (qingeditor/editor-editing/smartparens-pair-newline-and-ident "RET")))
      (sp-pair "[" nil :post-handlers
               '(:add (qingeditor/editor-editing/smartparens-pair-newlien-and-indent "RET")))
      (when qingeditor/config/smart-closing-parenthesis
        (global-set-key ?\) 'qing-smart-closing-parenthesis)))))

(defun qingeditor/editor-editing/init-qingeditor/whitespace-cleanup ()
  (use-package qingeditor-whitespace-cleanup
    :commands (qingeditor/whitespace-cleanup-mode
               qingeditor/global-whitespace-cleanup-mode)
    :init
    (progn
      (qingeditor/add-toggle whitespace-cleanup
        :mode qingeditor/whitespace-cleanup-mode
        :documentation "Automatic whitespace clean up."
        :on-message (qingeditor/whitespace-cleanup/on-message)
        :leader "tW")
      (qingeditor/add-toggle global-whitespace-cleanup
        :mode qingeditor/global-whitespace-cleanup-mode
        :status qingeditor/whitespace-cleanup-mode
        :on (let ((qingeditor/whitespace-cleanup-globally t))
              (qingeditor/whitespace-cleanup-mode))
        :off (let ((qingeditor/whitespace-cleanup-globally t))
               (qingeditor/whitespace-cleanup-mode -1))
        :on-message (qingeditor/whitespace-cleanup/on-message t)
        :documentation "Global automatic whitespace clean up."
        :leader "t C-S-w")
      (with-eval-after-load 'ws-butler
        (when qingeditor/config/whitespace-cleanup
          (qingeditor/toggle-global-whitespace-cleanup-on)))
      (qingeditor/font/diminish qingeditor/whitespace-cleanup-mode " Ⓦ" " W")
      (qingeditor/font/diminish qingeditor/global-whitespace-cleanup-mode " Ⓦ" " W"))))

(defun qingeditor/editor-editing/init-undo-tree ()
  (use-package undo-tree
    :init
    (progn
      (global-undo-tree-mode)
      (setq undo-tree-visualizer-timestamps t)
      (setq undo-tree-visualizer-diff t)
      (qingeditor/font/hide-lighter undo-tree-mode))))

(defun qingeditor/editor-editing/init-uuidgen ()
  (use-package uuidgen
    :commands (uuidgen-1 uuidgen-4)
    :init
    (progn
      (qingeditor/key-binder/declare-prefix "iU" "uuid")
      (qingeditor/key-binder/set-leader-keys
       "iU1" 'qing-uuidgen-1
       "iU4" 'qing-uuidgen-4
       "iUU" 'qing-uuidgen-4))))

(defun qingeditor/editor-editing/init-ws-butler ()
  ;; not deferred on purpose, init-spacemacs-whitespace-cleanup need
  ;; it to be loaded.
  (use-package ws-butler
    :init
    (progn
      (qingeditor/font/hide-lighter ws-butler-mode))))
