;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; editor-base module packages init methods defs

(defmethod qingeditor/cls/init-async ((this qingeditor/module/editor-bootstrap)))

(defmethod qingeditor/cls/init-bind-map ((this qingeditor/module/editor-bootstrap))
  "init bind map."
  (require 'bind-map)
  (bind-map qingeditor/default-map
    :prefix-cmd qingeditor/cmds
    :keys (qingeditor/config/leader-key)
    :override-minor-modes t
    :override-mode-name qingeditor/leader-override-mode))

(defmethod qingeditor/cls/init-bind-key ((this qingeditor/module/editor-bootstrap)))

(defmethod qingeditor/cls/init-diminish ((this qingeditor/module/editor-bootstrap))
  "init diminish."
  (let ((modulemgr (qingeditor/gmodulemgr)))
    (when (not (qingeditor/cls/package-usedp modulemgr 'spaceline))
      (add-hook
       'qingeditor/editor-ready-hooks
       (lambda ()
         (qingeditor/editor-bootstrap/diminish-hook t)
         (add-hook 'after-load-functions
                   'qingeditor/editor-bootstrap/diminish-hook))))))

(defmethod qingeditor/cls/init-hydra ((this qingeditor/module/editor-bootstrap))
  "init hydra."
  (require 'hydra)
  (setq hydra-key-doc-function 'qingeditor/editor-bootstrap/hydra-key-doc-function)
  (setq hydra-head-format "[%s] "))

(defmethod qingeditor/cls/init-use-package ((this qingeditor/module/editor-bootstrap))
  "init use package."
  (require 'use-package)
  (setq use-package-verbose init-file-debug)
  ;; inject use-package hooks for easy customization of stock package
  ;; configuration
  (setq use-package-inject-hooks t))

(defmethod qingeditor/cls/init-which-key ((this qingeditor/module/editor-bootstrap))
  "init which key."
  (require 'which-key)
  (qingeditor/toggle/add-toggle
   which-key
   :mode which-key-mode
   :documentation "Display a buffer with available key bindings.")
  (qingeditor/key-binder/set-leader-keys "hk" 'which-key-show-top-level)
  ;; Replace rules for better naming of functions
  (let ((new-descriptions
         ;; being higher in this list means the replacement is applied later
         '(
           ("qingeditor/\\(.+\\)" . "\\1")
           ("qingeditor/toggle-\\(.+\\)" . "\\1")
           ("select-window-\\([0-9]\\)" . "window \\1")
           ("qingeditor/alternate-buffer" . "last buffer")
           ("qingeditor/toggle-mode-line-\\(.+\\)" . "\\1")
           ("avy-goto-word-or-subword-1" . "avy word")
           ("shell-command" . "shell cmd")
           ("qingeditor/default-pop-shell" . "open shell")
           ("qingeditor/helm-project-smart-do-search-region-or-symbol" . "smart search w/input")
           ("qingeditor/helm-project-smart-do-search" . "smart search")
           ("qingeditor/search-project-auto-region-or-symbol" . "search project w/input")
           ("qingeditor/search-project-auto" . "search project")
           ("helm-descbinds" . "show keybindings")
           ("sp-split-sexp" . "split sexp")
           ("avy-goto-line" . "avy line")
           ("universal-argument" . "universal arg")
           ("er/expand-region" . "expand region")
           ("helm-apropos" . "apropos")
           ("\\(.+\\)-transient-state/\\(.+\\)" . "\\2")
           ("\\(.+\\)-transient-state/body" . "\\1-transient-state"))))
    (dolist (nd new-descriptions)
      ;; ensure the target matches the whole string
      (push (cons (concat "\\`" (car nd) "\\'") (cdr nd))
            which-key-description-replacement-alist)))
  (which-key-add-key-based-replacements
    (concat qingeditor/config/leader-key " m") "major mode commands"
    (concat qingeditor/config/leader-key " " qingeditor/config/command-key) "M-x")

  (which-key-declare-prefixes
    qingeditor/config/leader-key '("root" . "qingeditor root")
    (concat qingeditor/config/leader-key " m")
    '("major-mode-cmd" . "Major mode commands"))

  ;; disable special key handling for qingeditor, since it can be
  ;; disorienting if you don't understand it
  (pcase qingeditor/config/which-key-position
    (`right (which-key-setup-side-window-right))
    (`bottom (which-key-setup-side-window-bottom))
    (`right-then-bottom (which-key-setup-side-window-right-bottom)))

  (setq which-key-special-keys nil
        which-key-use-C-h-for-paging t
        which-key-prevent-C-h-from-cycling t
        which-key-echo-keystrokes 0.02
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay qingeditor/config/which-key-delay
        which-key-allow-evil-operators nil)

  (which-key-mode)
  (qingeditor/font/diminish which-key-mode " Ⓚ" " K"))
