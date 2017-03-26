;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qingeditor/emacs-lisp/init-ielm ()
  (use-package ielm
    :defer t
    :init
    (progn
      (qingeditor/register-repl 'ielm 'ielm)
      (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
        (qingeditor/key-binder/declare-prefix-for-mode mode "ms" "ielm")
        (qingeditor/key-binder/set-leader-keys-for-major-mode
         mode
         "'" 'ielm
         "si" 'ielm)))
    :config
    (defun qing-ielm-indent-line ()
      (interactive)
      (let ((current-point (point)))
        (save-restriction
          (narrow-to-region (search-backward-regexp "^ELISP>") (goto-char current-point))
          (lisp-indent-line))))))

(defun qingeditor/emacs-lisp/post-init-company ()
  (qingeditor/add-company-hook ielm-mode emacs-lisp)
  (push '(company-files company-capf) company-backends-ielm-mode))

(defun qingeditor/emacs-lisp/post-init-eldoc ()
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(defun qingeditor/emacs-lisp/init-auto-compile ()
  (use-package auto-compile
    :defer t
    :diminish (auto-compile-mode . "")
    :init
    (progn
      (setq auto-compile-display-buffer nil
            ;; lets spaceline manage the mode line
            auto-compile-use-mode-line nil
            auto-compile-mode-line-counter t
            )
      (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode))
    :config
    (progn
      (qingeditor/key-binder/set-leader-keys-for-major-mode
       'emacs-lisp-mode
       "cl" 'auto-compile-display-log))))

(defun qingeditor/emacs-lisp/init-elisp-slim-nav ()
  (use-package elisp-slime-nav
    :defer t
    :diminish elisp-slime-nav-mode
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
      (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
        (qingeditor/key-binder/declare-prefix-for-mode mode "mg" "find-symbol")
        (qingeditor/key-binder/declare-prefix-for-mode mode "mh" "help")
        (qingeditor/key-binder/set-leader-keys-for-major-mode
         mode "hh" 'elisp-slime-nav-describe-elisp-thing-at-point)
        (let ((jumpl (intern (format "qingeditor/jump-handlers-%S" mode))))
          (add-to-list jumpl 'elisp-slime-nav-describe-elisp-thing-at-point))))))

(defun qingeditor/emacs-lisp/init-emacs-lisp ()
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (qingeditor/key-binder/declare-prefix-for-mode mode "mc" "compile")
    (qingeditor/key-binder/declare-prefix-for-mode mode "me" "eval")
    (qingeditor/key-binder/declare-prefix-for-mode mode "mt" "tests")
    (qingeditor/key-binder/set-leader-keys-for-major-mode
     mode
     "cc" 'emacs-lisp-byte-compile
     "e$" 'lisp-state-eval-sexp-end-of-line
     "eb" 'eval-buffer
     "ee" 'eval-last-sexp
     "er" 'eval-region
     "ef" 'eval-defun
     "el" 'lisp-state-eval-sexp-end-of-line
     "gG" 'qing-nav-find-elisp-thing-at-point-other-window
     ","  'lisp-state-toggle-lisp-state
     "tb" 'qing-ert-run-tests-buffer
     "tq" 'ert))
  (push 'company-capf company-backends-emacs-lisp-mode)
  (qingeditor/add-company-hook emacs-lisp-mode emacs-lisp))

(defun qingeditor/emacs-lisp/init-macrosetp ()
  (use-package macrostep
    :defer t
    :mode ("\\*.el\\'" emacs-lisp-mode)
    :init
    (progn
      (qingeditor/define-transient-state macrostep
        :title "MacroStep Transient State"
        :doc "\n[_e_] expand [_c_] collapse [_n_/_N_] next/previous [_q_] quit"
        :foreign-keys run
        :bindings
        ("e" macrostep-expand)
        ("c" macrostep-collapse)
        ("n" macrostep-next-macro)
        ("N" macrostep-prev-macro)
        ("q" macrostep-collapse-all :exit t))
      (qingeditor/key-binder/set-leader-keys-for-major-mode
       'emacs-lisp-mode
       "dm" 'qing-macrostep-transient-state/body))))

(defun qingeditor/emacs-lisp/post-init-flycheck ()
  ;; Don't activate flycheck by default in elisp
  ;; because of too much false warnings
  ;; (spacemacs/add-flycheck-hook 'emacs-lisp-mode)
  ;; Make flycheck recognize packages in loadpath
  ;; i.e (require 'company) will not give an error now
  (setq flycheck-emacs-lisp-load-path 'inherit))

(defun qingeditor/emacs-lisp/post-init-semantic ()
  (add-hook 'emacs-lisp-mode 'semantic-mode)
  (with-eval-after-load 'semantic
    (semantic-default-elisp-setup)))

(defun qingeditor/emacs-lisp/post-init-smartparens ()
  )
