;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qingeditor/auto-completion/init-company ()
  (use-package compnay
    :defer t
    :init
    (progn
      (setq company-idle-delay 0.2
            company-minimum-prefix-length 2
            company-require-match nil
            company-dabbrev-ignore-case nil
            company-dabbrev-downcase nil)
      (add-hook 'company-completion-started-hook #'qingeditor/auto-completion/company-turn-off-fci)
      (add-hook 'company-completion-finished-hook #'qingeditor/auto-completion/company-maybe-turn-on-fci)
      (add-hook 'company-completion-cancelled-hook #'qingeditor/auto-completion/company-maybe-turn-on-fci)
      (qingeditor/font/diminish company-mode " ⓐ" " a"))
    :config
    (progn
      ;; key bindings
      (defun qing-company-complete-common-or-cycle-backward ()
        "Complete common prefix or cycle backward."
        (interactive)
        (company-complete-common-or-cycle -1))
      (qingeditor/auto-completion/set-RET-key-behavior 'company)
      (qingeditor/auto-completion/set-TAB-key-behavior 'company)
      (qingeditor/auto-completion/setup-key-sequence 'company)

      (let ((map company-active-map))
        (define-key map (kbd "C-/") 'company-search-candidates)
        (define-key map (kbd "C-M-/") 'company-filter-candidates)
        (define-key map (kbd "C-d") 'company-show-doc-buffer))
      (qingeditor/auto-completion/company-active-navigation)
      (setq company-transformers '(qingeditor/auto-completion/company-transformer-cancel
                                   company-sort-by-occurrence)))))

(defun qingeditor/auto-completion/init-helm-company ()
  (use-package helm-company
    :if (qingeditor/modulemgr/package-usedp 'company)
    :defer t
    :init
    (with-eval-after-load 'company
      (define-key company-active-map (kbd "C-/") 'helm-company))))
