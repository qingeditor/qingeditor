;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qingeditor/c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist
                   `("\\.h\\'" . ,qingeditor/c-c++/default-mode-for-headers)))
    :config
    (progn
      (require 'compile)
      (c-toggle-auto-newline 1)
      (qingeditor/key-binder/set-leader-keys-for-major-mode
       'c-mode
       "ga" 'projectile-find-other-file
       "gA" 'projectile-find-other-file-other-window)
      (qingeditor/key-binder/set-leader-keys-for-major-mode
       'c++-mode
       "ga" 'projectile-find-other-file
       "gA" 'projectile-find-other-file-other-window))))

;; (defun qingeditor/c-c++/init-irony ()
;;   (use-package irony
;;     :ensure t
;;     :defer t
;;     :init
;;     (progn
;;       (add-hook 'c-mode-hook #'irony-mode)
;;       (add-hook 'c++-mode-hook #'irony-mode))
;;     :config
;;     (progn
;;       (define-key irony-mode-map [remap completion-at-point]
;;         #'irony-completion-at-point-async)
;;       (define-key irony-mode-map [remap completion-symbol]
;;         #'irony-completion-at-point-async)
;;       (irony-cdb-autosetup-compile-options))))

;; (defun qingeditor/c-c++/init-company-irony ()
;;   (push 'company-irony company-backends-irony-mode))

(defun qingeditor/c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (progn
      (qingeditor/key-binder/set-leader-keys-for-major-mode
       'c-mode
       "D" 'disaster)
      (qingeditor/key-binder/set-leader-keys-for-major-mode
       'c++-mode
       "D" 'disaster))))

(defun qingeditor/c-c++/init-clang-format ()
  (use-package clang-format
    :if qingeditor/c-c++/enable-clang-support))

(defun qingeditor/c-c++/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode)
           ("\\.cmake\\'" . cmake-mode))
    :init (push 'company-cmake company-backends-cmake-mode)))

(defun qingeditor/c-c++/post-init-company ()
  (qingeditor/add-company-hook c-mode-common c-c++)
  (qingeditor/add-company-hook cmake-mode c-c++)
  (qingeditor/add-company-hook irony-mode c-c++)
  (when qingeditor/c-c++/enable-clang-support
    (push 'company-clang company-backends-c-mode-common)
    (defun qingeditor/c-c++/more-than-prefix-guesser ()
      (qingeditor/c-c++/load-clang-args)
      (company-clang-guess-prefix))

    (setq company-clang-prefix-guesser 'qingeditor/c-c++/more-than-prefix-guesser)
    (qingeditor/add-to-hooks 'qingeditor/c-c++/load-clang-args
                             '(c-mode-hook c++-mode-hook))))

(defun qingeditor/c-c++/init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (push 'company-c-headers company-backends-c-mode-common)))

(defun qingeditor/c-c++/post-init-ycmd ()
  (add-hook 'c++-mode-hook 'ycmd-mode)
  (add-hook 'c-mode-hook 'ycmd-mode)
  (add-to-list 'qingeditor/jump-handlers-c++-mode '(ycmd-goto :async t))
  (add-to-list 'qingeditorjump-handlers-c-mode '(ycmd-goto :async t))
  (prin1 "--------------------------------------")
  (dolist (mode '(c++-mode c-mode))
    (qingeditor/keu-binder/set-leader-keys-for-major-mode mode
      "gG" 'ycmd-goto-imprecise)))

(defun qingeditor/c-c++/post-init-company-ycmd ()
  (push 'company-ycmd company-backends-c-mode-common))
