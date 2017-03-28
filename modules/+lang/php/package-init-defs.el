;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qingeditor/php/init-company-php ()
  (use-package company-php
    :defer t
    :init
    (progn
      (push 'company-ac-php-backend company-backends-php-mode))))

(defun qingeditor/php/post-init-company ()
  (qingeditor/add-company-hook php-mode php))

(defun qingeditor/php/post-init-eldoc ()
  (add-hook 'php-mode-hook 'eldoc-mode))

(defun qingeditor/php/post-init-flycheck ()
  (qingeditor/syntax-checking/add-flycheck-hook 'php-mode))

(defun qingeditor/php/post-init-ggtags ()
  (add-hook 'php-mode-local-vars-hook #'qingeditor/gtags/ggtags-mode-enbale))

(defun qingeditor/php/post-init-helm-gtags ()
  (qingeditor/gtags/helm-gtags-define-keys-for-mode 'php-mode))

(defun qingeditor/php/init-php-extras ()
  (use-package php-extras
    :defer t))

(defun qingeditor/php/init-php-mode ()
  (use-package php-mode
    :defer t
    :mode ("\\.php\\'" . php-mode)))

(defun qingeditor/php/init-phpunit ()
  (use-package phpunit
    :defer t))
