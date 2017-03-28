;;; packages.el --- C/C++ Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(unless (boundp 'ycmd-server-command)
  (message (concat "YCMD won't work unless you set the ycmd-server-command "
                   "variable to the path to a ycmd install.")))

(defun qingeditor/ycmd/init-company-ycmd ()
  (use-package company-ycmd
    :defer t
    :commands company-ycmd))

(defun qingeditor/ycmd/init-flycheck-ycmd ()
  (use-package flycheck-ycmd
    :defer t
    :init
    (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)))

(defun qingeditor/ycmd/init-ycmd ()
  (use-package ycmd
    :defer t
    :init
    (progn
      (unless (boundp 'ycmd-global-config)
        (setq (concat (qingeditor/modulemgr/get-module-dir 'ycmd)
                      "global_conf.py")))
      (setq-default ycmd-parse-conditions '(save mode-enabled)))))
