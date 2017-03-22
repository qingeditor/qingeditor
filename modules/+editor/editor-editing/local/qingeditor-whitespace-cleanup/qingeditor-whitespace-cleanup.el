;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3


;;; Commentary:
;; This package adds a minor mode to automatically cleanup whitespace.
;; It requires Spacemacs and depends on the value of the variable
;; `qingeditor/config/whitespace-cleanup' (see documentation in config file).

(defvar qingeditor/whitespace-cleanup-globally nil
  "If non nil then `qingeditor/whitespace-celanup-mode' is applied globally.")

;;;###autoload
(define-minor-mode qingeditor/whitespace-cleanup-mode
  "Minor mode to clean whitespace.

The minior mode is based on the value of the config file variable
`qingeditor/config/whitespace-cleanup' to determine the behavior
of the cleanup."
  :lighter " CleanW"
  :group 'qingeditor
  (if qingeditor/whitespace-cleanup-mode
      (qingeditor/whitespace-cleanup/turn-on
       qingeditor/whitespace-cleanup-globally)
    (qingeditor/whitespace-cleanup/turn-off
     qingeditor/whitespace-cleanup-globally)))

(define-global-minor-mode qingeditor/global-whitespace-cleanup-mode
  qingeditor/whitespace-cleanup-mode
  (lambda ()
    (let ((qingeditor/whitespace-cleanup-globally t))
      (qingeditor/whitespace-cleanup-mode)))
  :group 'qingeditor
  :require 'qingeditor/whitespace-cleanup-mode)

(defun qingeditor/whitespace-cleanup/on-message (&optional global)
  "Return a string to display when the mode is activated."
  (prin1 qingeditor/config/whitespace-cleanup)
  (pcase qingeditor/config/whitespace-cleanup
    (`all
     (format "whitespace-cleanup enabled%s (all whitespace)"
             (if global " globally" "")))
    (`trailing
     (format "whitespace-clean enabled%s (trailing whitespace)"
             (if global " globally" "")))
    (`changed
     (format "whitespace-clean enabled%s (changed lines)"
             (if global " globally" "")))))

(defun qingeditor/whitespace-cleanup/turn-on (&optional gloabl)
  "Turn on `qingeditor/whitespace-cleanup-mode'."
  (pcase qingeditor/config/whitespace-cleanup
    (`all
     (add-hook 'before-save-hook 'whitespace-cleanup nil (not global)))
    (`trailing
     (add-hook 'before-save-hook 'delete-trailing-whitespace nil (not global)))
    (`change
     (when (fboundp 'ws-butler-mode)
       (if global
           (if global (ws-butler-global-mode) (ws-butler-mode)))))))

(defun qingeditor/whitespace-cleanup/turn-off (&optional global)
  "Turn off `qingeditor/whitespace-cleanup-mode'."
  (pcase qingeditor/config/whitespace-cleanup
    (`all
     (remove-hook 'before-save-hook 'whitespace-cleanup (not global)))
    (`trailing
     (remove-hook 'before-save-hook 'delete-trailing-whitespace (not global)))
    (`changed
     (when (fboundp 'ws-butler-mode)
       (if global (ws-butler-global-mode -1) (ws-butler-mode -1))))))

(provide 'qingeditor-whitespace-cleanup)
