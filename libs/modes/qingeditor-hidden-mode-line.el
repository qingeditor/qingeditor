;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define qingeditor/hidden-mode-line-mode
;;
;; hide mode line
;; from http://bzg.fr/emacs-hide-mode-line.html

(defvar-local qingeditor/hidden-mode-line-mode nil)
(defvar-local qingeditor/hide-mode-line/mode-format-backup nil)
(define-minor-mode qingeditor/hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable qingeditor/hidden-mode-line-mode
  :group 'editing-basics
  (if qingeditor/hidden-mode-line-mode
      (setq qingeditor/hide-mode-line/mode-format-backup mode-line-format
            mode-line-format nil)
    (setq mode-line-format qingeditor/hide-mode-line/mode-format-backup
          qingeditor/mode/hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             qingeditor/hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x qingeditor/hidden-mode-line-mode to make the mode-line appear."))))

(provide 'qingeditor-hidden-mode-line)
