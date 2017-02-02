;; hide mode line
;; from http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "在当前的buffer隐藏`mode-line'的minor mode。"
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; 有时间为了刷新mode-line光调用`(force-mode-line-update)'还不够
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "隐藏`mode line mode'已经开启，您可以用`M-x hidden-mode-line-mode'重新"
             "显示`mode-line'。"))))

(provide 'qingeditor-hidden-mode-line-mode)
