;; qingeditor界面渲染相关函数
(require 'qingeditor-runtime)

(defun qingeditor/ui/editor/remove-unused-gui-elements ()
  "移除`qingeditor'中不适用的界面元素,包含`menu bar', `tool bar'和`scroll bar'."
  (unless (qingeditor/runtime/window-system-is-mac)
    ;; `macos'下面的`Emacs'没有独立的`menu bar'。
    (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
      (menu-bar-mode -1)))
  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1)))

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

(provide 'qingeditor-editor)
