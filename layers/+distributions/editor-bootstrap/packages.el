(setq qingeditor-editor-bootstrap-packages
  '(
    (async :step bootstrap)
    (bind-map :step bootstrap)
    (bind-key :step bootstrap)
    (diminish :step bootstrap)
    (hydra :step bootstrap)
    (use-package :step bootstrap)
    (which-key :step bootstrap)))

 ;; 因为我们现在在初始化阶段，我们不能使用`use-package'去初始化这个里面的package软件包
(defun qingeditor/editor-bootstrap/init-async ())

(defun qingeditor/editor-bootstrap/init-bind-key ())

(defun qingeditor/editor-bootstrap/init-diminish ()
  (when (not (qingeditor/layer/layer/package-usedp 'spaceline))
    (add-hook 'after-load-functions 'qingeditor/editor-bootstrap/diminish-hook)))

 (defun qingeditor/editor-bootstrap/init-bind-map ()
   (require 'bind-map)
   (bind-map qingeditor/core/key-binder/default-map
     :prefix-cmd qingeditor-cmds
     :keys (qingeditor/core/user-cfg/emacs-leader-key)
     :override-minor-modes t
     :override-mode-name qingeditor-leader-override-mode))

 (defun qingeditor/editor-bootstrap/init-hydra ()
   (require 'hydra)
   (setq hydra-key-doc-function 'qingeditor/editor-bootstrap/hydra-key-doc-function
         hydra-head-format "[%s] "))

 (defun qingeditor/editor-bootstrap/init-use-package ()
   (require 'use-package)
   (setq use-package-verbose init-file-debug
	 ;; inject use-package hooks for easy customization of stock package
	 ;; configuration
	 use-package-inject-hooks t))

 (defun qingeditor/editor-bootstrap/init-which-key ()
   (require 'which-key)
   (qingeditor/core/toggle/add-toggle
    which-key
    :mode which-key-mode
    :documentation "在一个`buffer'里面显示可用的按键绑定。")
   (qingeditor/core/key-binder/set-leader-keys "hk" 'which-key-show-top-level)

   ;; 更好的函数名的替换规则
   (let ((new-description
          ;; being higher in this list means the replacement is applied later
          '(
            ("qingeditor/core/\\(.+\\)" . "\\1")
            ("qingeditor/core/toggle/toggle-\\(.+\\)" . "\\1")
            ("select-window-\([0-9]\)" . "window \\1"))))
     (dolist (nd new-description)
       (push (cons (concat "\\`" (car nd) "\\'") (cdr nd))
             which-key-description-replacement-alist)))
  
     (which-key-add-key-based-replacements
      (concat qingeditor/core/user-cfg/emacs-leader-key " m") "主模式命令"
      (concat qingeditor/core/user-cfg/emacs-leader-key " " qingeditor/core/user-cfg/emacs-command-key) "M-x")

   (which-key-declare-prefixes
     qingeditor/core/user-cfg/emacs-leader-key '("root" . "qingeditor根空间")
     (concat qingeditor/core/user-cfg/emacs-leader-key " m")
     '("major-mode-cmd" . "主模式命令"))

   ;; 禁止`qingeditor'特殊按键，因为如果你不理解它，可能使你迷惑。
   (pcase qingeditor/core/user-cfg/which-key-position
     (`right (which-key-setup-side-window-right))
     (`bottom (which-key-setup-side-window-bottom))
     (`right-then-bottom (which-key-setup-side-window-right-bottom)))

   (setq which-key-special-keys nil
         which-key-use-C-h-for-paging t
         which-key-prevent-C-h-from-cycling t
         which-key-echo-keystrokes 0.02
         which-key-max-description-length 32
         which-key-sort-order 'which-key-key-order-alpha
         which-key-idle-delay qingeditor/core/user-cfg/which-key-delay
         which-key-allow-evil-operators t)
   (which-key-mode)
   (qingeditor/ui/editor-font/diminish which-key-mode " Ⓚ" " k"))
