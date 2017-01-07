(defvar qingeditor-editor-bootstrap-packages
  '(
    (async :step bootstrap)
    (bind-map :step bootstrap)
    (bind-key :step bootstrap)
    (diminish :step bootstrap)
    (evil :step bootstrap)
    (hydra :step bootstrap)
    (use-package :step bootstrap)
    (which-key :step bootstrap)))

;; 因为我们现在在初始化阶段，我们不能使用`use-package'去初始化这个里面的package软件包
(defun qingeditor/editor-bootstrap/init-async ())
(defun qingeditor/editor-bootstrap/init-bind-map ()
  (require 'bind-map)
  (bind-map qingeditoreditor-default-map
    :prefix-cmd qingeditoreditor-cmds
    :keys (qingeditor/core/user-cfg/emacs-leader-key)
    :evil-keys (qingeditor/core/user-cfg/leader-key)
    :override-minor-modes t
    :override-mode-name qingeditoreditor-leader-override-mode))

(defun qingeditor/editor-bootstrap/init-bind-key ())

(defun qingeditor/editor-bootstrap/init-diminish ()
  (when (not (qingeditor/layer/layer/package-usedp 'spaceline))
    (add-hook 'after-load-functions 'qingeditor/editor-bootstrap/diminish-hook)))

(defun qingeditor/editor-bootstrap/init-evil ()
  ;; evil-mode是qingeditoreditor必须的模块
  ;; 我们在这里显示加载
  (require 'evil)
  (evil-mode 1)
  ;; 使用evil作为默认的跳转函数
  (push 'evil-goto-definition qingeditor/core/jumper/default-jump-handlers)

  (require 'cl)
  ;; 定义不同状态下的光标
  (defvar qingeditor/editor-bootstrap/evil-cursors
    '(("normal" "DarkGoldenrod2" box)
      ("insert" "chartreuse3" (bar . 2))
      ("emacs" "SkyBlue2" box)
      ("hybrid" "SkyBlue2" (bar . 2))
      ("replace" "chocolate" (hbar . 2))
      ("evilified" "LightGoldenrod3" box)
      ("visual" "gray" (hbar . 2))
      ("motion" "plum3" box)
      ("lisp" "HotPink1" box)
      ("iedit" "firebrick1" box)
      ("iedit-insert" "firebrick1" (bar . 2)))
    "定义不同evil状态下的指针的颜色。")
  (cl-loop for (state color cursor) in qingeditor/editor-bootstrap/evil-cursors
           do
           (eval `(defface ,(intern (format "qingeditor/editor-bootstrap/%s-face" state))
                    `((t (:background ,color
                                      :foreground ,(face-background 'mode-line)
                                      :inherit 'mode-line)))
                    (format "%s状态face。" state)
                    :group 'qingeditoreditor))
           (set (intern (format "evil-%s-state-cursor" state))
                (list (when qingeditor/core/user-cfg/colorize-cursor-according-to-state color)
                      cursor)))
  (add-hook 'qingeditor/gvars/post-theme-change-hook 'qingeditor/editor-bootstrap/set-state-faces)
  ;; put back refresh of the cursor on post-command-hook see status of:
  ;; https://bitbucket.org/lyro/evil/issue/502/cursor-is-not-refreshed-in-some-cases
  ;; (add-hook 'post-command-hook 'evil-refresh-cursor)

  ;; evil ex-command
  (define-key evil-normal-state-map (kbd qingeditor/core/user-cfg/ex-command-key) 'evil-ex)
  (define-key evil-visual-state-map (kbd qingeditor/core/user-cfg/ex-command-key) 'evil-ex)
  (define-key evil-motion-state-map (kbd qingeditor/core/user-cfg/ex-command-key) 'evil-ex)

  (setq evil-ex-substitute-global qingeditor/core/user-cfg/ex-substitute-global)
  ;; evil-want-Y-yank-to-eol must be set via customize to have an effect
  (customize-set-variable 'evil-want-Y-yank-to-eol qingeditor/core/user-cfg/remap-Y-to-y$)

  ;; 只在GUI模式下定义 `evil-jump-forward'
  (define-key evil-motion-state-map [C-i] 'evil-jump-forward)

  ;; Make the current definition and/or comment visible.
  (define-key evil-normal-state-map "zf" 'reposition-window)
  (define-key evil-window-map (kbd "o") 'qingeditor/core/toggle-maximize-buffer)
  (define-key evil-window-map (kbd "C-o") 'qingeditor/core/toggle-maximize-buffer)

  ;; make cursor keys work
  (define-key evil-window-map (kbd "<left>") 'evil-window-left)
  (define-key evil-window-map (kbd "<right>") 'evil-window-right)
  (define-key evil-window-map (kbd "up") 'evil-window-up)
  (define-key evil-window-map (kbd "down") 'evil-window-down)
  (qingeditor/core/key-binder/set-leader-keys "re" 'evil-show-registers)

  ;; 定义几个帮助buffer的按键
  (evil-define-key 'motion help-mode-map (kbd "<escape>") 'quit-system)
  (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
  (evil-define-key 'motion help-mode-map (kbd "S-<tab>") 'backward-button)
  (evil-define-key 'motion help-mode-map (kbd "]") 'help-go-forward)
  (evil-define-key 'motion help-mode-map (kbd "gf") 'help-go-forward)
  (evil-define-key 'motion help-mode-map (kbd "[") 'help-go-back)
  (evil-define-key 'motion help-mode-map (kbd "gb") 'help-go-back)
  (evil-define-key 'motion help-mode-map (kbd "gh") 'help-follow-symbol)

  ;; 设置一个不大不小的默认值
  (setq-default evil-shift-width 2)
  ;; 在major mode加载的时候我们为特定的mode设置`evil-shit-width'
  (add-hook 'after-change-major-mode-hook 'qingeditor/editor-bootstrap/set-evil-shift-width 'append)
  ;; 在shift的时候保持region保持激活状态
  (when qingeditor/core/user-cfg/retain-visual-state-on-shift
    (qingeditor/editor-bootstrap/evil-map visual "<" "<gv")
    (qingeditor/editor-bootstrap/evil-map visual ">" ">gv"))
  ;; 向上和向下移动选区
  (when qingeditor/core/user-cfg/visual-line-move-text
    (define-key evil-visual-state-map "J" (concat ":m" ">+1" (kbd "RET") "gv=gv"))
    (define-key evil-visual-state-map "K" (concat ":m" "<-2" (kbd "RET") "gv=gv")))

  (evil-ex-define-cmd "enew" 'qingeditor/qingeditoreditor-base/new-empty-buffer)

  (define-key evil-normal-state-map (kbd "K") 'qingeditor/editor-bootstrap/evil-smart-doc-lookup)
  (define-key evil-normal-state-map (kbd "gd") 'qingeditor/core/jumper/jump-to-definition)

  ;; 滚动transient state
  (qingeditor/core/transient-state/define-transient-state
   scroll
   :title "滚动临时状态"
   :bindings
   ("," evil-scroll-page-up "向上翻页")
   ("." evil-scroll-page-down "向下翻页")
   ("<" evil-scroll-up "向上翻半页")
   (">" evil-scroll-down "向下翻半页"))

  (qingeditor/core/key-binder/set-leader-keys
   "n," 'qingeditor/transient-state/scroll-transient-state/evil-scroll-page-up
   "n." 'qingeditor/transient-state/scroll-transient-state/evil-scroll-page-down
   "n<" 'qingeditor/transient-state/scroll-transient-state/evil-scroll-up
   "n>" 'qingeditor/transient-state/scroll-transient-state/evil-scroll-down)

  ;; 粘贴 transient-state
  (evil-define-command qingeditor/editor-bootstrap/transient-state-0 ()
    :keep-visual t
    :repeat nil
    (interactive)
    (if current-prefix-arg
        (progn
          (setq this-command #'digit-argument)
          (call-interactively #'digit-argument))
      (setq this-command #'evil-beginning-of-line
            hydra-deactivate t)
      (call-interactively #'evil-beginning-of-line)))

  (qingeditor/core/transient-state/define-transient-state
   paste
   :title "粘贴临时状态"
   :doc "\n[%s(length kill-ring-yank-pointer)/%s(length kill-ring)] \
   [_C-j_/_C-k_] cycles through yanked text, [_p_/_P_] pastes the same text \
   above or below. Anything else exits."
   :bindings
   ("C-j" evil-paste-pop)
   ("C-k" evil-paste-pop-next)
   ("p" evil-paste-after)
   ("P" evil-paste-before)
   ("0" qingeditor/editor-bootstrap/transient-state-0))
  (when qingeditor/core/user-cfg/enable-paste-transient-state
    (define-key evil-normal-state-map
      "p" 'qingeditor/transient-state/paste-transient-state/evil-paste-after)
    (define-key evil-normal-state-map
      "P" 'qingeditor/transient-state/paste-transient-state/evil-paste-before))

  ;; 折叠临时状态
  (when (eq 'evil qingeditor/core/user-cfg/folding-method)
    (qingeditor/core/transient-state/define-transient-state
     fold
     :title "代码折叠临时状态"
     :doc  "
 Close^^          Open^^              Toggle^^             Other^^
 ───────^^──────  ─────^^───────────  ─────^^────────────  ─────^^───
 [_c_] at point   [_o_] at point      [_a_] around point   [_q_] quit
 ^^               [_O_] recursively   ^^
 [_m_] all        [_r_] all"
     :foreign-keys run
     :bindings
     ("a" evil-toggle-fold)
     ("c" evil-close-fold)
     ("o" evil-open-fold)
     ("O" evil-open-fold-rec)
     ("r" evil-open-folds)
     ("m" evil-close-folds)
     ("q" nil :exit t)
     ("C-g" nil :exit t)
     ("<SPC>" nil :exit t)))
  (qingeditor/core/key-binder/set-leader-keys "z." 'qingeditor/transient-state/fold-transient-state-body)

  ;; 定义文本对象
  (qingeditor/editor-bootstrap/define-text-object "$" "dollar" "$" "$")
  (qingeditor/editor-bootstrap/define-text-object "*" "start" "*" "*")
  (qingeditor/editor-bootstrap/define-text-object "8" "block-start" "/*" "*/")
  (qingeditor/editor-bootstrap/define-text-object "|" "bar" "|" "|")
  (qingeditor/editor-bootstrap/define-text-object "%" "percent" "%s" "%s")
  (qingeditor/editor-bootstrap/define-text-object "/" "slash" "/" "/")
  (qingeditor/editor-bootstrap/define-text-object "_" "underscore" "_" "_")
  (qingeditor/editor-bootstrap/define-text-object "-" "hyphen" "-" "-")
  (qingeditor/editor-bootstrap/define-text-object "~" "title" "~" "~")
  (qingeditor/editor-bootstrap/define-text-object "=" "equal" "=" "=")

  (evil-define-text-object evil-pasted (count &rest args)
    (list (save-excursion (evil-goto-mark ?\[) (point))
          (save-excursion (evil-goto-mark ?\]) (point))))

  (define-key evil-inner-text-objects-map "P" 'evil-pasted)
  ;; 为整个buffer定义文本对象
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)
  ;; turn off evil in corelv buffers
  (push '("\\*LV\\*") evil-buffer-regexps)
  ;; 把 `dired-goto-file' 替换为 `helm-find-files', 因为 `helm-find-files'
  ;; 可以做同样的事情，另外他有模糊匹配和其他一些特性。
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map "J" 'qingeditor/helm/helm-find-files)
    (define-key dired-mode-map "j" 'qingeditor/helm/helm-find-files)
    (evil-define-key 'normal dired-mode-map (kbd qingeditor/core/user-cfg/leader-key)
      qingeditor/core/key-binder/default-map))

  ;; 支持智能匹配括号
  (when (qingeditor/layer/layer/package-usedp 'smartparens)
    (defadvice evil-delete-backward-char-and-join
        (around qingeditor/editor-bootstrap/evil-delete-backward-char-and-join activate)
      (if (bound-and-true-p smartparens-strict-mode)
          (call-interactively 'sp-backward-delete-char)
        ad-do-it)))

  ;; 为comit定义历史命令
  (when (eq qingeditor/core/user-cfg/editing-style 'vim)
    (evil-define-key 'insert comint-mode-map
      (kbd "C-k") 'comint-previous-input
      (kbd "C-j") 'comint-next-input))
  (evil-define-key 'normal comint-mode-map
    (kbd "C-k") 'comint-previous-input
    (kbd "C-j") 'comint-next-input))

(defun qingeditor/editor-bootstrap/init-hydra ()
  (require 'hydra)
  (setq hydra-key-doc-function 'qingeditor/editor-bootstrap/hydra-key-doc-function
        hydra-head-format "[%s] "))

(defun qingeditor/editor-bootstrap/init-use-package ()
  (require 'use-package)
  (setq use-package-verbose init-file-debug)
  ;; inject use-package hooks for easy customization of stock package
  ;; configuration
  use-package-inject-hooks t)

(defun qingeditor/editor-bootstrap/init-which-key ()
  (require 'which-key)
  (qingeditor/core/toggle/add-toggle
   which-key
   :mode which-key-mode
   :documentation "在一个`buffer'里面显示可用的按键绑定。"
   :evil-leader "tK")
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
  (dolist (leader-key `(,qingeditor/core/user-cfg/leader-key ,qingeditor/core/user-cfg/emacs-leader-key))
    (which-key-add-key-based-replacements
      (concat leader-key " m") "主模式命令"
      (concat leader-key " " qingeditor/core/user-cfg/emacs-command-key) "M-x"))

  (which-key-declare-prefixes
    qingeditor/core/user-cfg/leader-key '("root" . "qingeditoreditor根空间")
    qingeditor/core/user-cfg/emacs-leader-key '("root" . "qingeditoreditor根空间")
    (concat qingeditor/core/user-cfg/leader-key " m")
    '("major-mode-cmd" . "主模式命令")
    (concat qingeditor/core/user-cfg/emacs-leader-key " m")
    '("major-mode-cmd" . "主模式命令"))

  ;; 禁止`qingeditoreditor'特殊按键，因为如果你不理解它，可能使你迷惑。
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
