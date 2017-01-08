(setq qingeditor-editor-base-packages
  '(
    (abbrev :location built-in)
    ace-window
    (archive-mode :location built-in)
    (bookmark :location built-in)
    (conf-mode :location built-in)
    (dired :location built-in)
    (dired-x :location built-in)
    (electric-indent-mode :location built-in)
    (ediff :location built-in)
    (eldoc :location built-in)
    (exec-path-from-shell :step pre)
    help-fns+
    (hi-lock :location built-in)
    (image-mode :location built-in)
    (imenu :location built-in)
    (linum :location built-in)
    (occur-mode :location built-in)
    (package-menu :location built-in)
    ;; 这个已经在内核中包含
    (page-break-lines :location built-in)
    pcre2el
    (process-menu :location built-in)
    projectile
    (recentf :location built-in)
    (savehist :location built-in)
    (saveplace :location built-in)
    spacemacs-theme
    (subword :location built-in)
    (tar-mode :location built-in)
    (tar-mode :location built-in)
    (uniquify :location built-in)
    (url :location built-in)
    (visual-line-mode :location built-in)
    (whitespace :location built-in)
    (winner :location built-in)))

;; 初始化packages

(defun qingeditor/editor-base/init-abbrev ()
  (qingeditor/ui/editor-font/hide-lighter abbrev-mode))

(defun qingeditor/editor-base/init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (progn
      (qingeditor/core/key-binder/set-leader-keys
       "bD"
       ;; FIXME: Needs new binding.
       ;; "wC" 'qingeditor/ace-center-window
       "wD"
       "wM" 'ace-swap-window
       "wW" 'ace-window)
      ;; 设置`ace-window'按键到`home-row'
      (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))))

(defun qingeditor/editor-base/init-archive-mode ())

(defun qingeditor/editor-base/init-bookmark ()
  (use-package bookmark
    :defer t
    :init
    (progn
      (setq bookmark-default-file (concat qingeditor/cache-dir "bookmarks")
	    ;; 自动保存每一次改变
	    bookmark-save-flag 1)
      (qingeditor/core/key-binder/set-leader-keys "fb" 'bookmark-jump))))

(defun qingeditor/editor-base/init-conf-mode ()
  ;; 初始化
  ;; 强制的指定`conf-mode'继承`text-mode'和`major-mode'。
  (add-hook 'conf-mode-hook 'qingeditor/core/run-text-mode-hooks))

(defun qingeditor/editor-base/init-dired ()
  (qingeditor/core/key-binder/set-leader-keys
    "ad" 'dired
    "fj" 'dired-jump
    "jd" 'dired-jump
    "jD" 'dired-jump-other-window))

(defun qingeditor/editor-base/init-dired-x ()
  (use-package dired-x
    :commands (dired-jump
	       dired-jump-other-window
	       dired-omit-mode)))

(defun qingeditor/editor-base/init-electric-indent-mode ()
  (electric-indent-mode))

(defun qingeditor/editor-base/init-visual-line-mode ()
  (qingeditor/ui/editor-font/diminish visual-line-mode " Ⓛ" " L"))

(defun qingeditor/editor-base/init-ediff ()
  (use-package ediff
    :defer t
    :init
    (progn
      ;; 首先我们先设置一些默认配置
      (setq-default
       ediff-window-setup-function 'ediff-setup-window-plain
       ;; TODO
       ;; 这里的配置需要检查，因为我们不是evil为基础的
       ediff-split-window-function 'split-window-horizontally
       ediff-merge-split-window-function 'split-window-horizentally)
      ;; 使用org ediff展开
      (require 'outline)
      (add-hook 'ediff-prepare-buffer-hook #'show-all)
      ;; 完成比较之后恢复窗口布局
      (add-hook 'ediff-quit-hook #'winner-undo))))

(defun qingeditor/editor-base/init-eldoc ()
  (use-package eldoc
    :defer t
    :config
    (progn
      ;; 执行`eval-expression'的时候打开`eldoc'模式
      (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
      ;; 进入IELM的时候打开`eldoc'模式
      (add-hook 'ielm-mode-hook #'eldoc-mode)
      ;; 不在`mode-line'显示`eldoc'的指示字符
      (qingeditor/ui/editor-font/hide-lighter eldoc-mode))))

(defun qingeditor/editor-base/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :init
    (when (or (qingeditor/runtime/system-is-mac)
	      (qingeditor/runtime/system-is-linux)
	      (memq window-system '(x)))
      (exec-path-from-shell-initialize))))

(defun qingeditor/editor-base/init-help-fns+ ()
  (use-package help-fns+
    :commands (describe-keymap)
    :init (qingeditor/core/key-binder/set-leader-keys "hdK" 'describe-keymap)))

(defun qingeditor/editor-base/init-hi-lock ()
  (with-eval-after-load 'hi-lock
    (qingeditor/ui/editor-font/hide-lighter hi-lock-mode)))

(defun qingeditor/editor-base/init-image-mode ())
(defun qingeditor/editor-base/init-imenu ())
(defun qingeditor/editor-base/init-linum ())
(defun qingeditor/editor-base/init-occur-mode ())
(defun qingeditor/editor-base/init-package-menu ())
(defun qingeditor/editor-base/init-page-break-lines ())
(defun qingeditor/editor-base/init-pcre2el ())
(defun qingeditor/editor-base/init-process-menu ())
(defun qingeditor/editor-base/init-projectile ())
(defun qingeditor/editor-base/init-recentf ())
(defun qingeditor/editor-base/init-savehist ())
(defun qingeditor/editor-base/init-saveplace ())
(defun qingeditor/editor-base/init-spacemacs-theme ())
(defun qingeditor/editor-base/init-subword ())
(defun qingeditor/editor-base/init-tar-mode ())
(defun qingeditor/editor-base/init-uniquify ())
(defun qingeditor/editor-base/init-url ())

(defun qingeditor/editor-base/init-whitespace ())
(defun qingeditor/editor-base/init-winner ())
