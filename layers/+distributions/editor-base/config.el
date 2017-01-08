;; qingeditor base配置层配置文件
;; ------------------------------------------------------------------------------------------------------
;; 导航相关的配置
;; ------------------------------------------------------------------------------------------------------

;; ;; 自动刷新
;; (global-auto-revert-mode 1)

;; ;; 自动刷新dired，但是在后台自动刷新
;; (setq global-auto-revert-non-file-buffers t
;;       auto-revert-verbose nil)

;; ;; 让`dired'对某些操作的目标文件夹进行猜测，比如向一个分割后的`buffer'进行复制文件
;; (setq dired-dwim-target t)

;; ;; 定义一个我们认为无用和有用的`buffer'的名称的正则表达式，这样的话我们在切换`buffer'的时候比较好
;; (defvar qingeditor/editor-base/useless-buffers-regexp '("*\.\+")
;;   "保存无用的`buffer'的正则表达式列表。")

;; (defvar qingeditor/editor-base/useful-buffers-regexp '("\\*scratch\\*")
;;   "定义有用的`buffer'的正则表达式，尽管`buffer'同时匹配`qingeditor/editor-base/useless-buffers-regexp'。")

;; ;; 关闭bell的声音
;; (setq ring-bell-function 'ignore
;;       visible-bell nil)

;; ;; 修复tabulated-list.el的bug
;; ;; 详情: http://redd.it/2dgy52
;; (defun tabulated-list-revert (&rest ignore)
;;   "`tabulated-list-mode'的函数`revert-buffer-function'先运行`tabulated-list-revert-hook'
;; 然后运行`tabulated-list-print'。"
;;   (interactive)
;;   (unless (derived-mode-p 'tabulated-list-mode)
;;     (error "当前的`mode'不是`Tabulated List mode'。"))
;;   (run-hooks 'tabulated-list-revert-hook)
;;   ;; 我们的改动在这里
;;   ;; (tabulated-list-print t)
;;   (tabulated-list-print))

;; ;; 打开xterm的鼠标支持
;; (xterm-mouse-mode 1)

;; ;; 在编程的相关`mode'中高亮超链接，并且可以通过鼠标点击进行访问
;; ;; `goto-address-prog-mode'只在字符串和注释中高亮
;; (add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; ;; 在注释和字符串中高亮显示跟踪bug
;; (add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;; ;; 当浏览帮助`buffer'的时候保持焦点
;; (setq help-window-select t)

;; ;; 滚动到第一个编译错误或者到末尾
;; (setq compilation-scroll-output 'first-error)

;; ;; 不要去ping一些看着很像域名的名字
;; (setq ffap-machine-p-known 'reject)

;; ;; ------------------------------------------------------------------------------------------------------
;; ;; 编辑相关
;; ;; ------------------------------------------------------------------------------------------------------

;; ;; 设置`scratch buffer'默认的`mode'，这个可以加快emacs加载速度，因为他可以避免加载`elisp mode'
;; ;; 相关的很多文件
;; (setq initial-major-mode 'text-mode)

;; ;; 我们缩进的时候只使用空格进行缩进
;; (setq-default indent-tabs-mode nil
;;               ;; 仅仅针对`elisp mode'比较好，其他语言还会单独配置
;;               tab-width 2)

;; ;; 文本相关
;; (setq longlines-show-hard-newlines t)

;; ;; Use system trash for file deletion
;; ;; should work on Windows and Linux distros
;; ;; on OS X, see contrib/osx layer
;; (setq delete-by-moving-to-trash t)

;; (setq-default fill-column 80)
;; (qingeditor/ui/editor-font/diminish auto-fill-function " Ⓕ" " F")

;; ;; 持久化缩写文件
;; (setq-default abbrev-file-name (concat qingeditor/cache-dir "abbrev_defs"))

;; ;; 在贴换剪切板内容之前保存到`kill-ring'中
;; (setq save-interprogram-paste-before-kill t)

;; ;; 在语句之间一个空格比两个空格的情况要多
;; (setq-default sentence-end-double-space nil)

;; ;; The C-d rebinding that most shell-like buffers inherit from
;; ;; comint-mode assumes non-evil configuration with its
;; ;; `comint-delchar-or-maybe-eof' function, so we disable it
;; (with-eval-after-load 'comint
;;   (define-key comint-mode-map (kbd "C-d") nil))

;; ;; 当打开大文件的时候我们不加载任何`mode'
;; (add-hook 'find-file-hook 'qingeditor/editor-base/check-large-file)

;; ;; ------------------------------------------------------------------------------------------------------
;; ;; UI
;; ;; ------------------------------------------------------------------------------------------------------

;; ;; 为了黄金比例
;; (setq window-combination-resize t)

;; ;; 显示在`mode line'显示`column'号
;; (setq column-number-mode t)

;; ;; 打开全局的高亮显示当前行
;; (global-hl-line-mode t)

;; ;; 关闭光标闪烁
;; (blink-cursor-mode 0)

;; ;; 当`emacs'回答`yes'或者`no'的时候，我们使用`y'或者`n'代替。
;; (fset 'yes-or-no-p 'y-or-n-p)

;; ;; draw underline lower
;; (setq x-underline-at-descent-line t)

;; ;; don't let the cursor go into minibuffer prompt
;; ;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
;; (setq minibuffer-prompt-properties
;;       '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; ;; 在启动的时候最大化frame
;; (if qingeditor/core/user-cfg/fullscreen-at-startup
;;     ;; qingeditor/editor-base/toggle-fullscreen-frame-on在启动的时候不存在
;;     (if (fboundp 'qingeditor/editor-base/toggle-fullscreen-frame-on)
;;         (qingeditor/editor-base/toggle-fullscreen-frame-on)
;;       (qingeditor/editor-base/toggle-frame-fullscreen))
;;   (if qingeditor/core/user-cfg/maximized-at-startup
;;       (add-hook 'window-setup-hook 'toggle-frame-maximized)))

;; (setq ns-use-native-fullscreen (not qingeditor/core/user-cfg/fullscreen-use-non-native))

;; ;; 让`next-buffer'，`other-buffer'，跳过没有用的`buffers'
;; ;; (详情参见 `qingeditor/editor-base/useless-buffer-p')
;; (let ((buf-pred-entry (assq 'buffer-predicate default-frame-alist)))
;;   (if buf-pred-entry
;;       ;; `buffer-predicate' 项要是存在的话，修改它
;;       (setcdr buf-pred-entry #'qingeditor/editor-base/useless-buffer-p)
;;     ;; 如果不存在的话，我们去创建他
;;     (push '(buffer-predicate . ‘qingeditor/editor-base/useless-buffer-p) default-frame-alist)))


;; ;; ------------------------------------------------------------------------------------------------------
;; ;; 会话相关的设置
;; ;; ------------------------------------------------------------------------------------------------------

;; ;; 讲自定义变量保存在 `~/.qingeditor'文件里面
;; (unless (bound-and-true-p custom-file)
;;   (setq custom-file (qingeditor/core/user-cfg/user-cfg-filename)))

;; ;; 清空`scratch buffer'
;; (setq initial-scratch-message nil)

;; ;; 不要去创建`backup~'文件
;; (setq make-backup-files nil)

;; ;; 自动保存相关的设置
;; (setq auto-save-default (not (null qingeditor/core/user-cfg/auto-save-file-location)))
;; (setq auto-save-list-file-prefix qingeditor/auto-save-dir)

;; ;; 强制保存`TRAMP URLs'保存到缓存文件夹，忽略`qingeditor/core/user-cfg/auto-save-file-location'
;; ;; 的相关设置
;; (let ((auto-save-dir (concat qingeditor/auto-save-dir "dist/")))
;;   (setq auto-save-file-name-transforms
;;         `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,auto-save-dir  t)))
;;   (unless (or (file-exists-p auto-save-dir)
;;               (null qingeditor/core/user-cfg/auto-save-file-location))
;;     (make-directory auto-save-dir t)))

;; ;; 选择自动保存的位置
;; (cl-case qingeditor/core/user-cfg/auto-save-file-location
;;   (cache (let ((auto-save-dir (concat qingeditor/auto-save-dir "site/")))
;;            (add-to-list 'auto-save-file-name-transforms
;;                         `(".*" ,auto-save-dir t) 'append)
;;            (unless (file-exists-p auto-save-dir)
;;              (make-directory auto-save-dir t))))
;;   (original (setq auto-save-visited-file-name t))
;;   (_ (setq auto-save-default nil
;;            auto-save-list-file-prefix nil)))

;; ;; 当打印表达式值得时候删除多于的省略号
;; (setq eval-expression-print-length nil
;;       eval-expression-print-level nil)

;; ;; 缓存文件
;; (setq tramp-persistency-file-name (concat qingeditor/cache-dir "tramp"))

;; ;; 在这些地方发出告警感觉没有意义
;; (put 'narrow-to-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)
;; (put 'erase-buffer 'disabled nil)
;; (put 'scroll-left 'disabled nil)
;; (put 'dired-find-alternate-file 'disabled nil)

;; ;; 如果文件被其他客户端打开那么我们关闭提示框
;; (defun qingeditor/editor-base/server-remove-kill-buffer-hook ()
;;   (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

;; (add-hook 'server-visit-hook 'qingeditor/editor-base/server-remove-kill-buffer-hook)

;; ;; ------------------------------------------------------------------------------------------------------
;; ;; 其他设置
;; ;; ------------------------------------------------------------------------------------------------------

;; ;; hook into `hack-local-variables' in order to allow switching qingeditor
;; ;; configurations based on local variables
;; (add-hook 'hack-local-variables-hook #'qingeditor/editor-base/run-local-vars-mode-hook)
