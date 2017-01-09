;; 这个文件主要负责整个编辑器的初始化引导等函数的定义
;; 加载依赖的第三方库
(require 'subr-x nil 'noerror)
(require 'page-break-lines)
(require 'ht)
(require 'eieio)

(require 'qingeditor-user-cfg)
(require 'qingeditor-macros)
(require 'qingeditor-installer)
(require 'qingeditor-editor-theme)
(require 'qingeditor-editor-font)
(require 'qingeditor-layer)
(require 'qingeditor-jumper)
(require 'qingeditor-key-binder)
(require 'qingeditor-transient-state)
(require 'qingeditor-toggle)
(require 'qingeditor-release-mgr)

;; 负责`qingeditor'用户界面绘制
(require 'qingeditor-editor)

(defun qingeditor/core/boot/init ()
  "`qingeditor'初始化入口函数。"
  ;; (when qingeditor/debugp (qingeditor/debug/init))
  ;; 当advised函数被重新定义的时候`ad-handle-definition'不输出任何东西
  (setq ad-redefinition-action 'accept)
  ;; 为了更流畅的用户体验 (更少的图形显示问题)
  (hidden-mode-line-mode)
  (qingeditor/ui/editor/remove-unused-gui-elements)
  ;; 为在避免在Windows系统中弹窗，我们显式设置`coding system'
  (prefer-coding-system 'utf-8)
  ;; TODO 如果`evil'不在我们的引导配置层的话,我们需要移除这个变量。
  (setq-default evil-want-C-u-scroll t
		;; 将`evil-want-C-i-jump'设置成`nil'是为了避免`TAB'在终端模式下被覆盖。使用`<C-i>'代替。
		evil-want-C-i-jump nil)
  (qingeditor/core/user-cfg/load-user-cfg-file)
  (qingeditor/core/call-func qingeditor/user-cfg-init "正在调用配置脚本初始化函数...")
  (qingeditor/core/call-func qingeditor/user-cfg-custom-init "正在调用配置脚本自定义初始化函数...")
  (setq qingeditor/user-cfg/editing-style
	(qingeditor/core/user-cfg/read-editing-style-cfg qingeditor/core/user-cfg/editing-style))
  (qingeditor/pkg/installer/initialize)
  ;; 设置默认的主题
  (let ((default-theme (car qingeditor/core/user-cfg/themes)))
    (qingeditor/ui/editor-theme/load-theme default-theme)
    (setq qingeditor/pkg/installer/protected-packages
	  (append
	   (delq nil (mapcar
		      (lambda (theme)
			(qingeditor/ui/editor-theme/get-theme-pkg-by-name theme))
		      qingeditor/core/user-cfg/themes))
	   qingeditor/pkg/installer/protected-packages))
    (setq-default qingeditor/ui/editor-theme/cur-theme-private default-theme)
    (setq-default qingeditor/ui/editor-theme/cycle-themes-private
		  (cdr qingeditor/core/user-cfg/themes)))
  ;; 设置字体
  (qingeditor/ui/editor/render-loading-text)
  (qingeditor/ui/do-after-display-system-init
   ;; 不要删除这里的 `(message)'函数调用，一定要三思，如果你删除了，你会让那些使用Emacs守护模式的程序猿很不爽
   ;; 他们设置的客户端字体将不会再第一个实例化EmacsClient的时候使用或者至少看着不像他们设置的字体
   ;; 如果你不相信我，那么你删除把，如果如我所说把下面的计数器加1，以增加我说的话的分量。
   ;; Counter = 1
   (qingeditor/core/message "正在设置qingeditor字体...")
   (unless (qingeditor/ui/editor-font/set-default-font qingeditor/core/user-cfg/default-font)
     (qingeditor/core/io/warning "没有找到指定的字体(%s)！字体配置可能不正确。"
				 (if (listp (car qingeditor/core/user-cfg/default-font))
				     (mapconcat 'car qingeditor/core/user-cfg/default-font ", ")
				   (car qingeditor/core/user-cfg/default-font)))))
  ;; 开始初始化`qingeditor'
  (setq inhibit-startup-screen t)
  (qingeditor/ui/editor/register-window-setup-hooks)
  (qingeditor/ui/editor/draw)
  (unless (display-graphic-p)
    ;; 当时命令行启动，为了能正确的显示logo我们再次手动创建QwsEditor buffer
    (qingeditor/ui/do-after-display-system-init
     (kill-buffer (get-buffer qingeditor/ui/editor/buffer-name))
     (qingeditor/ui/editor/draw)))
  ;; 设置这个参数，为了在启动期间让`qingeditor'像命令行参数那样显示buffer
  (setq initial-buffer-choice nil)
  (setq inhibit-startup-screen t)
  (unless (fboundp 'tool-bar-mode)
    (qingeditor/core/io/message (concat "当前的环境不支持图形渲染，"
					"你将不能开启Emacs的图形实例。")))
  ;; 检查新版本符号
  (if qingeditor/core/user-cfg/show-mode-line-unicode-symbols
      (setq qingeditor/pkg/release-mgr/version-check-lighter "[⇪]")))

(defun qingeditor/core/boot/setup-startup-hook ()
  "这个函数处理一些layer配置层加载完成之后的一些设置。"
  (add-hook
   'emacs-startup-hook
   (lambda ()
     ;; 在这个地方设置`init-buffer-choice'便于emacs client显示`qingeditor'的欢迎界面
     ;; 在这个步骤之前我们将`init-buffer-choice'设置为`nil'是为了正常去处理命令行参数
     (setq initial-buffer-choice (lambda () (get-buffer qingeditor/ui/editor/buffer)))
     ;; 调用配置脚本中定义的配置设置函数
     (qingeditor/core/call-func qws/user-cfg-setup "正在调用配置文件用户自定义配置函数...")
     (run-hooks 'qingeditor/gvars/post-user-cfg-hook)
     (setq qingeditor/gvars/post-user-cfg-hook-run t)
     (when (fboundp qingeditor/core/user-cfg/scratch-mode)
       (with-current-buffer "*scratch*"
         (funcall qingeditor/core/user-cfg/scratch-mode)))
     (qingeditor/ui/editor/display-summary qingeditor/start-time)
     (qingeditor/pkg/release-mgr/check-for-new-version
      nil qingeditor/pkg/release-mgr/version-check-interval)
     (setq qingeditor/core/runtime/initialized t))))

(provide 'qingeditor-boot)
