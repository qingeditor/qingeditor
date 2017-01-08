;; `qingeditor'配置相关的函数定义
(require 'qingeditor-std-dir)

(let* ((env (getenv "QINGEDITORDIR"))
       (env-dir (when env (expand-file-name (concat env "/"))))
       (env-init (and env-dir (expand-file-name "init.el" env-dir)))
       (no-env-dir-default (expand-file-name
			    (concat qingeditor/user-home-dir ".qingeditor.d/")))
       (default-init (expand-file-name ".qingeditor" qingeditor/user-home-dir)))
  (defconst qingeditor/core/user-cfg/target-cfg-dir
    (cond
     ((and env (file-exists-p env-dir))
      env-dir)
     ((file-exists-p no-env-dir-default)
      no-env-dir-default)
     (t nil))
    "首先判断环境变量指定的文件夹，然后判断是否存在`~/.qingeditor.d/'文件夹是否存在，如果
都存在返回`nil'。")

  (defconst qingeditor/core/user-cfg/target-cfg-filename
    (let ((qingeditor-dir-init (when qingeditor/core/user-cfg/target-cfg-dir
				 (concat qingeditor/core/user-cfg/target-cfg-dir
					 "init.el"))))
      (cond
       (env-init)
       ((file-exists-p default-init) default-init)
       ((and qingeditor/core/user-cfg/target-cfg-dir
	     (file-exists-p qingeditor-dir-init))
	qingeditor-dir-init)
       (t default-init)))
    "首先是保证环境用户自己指定的初始化脚本
     然后是`~/.qingeditor'
     然后是默认的`~/.qingeditor.d/init.el'
     最后是强行指定为`~/.qingeditor'。"))

(defvar qingeditor/core/user-cfg/distribution 'editor-standard
  "最基本的layer层，当前包含两个组件 `editor-base' 或者 `editor-standard'。")

(defvar qingeditor/core/user-cfg/elpa-https t
  "是否通过https连接GNU的ELPA软件仓库, 如果您的环境不支持https请设置成`nil',强烈建议您将此参数设置成`t'。")

(defvar qingeditor/core/user-cfg/elpa-timeout 5  "ELPA软件仓库的连接超时时间。")

(defvar qingeditor/core/user-cfg/elpa-subdir nil "是否按照Emacs的版本号建立各自的ELAP库的文件夹。")

(defvar qingeditor/core/user-cfg/cfg-layer-dir '() "`qingeditor'的layer层存放的文件夹，必须以`/'结尾。")

(defvar qingeditor/core/user-cfg/install-packages 'used-only
  "`qingeditor'安装`packages'选项，有如下值`used-only', `used-but-keep-unused'和`all'.
 `used=only'只安装需要的`packages'，卸载不需要的`packages'和这些`packages'
依赖的`packages'。`used-but-keep-unused'安装需要的`packages'，但是不卸载不需要的`packages'，
`all'安装所有的`packages'，从不卸载`packages'。")

(defvar qingeditor/core/user-cfg/lazy-installation-type 'unused
  "迟延安装`qingeditor'的配置层选项，有如下几种选项`all', `unused'和`nil'.`unused'选项只迟延
安装不在`qingeditor/core/user-cfg/install-packages/cfg-layers'里面的层。`all'迟延安装所有的配置层哪怕配置层在
`qingeditor/core/user-cfg/install-packages/cfg-layers'里面。`nil'不迟延安装所有的配置层，如果您需要安装某个配置层您需要将其
添加到`qingeditor/core/user-cfg/install-packages/cfg-layers'配置项里面。")

(defvar qingeditor/core/user-cfg/ask-for-lazy-installation t
  "当指定为`t'时候，迟延安装某个配置层时候`qingeditor'会去询问用户是否要进行安装。")

(defvar qingeditor/core/user-cfg/additional-packages '()
  "额外的不包含在任何配置层里面的`package'包，如果您需要配置这些`package'您可以考虑新建一个`layer'
当然您可以在`qingeditor/user-cfg'函数中进行相关的配置。")

(defvar qingeditor/core/user-cfg/editing-style 'emacs
  "`qingeditor'编辑风格，目前支持三个选项`emacs', `vim'和`hybrid'。
`hybrid'跟`vim'很相似，除了将`insert state'换成`hybrid state'并且使用`emacs'按键绑定。这个值也可以指定为一个list，像配置层那样
使用`:variables'关键字。详情请看文档中关于编辑风格相关的章节。")

(defvar qingeditor/core/user-cfg/startup-banner 'official
  "指定`qingeditor'启动的时候的banner图片，有三个选项：`official', `random'和自定义的字符串。
`offical'显示官方默认的banner图片，`random'随机从内置的banner图片 库选择一张。自定义字符串必须指定一张png格式的图片路径。如果这个
选项为`nil'的话不显示banner。")

(defvar qingeditor/core/user-cfg/scratch-mode 'text-mode
  "默认的`scratch buffer'默认的`major mode'。默认`text-mode'")

(defvar qingeditor/core/user-cfg/check-for-update nil
  "如果不为`nil' qingeditor的分支如果不是`devel'的话将在启动的时候去检测新版本。注意，我们
这里是通过访问`github'相关服务进行版本检测的。")

(defvar qingeditor/core/user-cfg/cfg-layers '(emacs-lisp)
  "`qingeditor'启动默认加载的配置层列表。")

(defvar qingeditor/core/user-cfg/themes '(spacemacs-dark spacemacs-light)
  "`qingeditor'的主题列表，第一个在启动时候默认选择，再运行时您可以通过`SPC T n'进行循环切换。")

(defvar qingeditor/core/user-cfg/colorize-cursor-according-to-state t
  "如果不为`nil' GUI Emacs将会对匹配的括号进行着色。")


(defvar qingeditor/core/user-cfg/emacs-leader-key "M-m"
  "`qingeditor'的`emacs state'和`insert state'默认的`leader key'。")

(defvar qingeditor/core/user-cfg/major-mode-emacs-leader-key  "C-M-m"
  "`qingeditor''在`emacs state'和`insert state'的leader key。")

(defvar qingeditor/core/user-cfg/emacs-command-key "SPC"
  "当按了`leader key'之后执行Emacs command (M-x)命令的`key'。")

(defvar qingeditor/core/user-cfg/distinguish-gui-tab nil
  "在GUI模式下是否区分`TAB'和`c-i'。")

(defvar qingeditor/core/user-cfg/distinguish-gui-ret nil
  "在GUI模式下是否区分`c-m'个`return'。")

(defvar qingeditor/core/user-cfg/default-font '("Source Code Pro"
						:size 14
						:weight normal
						:width normal
						:powerline-scale 1.1))
  

(defvar qingeditor/core/user-cfg/folding-method 'origami
  "默认的代码折叠方法，可选的值有`evil'和`origami'。")

(defvar qingeditor/core/user-cfg/default-layout-name
  "Default" "默认的布局名称。")

(defvar qingeditor/core/user-cfg/display-default-layout nil
  "是否在mode-line显示默认布局的名称。")

(defvar qingeditor/core/user-cfg/auto-resume-layouts nil
  "是否自动恢复上一次保存的布局。")

(defvar qingeditor/core/user-cfg/max-rollback-slots 5
  "在缓存中保存rollback slots最大的数量。")

(defvar qingeditor/core/user-cfg/helm-resize nil
  "如果不为`nil'那么helm模式将最小化使用占用空间。")

(defvar qingeditor/core/user-cfg/helm-no-header nil
  "如果不为`nil'，当结果就一个结果时候不显示helm header。")

(defvar qingeditor/core/user-cfg/helm-position 'bottom
  "指定helm在minibuffer的那个位置进行显示。")

(defvar qingeditor/core/user-cfg/helm-use-fuzzy 'always
  "模糊匹配选项，如果设置成`always'那个强制模糊匹配所有非异步的来源。如果
设置成`source'保留各自来源的模糊匹配的设置。`nil'禁止所有的数据来源的模糊匹配。")

(defvar qingeditor/core/user-cfg/large-file-size 2
  "超过这个阈值，`qingeditor'会提示用户进行选择是否打开大文件。打开大文件将使用literal模式。")

(defvar qingeditor/core/user-cfg/auto-save-file-location 'cache
  "auto-save保存文件的目标文件夹地址，可能的选项有`original'就在文件的当前文件夹进行保存。
`cache'在cache目录中进行保存，设置成`nil'就禁止auto-save功能。")

(defvar qingeditor/core/user-cfg/enable-paste-transient-state t
  "是否开启transient-mode，当开启时按`p'循环粘贴kill ring里面的内容。")

(defvar qingeditor/core/user-cfg/which-key-delay 0.4
  "按键只能提示的延迟时间，一般是最后一个按键完成开始算起。设置这个选项等价于
设置`which-key-idle-delay'。")

(defvar qingeditor/core/user-cfg/which-key-position 'bottom
  "which key弹出框的位置，可能的值有`bottom'和`right-then-bottom',当设置为
`right-then-bottom'先是尝试在右边进行显示，显示不了就在下面显示。")

(defvar qingeditor/core/user-cfg/loading-progress-bar t
  "设置为`t'将在`qingeditor'启动的时候显示一个进度条，这个选项开启可能会影响启动速度。")

(defvar qingeditor/core/user-cfg/fullscreen-at-startup nil "是否在初始化的时候全屏显示`qingeditor' frame，这个特性要求(Emacs 24.4+)。")

(defvar qingeditor/core/user-cfg/fullssreen-use-non-native nil
  "不使用原生的全屏接口，用于取消Mac OSX的全屏动画。")

(defvar qingeditor/core/user-cfg/maximized-at-startup nil
  "在`qingeditor'启动的时候最大化`qingeditor' frame，这个选项只在
`qingeditor/core/user-cfg/fullscreen-at-startup'为`nil'的时候起作用。")

(defvar qingeditor/core/user-cfg/active-transparency 90
  "设置当前frame的激活的或者选中的时候的透明度，取值范围为(0...100)。")

(defvar qingeditor/core/user-cfg/inactive-transparency 90
  "设置当前的frame没激活或者没有被选中的透明度，取值的范围为(0...100)。")

(defvar qingeditor/core/user-cfg/show-transient-state-title t
  "如果不为`nil'在transient states显示当前的frame的标题。")

(defvar qingeditor/core/user-cfg/show-transient-state-color-guide t
  "如果不为`nil',那么在transient keys显示彩色的提示信息。")

(defvar qingeditor/core/user-cfg/show-mode-line-unicode-symbols t
  "是否在mode-line显示unicode字符。")

(defvar qingeditor/core/user-cfg/smooth-scrolling t
  "是否开启平滑滚动，如果为`t'则使用原生接口平滑滚动代替Emacs的跳跃滚动方式。")

(defvar qingeditor/core/user-cfg/line-numbers nil
  "设置为`t'为`prog-mode'和`text-mode'开启行号，如果设置为`relative'则显示相对的行号。")

(defvar qingeditor/core/user-cfg/persistent-server nil
  "设置为`t'当用户退出的时候`qingeditor'会提示用户在退出编辑器的时候是否让Emacs服务器继续运行。")

(defvar qingeditor/core/user-cfg/smart-closing-parenthesis nil
  "当为`t'的时候会在insert mode自动插入相应的反向括号，当开启这个想选个的时候可以在`)'之前
按`c-q'暂时来禁止这个行为。(默认为`nil')。")

(defvar qingeditor/core/user-cfg/smartparens-strict-mode nil
  "如果这个选项不为`nil'的话`smartparens-strict-mode'将在编程模型。")

(defvar qingeditor/core/user-cfg/highlight-delimiters 'all
  "设置高亮显示分割符的范围，可能的作用域选项有`any', `current', `all' 或者 `nil'. 默认是`all'
  高亮显示所有的作用域，特别突出当前的作用域。")

(defvar qingeditor/core/user-cfg/whitespace-cleanup nil
  "是否在保存buffer的时候删除相关的空白符.可能的取值有`all', `trailing', `changed'或者`nil'。
`all'侵入式删除的空格，删除所有的空行和内部的连续的空格。`trailing'删除行尾部的空格。`changed'只删除有改动的行的空白。
`nil'关闭删除空格的功能。")

(defvar qingeditor/core/user-cfg/search-tools '("ag" "pt" "ack" "grep")
  "`qingeditor'支持的搜索工具的名称列表，系统将使用出现在列表第一个的工具。默认支持：
`ag', `pt', `ack'和`grep'。")

(defvar qingeditor/core/user-cfg/default-package-repository 'melpa-stable
  "`qingeditor'默认使用的包仓库的源，这个参数暂时还没有启用。")

(defvar qingeditor/core/user-cfg/startup-lists '((recents . 5)
                                          (projects . 7))
  "`qingeditor'欢迎页面的两个列表的显示调试
格式为 `(list-type . list-size)',如果为`nil'就不显示任何的列表，list-type支持的选项有：
`recents', `bookmarks', `projects', `agenda'和 `todos'。")

(defvar qingeditor/core/user-cfg/startup-buffer-responsive t
  "`qingeditor'的欢迎页是否响应系统的resize事件。")

(defvar qingeditor/core/user-cfg/excluded-packages '()
  "默认不被安装和加载的包的列表。")

(defvar qingeditor/core/user-cfg/frozen-packages '()
  "默认系统不会去升级的包列表，将如果升级可能会影响兼容性的时候可以将包名称加入到这个列表。")

(defvar qingeditor/core/user-cfg/verbose-loading nil
  "当这个配置值不为`nil'的时候，系统将在`*Message*' buffer里面显示加载进度。默认为`nil'。")

(defun qingeditor/core/user-cfg/user-cfg-filename ()
  "获取`qingeditor'配置文件的绝对路径。"
  qingeditor/core/user-cfg/target-cfg-filename)

(defun qingeditor/core/user-cfg/load-user-cfg-file ()
  "在家系统探测得到的`qingeditor'的配置文件。"
  (let ((cfg-filename (qingeditor/core/user-cfg/user-cfg-filename)))
    (message cfg-filename)
    (if (file-exists-p cfg-filename)
	(unless (with-demoted-errors "加载配置脚本错误：%S"
		  (load cfg-filename))
	  (qingeditor/core/user-cfg/load-tpl-user-cfg-file)))))

(defun qingeditor/core/user-cfg/load-tpl-user-cfg-file ()
  "当加载用户指定的配置脚本出错的时候调用本方法，加载一个默认正确的配置脚本。"
  ())

(defun qingeditor/core/user-cfg/read-editing-style-cfg (config)
  "读取编辑器编辑风格`config', 编辑风格对象可以是一个存编辑风格的`symbol', 或者是一个列表
列表的`car'是编辑风格，cdr是一个`:variables'关键字。"
  (cond
   ((symbolp config) config)
   ((listp config)
    (let ((variables (qingeditor/core/mplist-get config :variables)))
      (while variables
        (let ((var (pop variables)))
          (if (consp variables)
              (condition-case-unless-debug err
                  (set-default var (eval (pop variables)))
                ('error
                 (qingeditor/ui/editor/buffer-append
                  (format (concat "\n读取编辑风格的变量%s出现出错，(错误信息: %s)"
                                  "请确定一下是否quote变量名称。") var err))))
            (qingeditor/core/io/warning "变量%s没有指定值" var)))))
    (car config))))

(defun qingeditor/core/user-cfg/maybe-install-user-cfg-file ()
  "探测`qingeditor'的配置文件是否安装，如果没有安装，我们就认为是全新安装然后进行相关文件的安装。"
  (unless (file-exists-p qingeditor/core/user-cfg/target-cfg-filename)
    (qingeditor/ui/editor/set-mode-line " qingeditor配置脚本安装向导")
    (qingeditor/ui/editor/redisplay)
    (when (qingeditor/core/user-cfg/install-user-cfg-file 'with-wizard)
      (qingeditor/layer/layer/sync))))

(defun qingeditor/core/user-cfg/install-user-cfg-file (confirm)
  "安装用户配置脚本，如果脚本已经存在返回`nil'，如果`confirm'不为`nil'
那么在安装配置脚本之前会询问用户。"
  ;; 用户配置是`qingeditor'配置选项的plist，在这里我们进行搜索替换
  (let ((preferences
         (when confirm
           ;; editing style
           `(("editing-style 'emacs"
              ,(format
                "editing-style '%S"
                (qingeditor/core/user-cfg/ido-completing-read "选择您的偏好编辑风格"
                                         '(("神的编辑器 (emacs)" emacs)
                                           ("编辑器之神 (vim)" vim)))
                ))
             ("distribution 'editor-standard"
              ,(format
                "distribution '%S"
                (qingeditor/core/user-cfg/ido-completing-read "选择`qingeditor'的发行版类型"
                                         `(("标准的发行类型，也是推荐的类型 (editor-standard)" editor-standard)
                                           ( "最小化发行类型，最小化安装 (editor-base)" editor-base)))))
             ("helm"
              ,(qingeditor/core/user-cfg/ido-completing-read "选择一个自动补全的框架"
                                        '(("重量级全功能型补全框架 (helm)" "helm")
                                          ("轻量级但是也非常强大的框架 (ivy)" "ivy")
                                          ;; 到目前为止，如果不选的话只有发行类型为`qingeditor-base'的
                                          ;; 时候才可以。
                                          ("什么都不选 None (不推荐此选项)" ""))))))))
    (with-current-buffer (find-file-noselect
                          (concat qingeditor/template-dir
                                  ".qingeditor.template"))
      (dolist (p preferences)
        (goto-char (point-min))
        (re-search-forward (car p))
        (replace-match (cadr p)))
      (let* ((init-filename qingeditor/core/user-cfg/target-cfg-filename)
             (install
              (if (file-exists-p init-filename)
                  (y-or-n-p
                   (format "%s 已经存在，您想要覆盖它吗？"
                           init-filename)) t)))
        (when install
          (write-file init-filename)
          (qingeditor/core/message "配置脚本%s已经成功安装。" init-filename)
          t))))
  (load-file qingeditor/core/user-cfg/target-cfg-filename))

(defun qingeditor/core/user-cfg/ido-completing-read (prompt candidates)
  "调用`ido-completing-read'，传入`CANDIDATES'，键是显示出来的字符串，值是实际返回的值。"
  (let ((ido-max-window-height (1+ (length candidates))))
    (cadr (assoc (ido-completing-read prompt (mapcar 'car candidates))
                 candidates))))

(defun qingeditor/core/user-cfg/defer-until-after-user-cfg-ready (func)
  "如果函数否则注册钩子函数，当`user-cfg'加载完成后执行，通过`qingeditor/gvars/post-user-cfg-hook'执行。"
  (if qingeditor/gvars/post-user-cfg-hook-run
      (funcall func)
    (add-hook 'qingeditor/gvars/post-user-cfg-hook func)))

(provide 'qingeditor-user-cfg)
