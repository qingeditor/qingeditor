;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define `qingedtior' global configuration variables

(defvar qingeditor/config/distribution 'editor-standard
  "最基本的layer层，当前包含两个组件 `editor-base' 或者 `editor-standard'。")

(defvar qingeditor/config/elpa-https t
  "是否通过https连接GNU的ELPA软件仓库, 如果您的环境不支持https请设置成`nil',强烈建议您将此参数设置成`t'。")

(defvar qingeditor/config/elpa-timeout 5  "ELPA软件仓库的连接超时时间。")

(defvar qingeditor/config/elpa-subdir nil "是否按照Emacs的版本号建立各自的ELAP库的文件夹。")

(defvar qingeditor/config/cfg-layer-dir '() "`qingeditor'的layer层存放的文件夹，必须以`/'结尾。")

(defvar qingeditor/config/install-packages 'used-only
  "`qingeditor'安装`packages'选项，有如下值`used-only', `used-but-keep-unused'和`all'.
 `used=only'只安装需要的`packages'，卸载不需要的`packages'和这些`packages'
依赖的`packages'。`used-but-keep-unused'安装需要的`packages'，但是不卸载不需要的`packages'，
`all'安装所有的`packages'，从不卸载`packages'。")

(defvar qingeditor/config/lazy-installation-type 'unused
  "迟延安装`qingeditor'的配置层选项，有如下几种选项`all', `unused'和`nil'.`unused'选项只迟延
安装不在`qingeditor/core/user-cfg/install-packages/cfg-layers'里面的层。`all'迟延安装所有的配置层哪怕配置层在
`qingeditor/config/install-packages/cfg-layers'里面。`nil'不迟延安装所有的配置层，如果您需要安装某个配置层您需要将其
添加到`qingeditor/config/install-packages/cfg-layers'配置项里面。")

(defvar qingeditor/config/ask-for-lazy-installation t
  "当指定为`t'时候，迟延安装某个配置层时候`qingeditor'会去询问用户是否要进行安装。")

(defvar qingeditor/config/additional-packages '()
  "额外的不包含在任何配置层里面的`package'包，如果您需要配置这些`package'您可以考虑新建一个`layer'
当然您可以在`qingeditor/user-cfg'函数中进行相关的配置。")

(defvar qingeditor/config/editing-style 'emacs
  "`qingeditor'编辑风格，目前支持三个选项`emacs', `vim'和`hybrid'。
`hybrid'跟`vim'很相似，除了将`insert state'换成`hybrid state'并且使用`emacs'按键绑定。这个值也可以指定为一个list，像配置层那样
使用`:variables'关键字。详情请看文档中关于编辑风格相关的章节。")

(defvar qingeditor/config/startup-banner 'official
  "指定`qingeditor'启动的时候的banner图片，有三个选项：`official', `random'和自定义的字符串。
`offical'显示官方默认的banner图片，`random'随机从内置的banner图片 库选择一张。自定义字符串必须指定一张png格式的图片路径。如果这个
选项为`nil'的话不显示banner。")

(defvar qingeditor/config/scratch-mode 'text-mode
  "默认的`scratch buffer'默认的`major mode'。默认`text-mode'")

(defvar qingeditor/config/check-for-update nil
  "如果不为`nil' qingeditor的分支如果不是`devel'的话将在启动的时候去检测新版本。注意，我们
这里是通过访问`github'相关服务进行版本检测的。")

(defvar qingeditor/config/cfg-layers '(emacs-lisp)
  "`qingeditor'启动默认加载的配置层列表。")

(defvar qingeditor/config/themes '(spacemacs-dark spacemacs-light)
  "`qingeditor'的主题列表，第一个在启动时候默认选择，再运行时您可以通过`SPC T n'进行循环切换。")

(defvar qingeditor/config/colorize-cursor-according-to-state t
  "如果不为`nil' GUI Emacs将会对匹配的括号进行着色。")


(defvar qingeditor/config/emacs-leader-key "M-m"
  "`qingeditor'的`emacs state'和`insert state'默认的`leader key'。")

(defvar qingeditor/config/major-mode-emacs-leader-key  "C-M-m"
  "`qingeditor''在`emacs state'和`insert state'的leader key。")

(defvar qingeditor/config/emacs-command-key "SPC"
  "当按了`leader key'之后执行Emacs command (M-x)命令的`key'。")

(defvar qingeditor/config/distinguish-gui-tab nil
  "在GUI模式下是否区分`TAB'和`c-i'。")

(defvar qingeditor/config/distinguish-gui-ret nil
  "在GUI模式下是否区分`c-m'个`return'。")

(defvar qingeditor/core/config/default-font '("Source Code Pro"
						:size 14
						:weight normal
						:width normal
						:powerline-scale 1.1))
  

(defvar qingeditor/config/folding-method 'origami
  "默认的代码折叠方法，可选的值有`evil'和`origami'。")

(defvar qingeditor/config/default-layout-name
  "Default" "默认的布局名称。")

(defvar qingeditor/config/display-default-layout nil
  "是否在mode-line显示默认布局的名称。")

(defvar qingeditor/config/auto-resume-layouts nil
  "是否自动恢复上一次保存的布局。")

(defvar qingeditor/config/max-rollback-slots 5
  "在缓存中保存rollback slots最大的数量。")

(defvar qingeditor/config/helm-resize nil
  "如果不为`nil'那么helm模式将最小化使用占用空间。")

(defvar qingeditor/config/helm-no-header nil
  "如果不为`nil'，当结果就一个结果时候不显示helm header。")

(defvar qingeditor/config/helm-position 'bottom
  "指定helm在minibuffer的那个位置进行显示。")

(defvar qingeditor/config/helm-use-fuzzy 'always
  "模糊匹配选项，如果设置成`always'那个强制模糊匹配所有非异步的来源。如果
设置成`source'保留各自来源的模糊匹配的设置。`nil'禁止所有的数据来源的模糊匹配。")

(defvar qingeditor/config/large-file-size 2
  "超过这个阈值，`qingeditor'会提示用户进行选择是否打开大文件。打开大文件将使用literal模式。")

(defvar qingeditor/config/auto-save-file-location 'cache
  "auto-save保存文件的目标文件夹地址，可能的选项有`original'就在文件的当前文件夹进行保存。
`cache'在cache目录中进行保存，设置成`nil'就禁止auto-save功能。")

(defvar qingeditor/config/enable-paste-transient-state t
  "是否开启transient-mode，当开启时按`p'循环粘贴kill ring里面的内容。")

(defvar qingeditor/config/which-key-delay 0.4
  "按键只能提示的延迟时间，一般是最后一个按键完成开始算起。设置这个选项等价于
设置`which-key-idle-delay'。")

(defvar qingeditor/config/which-key-position 'bottom
  "which key弹出框的位置，可能的值有`bottom'和`right-then-bottom',当设置为
`right-then-bottom'先是尝试在右边进行显示，显示不了就在下面显示。")

(defvar qingeditor/config/loading-progress-bar t
  "设置为`t'将在`qingeditor'启动的时候显示一个进度条，这个选项开启可能会影响启动速度。")

(defvar qingeditor/config/fullscreen-at-startup nil "是否在初始化的时候全屏显示`qingeditor' frame，这个特性要求(Emacs 24.4+)。")

(defvar qingeditor/config/fullssreen-use-non-native nil
  "不使用原生的全屏接口，用于取消Mac OSX的全屏动画。")

(defvar qingeditor/config/maximized-at-startup nil
  "在`qingeditor'启动的时候最大化`qingeditor' frame，这个选项只在
`qingeditor/core/user-cfg/fullscreen-at-startup'为`nil'的时候起作用。")

(defvar qingeditor/config/active-transparency 90
  "设置当前frame的激活的或者选中的时候的透明度，取值范围为(0...100)。")

(defvar qingeditor/config/inactive-transparency 90
  "设置当前的frame没激活或者没有被选中的透明度，取值的范围为(0...100)。")

(defvar qingeditor/config/show-transient-state-title t
  "如果不为`nil'在transient states显示当前的frame的标题。")

(defvar qingeditor/config/show-transient-state-color-guide t
  "如果不为`nil',那么在transient keys显示彩色的提示信息。")

(defvar qingeditor/config/show-mode-line-unicode-symbols t
  "是否在mode-line显示unicode字符。")

(defvar qingeditor/config/smooth-scrolling t
  "是否开启平滑滚动，如果为`t'则使用原生接口平滑滚动代替Emacs的跳跃滚动方式。")

(defvar qingeditor/config/line-numbers nil
  "设置为`t'为`prog-mode'和`text-mode'开启行号，如果设置为`relative'则显示相对的行号。")

(defvar qingeditor/config/persistent-server nil
  "设置为`t'当用户退出的时候`qingeditor'会提示用户在退出编辑器的时候是否让Emacs服务器继续运行。")

(defvar qingeditor/config/smart-closing-parenthesis nil
  "当为`t'的时候会在insert mode自动插入相应的反向括号，当开启这个想选个的时候可以在`)'之前
按`c-q'暂时来禁止这个行为。(默认为`nil')。")

(defvar qingeditor/config/smartparens-strict-mode nil
  "如果这个选项不为`nil'的话`smartparens-strict-mode'将在编程模型。")

(defvar qingeditor/config/highlight-delimiters 'all
  "设置高亮显示分割符的范围，可能的作用域选项有`any', `current', `all' 或者 `nil'. 默认是`all'
  高亮显示所有的作用域，特别突出当前的作用域。")

(defvar qingeditor/config/whitespace-cleanup nil
  "是否在保存buffer的时候删除相关的空白符.可能的取值有`all', `trailing', `changed'或者`nil'。
`all'侵入式删除的空格，删除所有的空行和内部的连续的空格。`trailing'删除行尾部的空格。`changed'只删除有改动的行的空白。
`nil'关闭删除空格的功能。")

(defvar qingeditor/config/search-tools '("ag" "pt" "ack" "grep")
  "`qingeditor'支持的搜索工具的名称列表，系统将使用出现在列表第一个的工具。默认支持：
`ag', `pt', `ack'和`grep'。")

(defvar qingeditor/config/default-package-repository 'melpa-stable
  "`qingeditor'默认使用的包仓库的源，这个参数暂时还没有启用。")

(defvar qingeditor/config/startup-lists '((recents . 5)
                                          (projects . 7))
  "`qingeditor'欢迎页面的两个列表的显示调试
格式为 `(list-type . list-size)',如果为`nil'就不显示任何的列表，list-type支持的选项有：
`recents', `bookmarks', `projects', `agenda'和 `todos'。")

(defvar qingeditor/config/startup-buffer-responsive t
  "`qingeditor'的欢迎页是否响应系统的resize事件。")

(defvar qingeditor/config/excluded-packages '()
  "默认不被安装和加载的包的列表。")

(defvar qingeditor/config/frozen-packages '()
  "默认系统不会去升级的包列表，将如果升级可能会影响兼容性的时候可以将包名称加入到这个列表。")

(defvar qingeditor/config/verbose-loading nil
  "当这个配置值不为`nil'的时候，系统将在`*Message*' buffer里面显示加载进度。默认为`nil'。")

(provide 'qingeditor-user-cfg)
