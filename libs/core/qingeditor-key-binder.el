;; 按键绑定相关函数

(require 'qingeditor-funcs)

(defvar qingeditor/core/key-binder/prefix-titles nil
  "将命令的缩写映射到全名的`alist'。")

(defvar qingeditor/core/key-binder/default-map (make-sparse-keymap)
  "所有的qingeditor leader按键命令的基础`keymap'。")

(defun qingeditor/core/key-binder/translate-C-i (_)
  "If `qingeditor/core/user-cfg/distinguish-gui-tab' is non nil, the raw key
sequence does not include <tab> or <kp-tab>, and we are in the
gui, translate to [C-i]. Otherwise, [9] (TAB)."
  (interactive)
  (if (and (not (cl-position 'tab (this-single-command-raw-keys)))
           (not (cl-position 'kp-tab (this-single-command-raw-keys)))
           qingeditor/core/user-cfg/distinguish-gui-tab
           (display-graphic-p))
      [C-i] [?\C-i]))

(define-key key-translation-map [?\C-i] 'qingeditor/core/key-binder/translate-C-i)

;; (defun qingeditor/key-binder/translate-C-m (_)
;;   "If `qingeditor/core/user-cfg/distinguish-gui-ret' is non nil, the raw key
;; sequence does not include <ret>, and we are in the gui, translate
;; to [C-m]. Otherwise, [9] (TAB)."
;;   (interactive)
;;   (if (and
;;        (not (cl-position 'return (this-single-command-raw-keys)))
;;        (not (cl-position 'kp-enter (this-single-command-raw-keys)))
;;        qingeditor/core/user-cfg/distinguish-gui-ret
;;        (display-graphic-p))
;;     [C-m] [?\C-m]))
;; (define-key key-translation-map [?\C-m] 'qingeditor/key-binder/translate-C-m)

(defun qingeditor/core/key-binder/declare-prefix (prefix name &optional long-name)
  "定义一个前缀`prefix',`prefix'是描述一个按键序列的，`name'是一前缀命令的名字。如果
`long-name'不为空那么他保存在`qingeditor/core/key-binder/prefix-titles'中。"
  (let* ((command name)
         (full-prefix (concat qingeditor/core/user-cfg/leader-key " " prefix))
         (full-prefix-emacs (concat qingeditor/core/user-cfg/emacs-leader-key " " prefix))
         (full-prefix-lst (listfy-key-sequence (kbd full-prefix)))
         (full-prefix-emacs-lst (listify-key-sequence (kbd full-prefix-emacs))))
    ;; 如果指定的前缀命令不存在就定义
    (unless long-name
      (setq long-name name))
    (which-key-declare-prefixes
      full-prefix-emacs (cons name long-name)
      full-prefix (cons name long-name))))

(defun qingeditor/core/key-binder/declare-prefix-for-mode (mode prefix name &optional long-name)
  "为指定的`mode'定义前缀命令，`prefix'用来描述这个前缀命令，`name'是一个符号名，用作前缀命令。"
  (let ((command (intern (concat (symbol-name mode) name)))
        (full-prefix (concat qingeditor/core/user-cfg/leader-key " " prefix))
        (full-prefix-emacs (concat qingeditor/core/user-cfg/emacs-leader-key " " prefix))
        (is-major-mode-prefix (string-prefix-p "m" prefix))
        (major-mode-prefix (concat qingeditor/core/user-cfg/major-mode-leader-key " " (substring prefix 1)))
        (major-mode-prefix-emacs
         (concat qingeditor/core/user-cfg/major-mode-emacs-leader-key " " (substring prefix 1))))
    (unless long-name (setq long-name name))
    (let ((prefix-name (cons name long-name)))
      (which-key-declare-prefixes-for-mode mode
        full-prefix-emacs prefix-name
        full-prefix prefix-name)
      (when (and is-major-mode-prefix qingeditor/core/user-cfg/major-mode-leader-key)
        (which-key-declare-prefixes-for-mode mode major-mode-prefix prefix-name))
      (when (and is-major-mode-prefix qingeditor/core/user-cfg/major-mode-emacs-leader-key)
        (which-key-declare-prefixes-for-mode mode major-mode-prefix-emacs prefix-name)))))

(defun qingeditor/core/key-binder/set-leader-keys (key def &rest bindings)
  "在`qingeditor/core/user-cfg/leader-key'和`qingeditor/core/user-cfg/emacs-leader-key'
下面添加按键`key'和这个按键的绑定。`key'必须是一个能用在`kbd'里面的字符串，`def'是一个命令的名称。
详情可以看`define-key'，这个函数文档有对`def'格式有详细的描述。
为了方便，这个函数接受一下的键值对

\(qingeditor/core/key-binder/set-leader-keys
    \"a\" 'command1
    \"C-c\" 'command2
    \"bb\" 'command3\)"
  (while key
    (define-key qingeditor/core/key-binder/default-map (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))
(put 'qingeditor/core/key-binder/set-leader-keys 'lisp-indent-function 'defun)

(defun qingeditor/core/key-binder/acceptable-leader-p (key)
  "当`key'是一个字符串并且不为空的时候返回`t'。"
  (and (stringp key) (not (string= key ""))))

(defun qingeditor/core/key-binder/init-leader-mode-map (mode map &optional minor)
  "检查`map-prefix',如果不存在，使用`bind-map'创建并且绑定到`qingeditor/core/user-cfg/major-mode-leader-key'
和`qingeditor/core/user-cfg/major-mode-emacs-leader-key'。如果`mode'是minor mode，那么第三个
参数不能为`nil'。"
  (let* ((prefix (intern (format "%s-prefix" map)))
         (emacs-leader1 (when (qingeditor/core/key-binder/acceptable-leader-p
                               qingeditor/core/user-cfg/major-mode-emacs-leader-key)
                          qingeditor/core/user-cfg/major-mode-emacs-leader-key))
         (emacs-leader2 (when (qingeditor/core/key-binder/acceptable-leader-p
                               qingeditor/core/user-cfg/emacs-leader-key)
                          (concat qingeditor/core/user-cfg/emacs-leader-key
                                  (unless minor " m"))))
         (emacs-leaders (delq nil (list emacs-leader1 emacs-leader2))))
    (or (boundp prefix)
        (progn
          (eval
           `(bind-map ,map
              :prefix-cmd ,prefix
              ,(if minor :minor-modes :major-modes) (,mode)
              :keys ,emacs-leaders))
          (boundp prefix)))))

(defun qingeditor/core/key-binder/set-leader-keys-for-major-mode (mode key def &rest bindings)
  "在`qingeditor/core/user-cfg/major-mode-leader-key'和
`qingeditor/core/user-cfg/major-mode-emacs-leader-key'
下为`mode'增加按键绑定`key'->`def',`mode'必须是一个quote符号并且是major mode。其他的参数函数跟在
函数`qingeditor/core/kye-binder/set-leader-keys'里面一样。"
  (let* ((map (intern (format "qingeditor-%s-map" mode))))
    (when (qingeditor/core/key-binder/init-leader-mode-map mode map)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings)
              def (pop bindings))))))
(put 'qingeditor/core/key-bninder/set-leader-keys-for-major-mode 'lisp-indent-function 'defun)

(defun qingeditor/core/key-binder/set-leader-keys-for-minor-mode (mode key def &rest bindings)
  "在`qingeditor/core/user-cfg/major-mode-leader-key'和
`qingeditor/core/user-cfg/major-mode-emacs-leader-key'
下为`mode'增加按键绑定`key'->`def',`mode'必须是一个quote符号并且是minor mode。其他的参数函数跟在
函数`qingeditor/core/kye-binder/set-leader-keys'里面一样。"
  (let* ((map (intern (format "qingeditor-%s-map" mode))))
    (when (qingeditor/core/key-binder/init-leader-mode-map mode map t)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))

(put 'qingeditor/core/key-binder/set-leader-keys-for-minor-mode 'lisp-indent-function 'defun)

(defun qingeditor/core/key-binder/create-key-binding-form (props func)
  "帮助函数，根据`props'绑定一个`key'到`func'。
支持的属性有：

`:global-key string'
     一个或者多个按键序列，调用`global-set-key'进行设置。

`:define-key cons cell'
     一个或者多个cons cell \(MAP . KEY\)，其中`map'是一个`keymap',`key'是一个
按键序列，使用`define-key'进行设置。"
  (let ((global-key (qingeditor/core/mplist-get props :global-key))
        (def-key (qingeditor/core/mplist-get props :define-key)))
    (append
     (when global-key
       `((dolist (key ',global-key)
           (global-set-key (kbd key) ',func))))
     (when def-key
       `((dolist (val ',def-key)
           (define-key (eval (car val)) (kbd (cdr val)) ',func)))))))

(provide 'qingeditor-key-binder)
