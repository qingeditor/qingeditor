;; 瞬时状态的相关的函数定义

(defun qingeditor/core/transient-state/func-name (name)
  "返回`transient-state'的函数名字符号。"
  (intern (format "qingeditor/transient-state/%S-transient-state" name)))

(defun qingeditor/core/transient-state/props-var-name (name)
  "返回保存`transient-state'状态属性的变量名称符号。"
  (intern (format "qingeditor/transient-state/%S-transient-state-props" name)))

(defun qingeditor/core/transient-state/body-func-name (name)
  "获取`transient state' body函数名符号。"
  (intern (format "qingeditor/transient-state/%S-transient-state-body" name)))

(defun qingeditor/core/transient-state/heads-name (name)
  "获取`transient state'中保存按键绑定的`heads'的符号名称。"
  (intern (format "qingeditor/transient-state/%S-transient-state-heads" name)))

(defun qingeditor/core/transient-state/adjust-bindings (bindings to-remove to-add)
  (append
   (cl-remove-if
    (lambda (bind-item)
      (and (boundp to-remove)
           (listp (symbol-value to-remove))
           (member (car bind-item) (symbol-value to-remove))))
    bindings)
   (when (and (boundp to-add)
              (listp (symbol-value to-add)))
     (symbol-value to-add))))

(defun qingeditor/core/transient-state/make-doc (transient-state docstring &optional body)
  "使用内部的`hydra'函数格式化和apply传入的`docstring'。"
  (let ((heads (qingeditor/transient-state/heads-name transient-state)))
    (setq body (if body body '(nil nil :hint nil :foreign-keys nil)))
    (eval
     (hydra--format nil body docstring (symbol-value heads)))))

(defun qingeditor/core/transient-state/format-hint (name var hint)
  "格式化`hit'并且将`var'的内容保存在`transient state'的符号中。"
  (declare (indent 1))
  `(add-hook 'qingeditor/gvars/post-user-cfg-hook
             (lambda ()
               (let* ((props-var ,(qingeditor/transient-state/props-var-name name))
                      (prop-hint (cadr (assq 'hint props-var)))
                      (prop-columns (cadr (assq 'columns props-var)))
                      (prop-foreign-keys (cadr (assq 'foreign-keys props-var)))
                      (prop-entry-sexp (cadr (assq 'entry-sexp props-var)))
                      (prop-exist-sexp (cadr (assq 'exit-sexp props-var))))
                 (setq ,valr (qingeditor/transient-state/make-doc
                             `,name
                             ,hint
                             `(nil
                               nil
                               :hint ,prop-hint
                               :columns ,prop-columns
                               :foreign-keys ,prop-foreign-keys
                               :body-pre ,prop-entry-sexp
                               :before-exit ,prop-exit-sexp)))))
             'append))

(defface qingeditor/core/transient-state/title-face
  `((t :inherit mode-line))
  "`transitent'状态标题的face。")

(defmacro qingeditor/core/transient-state/define-transient-state (name &rest props)
  "定义一个名称`name'的`transient state'。
`name'是一个符号。
    可用的属性有:
`:on-entry sexp'
    当`transient state'打开的时候求`sexp'的值。
`:on-exit sexp'
    当`transient state'关闭的时候求`sexp'的值。
`:doc string或者sexp'
    指定一个`defhydra'支持的文档字符串。
`:additional-docs cons cells (variable . string)'
    保存在变量`variable'里面额外的docstring，这个可以用来动态切换docstring。
`:title'
    指定显示在`transient state'顶部的标题。
`:columns integer'
    指定动态生成`:doc'的时候使用的`columns'的值。
`:hint boolean'
    是否显示提示的信息。默认为`nil'。
`:hint-is-doc boolean'
    是否将`hit'提示信息显示成为文档，这个值得唯一的作用是显示`hit'信息的显示位置，如果不为
`nil'，将在`:title'的同一行进行显示，否则显示在他的下面。默认为`nil'。
`:dynamic-hint sexp'
    动态的对`sexp'进行求值，得到一个字符串或者提示信息，这个关键字指定的时候会使`:hint'关键词属性，
默认为`nil'。
`:foreign-keys symbol'
    当在`transient state'的时候，输入的按键没有绑定模型的行为，这个值可以为`nil',代表意思是直接退出
`transient state'，或者指定为`warn'，代表意思是警告用户当前的按键不存在但是不退出，或者`run'代表
不退出`transient'状态并且运行按键绑定。
`:bindings expressions'
    表达式`expressions'的格式如下：
\(string1 symbol1 docstring :exit symbol\)
解释：
- string1 是一个按键的`key'它绑定到一个函数或者一个`key map'。
- docstring 是一个字符串或者一个求值可以得到字符串的`sexp'表达式。
- :exit 不为`nil'的话，按下这个值指定的按键我们将退出`transient state'
重要请示：
因为内部实`transient maps'的缘故，关键字`:exit'求值发生在bound命令值之前。
函数`qingeditor/core/key-binder/create-key-binding-form'支持的的属性，在这里都可以使用。"
  (declare (indent 1))
  (let* ((func (qingeditor/core/transient-state/func-name name))
         (props-var (qingeditor/core/transient-state/props-var-name name))
         (body-func (qingeditor/core/transient-state/body-func-name name))
         (add-bindings
          (intern (format "qingeditor/transient-state/%S-transient-state-add-bindings" name)))
         (remove-bindings
          (intern (format "qingeditor/transient-state/%S-transient-state-remove-bindings" name)))
         (bindings (qingeditor/core/mplist-get props :bindings))
         (doc (or (plist-get props :doc) "\n"))
         (title (plist-get props :title))
         (hint-var (intern (format "%s-hint" func)))
         (columns (plist-get props :columns))
         (entry-sexp (plist-get props :on-enter))
         (exit-sexp (plist-get props :on-exit))
         (hint (plist-get props :hint))
         (hint-doc-p (plist-get props :hint-is-doc))
         (dynamic-hint (plist-get props :dynamic-hint))
         (additional-docs (qingeditor/core/mplist-get props :additional-docs))
         (foreign-keys (plist-get props :foreign-keys))
         (bind-keys (qingeditor/core/key-binder/create-key-binding-form props body-func)))
    `(progn
       (defvar ,props-var nil
         ,(format (concat "一个包含`transient state'%S一些属相的关联列表，"
                          "这些属性可以用在宏`qingeditor/core/transient-state/format-hint'里面。") name))
       (add-to-list ',props-var '(hint ,hint))
       (add-to-list ',props-var '(columns ,columns))
       (add-to-list ',props-var '(foreign-keys ,foreign-keys))
       (add-to-list ',props-var '(entry-sexp ,entry-sexp))
       (add-to-list ',props-var '(exit-sexp ,exit-sexp))
       (qingeditor/core/user-cfg/defer-until-after-user-cfg-ready
        '(lambda ()
           (eval
            (append
             '(defhydra ,func
                (nil nil
                     :hint ,hint
                     :columns ,columns
                     :foreign-keys ,foreign-keys
                     :body-pre ,entry-sexp
                     :before-exit ,exit-sexp)
                ,doc)
             (qingeditor/core/transient-state/adjust-bindings
              ',bindings ',remove-bindings ',add-bindings)))
           (when ,title
             (let ((guide (concat "[" (propertize "KEY" 'face 'hydra-face-blue)
                                  "] 退出状态 ["
                                  (if ',foreign-keys
                                      (propertize "KEY" 'face 'hydra-face-pink)
                                    (propertize "KEY" 'face 'hydra-face-red))
                                  "] 将不会退出")))
               ;; (add-face-text-property 0 (length guide) '(:height 0.9) t guide)
               (add-face-text-property 0 (length guide) 'italic t guide)
               (setq ,hint-var
                     (list 'concat
                           (when qingeditor/core/user-cfg/show-transient-state-title
                             (concat
                              (propertize
                               ,title
                               'face 'qingeditor/core/transient-state/title-face)
                              (if ,hint-doc-p " " "\n"))) ',hint-var
                              ',dynamic-hint
                              (when qingeditor/core/user-cfg/show-transient-state-color-guide
                                (concat "\n" guide))))
               ))
           ,@bind-keys)))))

(provide 'qingeditor-transient-state)
