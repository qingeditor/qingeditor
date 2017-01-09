(require 'qingeditor-funcs)

(defvar qingeditor/core/toggle/toggles '()
  "所有定义的toggles的列表，列表的元素结构是一个属性列表
\(name :func function :doc string :key string\)。")

(defmacro qingeditor/core/toggle/add-toggle (name &rest props)
  "增加一个叫做`name'的toggle。

这个宏创建如下几个函数：
- qingeditor/toggle-NAME 根据当前的状态打开或者关闭
- qingeditor/toggle-NAME-on 当前状态时关闭的时候就打开
- qingeditor/toggle-NAME-off 当前状态是打开就关闭当前状态

提供的属性如下 PROPS:
`:status EXPRESSION'
   指定一个表达式，通过求值获取当前的状态

`:if EXPRESSION'
                                                               如果`EXPRESSION'求值得到`nil'的话，不在更新`toggle'的状态

`:on BODY'
   当打开`toggle'的时候对`BODY'表达式求值

`:off BODY'
   当关闭`toggle'的时候对`BODY'表达式求

`:documentation STRING'
   指定`toggle'的描述字符串

`:prefix SYMBOL'
    SYMBOL is bound to the raw value of prefix-arg (same as calling
    (interactive \"P\")) in the wrapper function.

`:on-message EXPRESSION'
    当`toggle'打开的时候计算`EXPRESSION'然后显示出来。

`:mode SYMBOL'
    如果不为`nil'那么必须是一个`minor mode',这个将覆盖`:on', `:off'和`:status'。

所有函数`qingeditor/core/key-binder/create-key-binding-form'支持的属性都支持。
"
  (declare (indent 1))
  (let* ((wrapper-func (intern (format "qingeditor/toggle-%s"
                                       (symbol-name name))))
         (wrapper-func-status (intern (format "%s-p" wrapper-func)))
         (wrapper-func-on (intern (format "%s-on" wrapper-func)))
         (wrapper-func-off (intern (format "%s-off" wrapper-func)))
         (mode (plist-get props :mode))
         (status (or mode (plist-get props :if)))
         (condition (plist-get props :if))
         (doc (plist-get props :documentation))
         (on-body (if mode `((,mode)) (qingeditor/core/mplist-get props :on)))
         (off-body (if mode `((,mode -1)) (qingeditor/core/mplist-get props :off)))
         (prefix-arg-var (plist-get props :prefix))
         (on-message (plist-get props :on-message))
         (bind-keys (qingeditor/core/key-binder/create-key-binding-form props wrapper-func))
         ;; 当他们是列表和绑定的符号的时候计算条件和状态
         (status-eval `(and (or (and (symbolp ',status) (boundp ',status))
                                (listp ',status))
                            ,status)))
    `(progn
       (push (append '(,name) '(:function ,wrapper-func
                                          :predicate ,wrapper-func-status) ',props)
             qingeditor/core/toggle/toggles)
       ;; 打开函数
       (defun ,wrapper-func ,(if prefix-arg-var (list prefix-arg-var) ())
         ,(format "Toggle %s打开或者关闭。" (symbol-name name))
         ,(if prefix-arg-var '(interactive "P") (interactive))
         (if (or (null ',condition)
                 (and (or (and (symbolp ',condition) (boundp ',condition))
                          (listp ',condition))
                      ,condition))
             (if (,wrapper-func-status)
                 (progn ,@off-body
                        (when (called-interactively-p 'any)
                          (message ,(format "%s禁止。" name))))
               ,@on-body
               (when (called-interactively-p 'any)
                 (message ,(or on-message (format "%s开启。" name)))))
           (message "当前`Toggle'不被支持。")))
       ;; 函数断言
       (defun ,wrapper-func-status ()
         ,(format "如果%s打开就计算状态表达书。" (symbol-name name))
         ,status-eval)
       ;; 当状态可用的时候定义相关 `on-'跟`off-'函数
       ,@(when status
           ;; on-function
           `((defun ,wrapper-func-on ()
               ,(format "将%s打开。" (symbol-name name))
               (interactive)
               (unless (,wrapper-func-status) (,wrapper-func)))
             ;; off-function
             (defun ,wrapper-func-off ()
               ,(format "将%s关闭。" (symbol-name name))
               (interactive)
               (when (,wrapper-func-status) (,wrapper-func)))))
       ,@bind-keys)))

(provide 'qingeditor-toggle)
