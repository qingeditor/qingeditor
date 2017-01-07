;; 定义一些输入输出的函数
(require 'qingeditor-funcs)

(defvar qingeditor/core/io/warnings nil "记录启动过程中出现的`warning'信息。")

(defun qingeditor/core/io/message (msg &rest args)
  "在`*Message*' buffer里面显示一条以(QingEditor)开头的信息。只有当`init-file-debug'不为`nil'的时候
才显示相应的信息。"
  (when init-file-debug
    (qingeditor/core/message "(QingEditor) %s" (apply 'format msg args))))

(defun qingeditor/core/io/warning (msg &rest args)
  "在`*Message*' buffer中显示一条告警信息 `MSG'，传入的`MSG'会无条件显示。"
  (let ((msg (apply 'format msg args)))
    (qingeditor/core/message "(QingEditor) Warning: %s" msg)
    (add-to-list 'qingeditor/core/io/warnings msg 'append)))

(provide 'qingeditor-io)
