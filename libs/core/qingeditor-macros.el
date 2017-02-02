;; 定义一个`qingeditor'常用的宏

(require 'qingeditor-editor)

(defmacro qingeditor/core/call-func (func &optional msg)
  "如果`func'绑定了函数，那么在配置脚本中调用该的函数。"
  `(progn
     (when ,msg (qingeditor/core/message ,msg))
     (when (fboundp ',func)
       (condition-case-unless-debug err
           (,func)
         (error
          (qingeditor/core/increment-error-count)
          (qingeditor/ui/editor/buffer-append
                      (format "调用函数: %s出现错误，错误信息: %s\n"
                              ',(symbol-name func)
                              (error-message-string err))
                      t))))))

(provide 'qingeditor-macros)

