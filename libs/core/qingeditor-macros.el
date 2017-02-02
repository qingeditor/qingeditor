;; 定义一个`qingeditor'常用的宏

(require 'qingeditor-editor)

(defmacro qingeditor/call-func (func &optional msg)
  "如果`func'绑定了函数，那么在配置脚本中调用该的函数。"
  `(progn
     (when ,msg (qingeditor/core/message ,msg))
     (when (fboundp ',func)
       (condition-case-unless-debug err
           (,func)
         (error
          (qingeditor/increment-error-count)
          (qingeditor/ui/editor/buffer-append
                      (format "调用函数: %s出现错误，错误信息: %s\n"
                              ',(symbol-name func)
                              (error-message-string err))
                      t))))))

(defmacro qingeditor/do-after-display-system-ready (&rest body)
  "If the display system is initialized, run `BODY', otherwise
we attach a listener to `qingeditor/display-system-ready-event' event."
  `(let ((init
          (cond
           ((boundp 'ns-initialized) ns-initialized)
           ;; w32-initialized gets set too early, so
           ;; if we're on Windows, check the list of fonts
           ;; instead (this is nil until the graphics system
           ;; is initialized)
           ((boundp 'w32-initialized) (font-family-list))
           ;; fallback to normal loading behavior only if in a GUI
           (t (display-graphic-p)))))
     (if init
         (progn
           ,@body)
       (push (qingeditor/cls/attach
              qingeditor/shared-eventmgr
              qingeditor/display-system-ready-event
              (qingeditor/eventmgr/event-handler/init (lambda ()
                                                        ,@body)))
             qingeditor/global-listeners-pool))))

(provide 'qingeditor-macros)

