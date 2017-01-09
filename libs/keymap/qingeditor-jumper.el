;; 这个类只要负责调转函数的管理

(defvar qingeditor/core/jumper/default-jump-handlers '()
  "全局适用的所有模块的跳转函数。")

(defvar-local qingeditor/core/jumper/jump-handlers '()
  "当前`buffer'中有效的跳转函数。")

(defmacro qingeditor/core/jumper/define-jump-handlers (mode &rest handlers)
  "为指定的`mode'定义跳转函数，函数会定义一个`qingeditor/core/jumper/jump-handlers-MODE'，这个变量保存
跳转函数，同时我们会定义一个钩子函数去设置`qingeditor/core/jumper/jump-handlers'变量。"
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "qingeditor/core/jumper/init-jump-handlers-%S" mode)))
        (handlers-list (intern (format "qingeditor/core/jumper/jump-handlers-%S"))))
    `(progn
       (defvar ,handler-list ',handlers
       ,(format (concat "定义指定`mode'：%S的跳转函数，这个里面的"
                        "优先级比`qingeditor/core/jumper/default-jump-handlers'要高。")
                mode))
       (defun ,func ()
         (setq qingeditor/core/jumper/jump-handlers
               (append ,handlers-list qingeditor/core/jumper/default-jump-handlers)))
       (add-hook ',mode-hook ',func)
       (with-eval-after-load 'bind-map
         (qingeditor/core/key-binder/set-leader-keys-for-major-mode
          ',mode "gg" 'qingeditor/core/jumper/jump-to-definition)))))

(defun qingeditor/core/jumper/jump-to-definition ()
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
          (old-point (point)))
      (dolist (handler? qingeditor/core/jump-handlers)
        (let ((handler (if (listp handler?) (car handler?) handler))
              (async (when (listp handler?)
                       (plist-get (cdr handler?) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or async
                    (not (eq old-point (point)))
                    (not (equal old-buffer (current-buffer))))
            (throw 'done t)))))
    (qingeditor/core/message "没有能找到这个`symbol'的跳转函数。")))

(with-eval-after-load 'evil
  (evil-set-command-property 'qingeditor/core/jumper/jump-to-definition :jump t))

(provide 'qingeditor-jumper)
