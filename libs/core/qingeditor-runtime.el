;; `qingeditor'运行时的信息
(defun qingeditor/runtime/system-is-mac ()
  (eq system-type 'darwin))

(defun qingeditor/runtime/system-is-linux ()
  (eq system-type 'gnu/linux))

(defun qingeditor/runtime/system-is-mswindows ()
  (eq system-type 'windows-nt))

(defun qingeditor/runtime/window-system-is-mac ()
  ;; 在Emacs 25+的mac `(window-system)'返回 ns
  (memq (window-system) '(mac ns)))

(defvar qingeditor/core/runtime/error-count nil
  "系统启动过程中出现的错误次数。")

(defvar qingeditor/core/runtime/initialized nil
  "标志变量，显示当前`qingeditor'是否已经初始化完成，这个字段会在`emacs-startup-hook'，里面进行设置。")

(defvar qingeditor/core/runtime/fresh-install (file-exists-p qingeditor/core/editor-cfg/target-cfg-filename)
  "判断是否是全新安装`qingeditor'。")

(defun qingeditor/core/runtime/increment-error-count ()
  "增加全局错误计数器。"
  (if qingeditor/core/runtime/error-count
      (setq qingeditor/core/runtime/error-count
	    (1+ qingeditor/core/runtime/error-count))
    (setq qingeditor/core/runtime/error-count 1)))

(provide 'qingeditor-runtime)
