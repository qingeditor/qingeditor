;; 定义一些跟`qingeditor'的全局信息

(defvar qingeditor/core/editor-info/version nil
  "当前`qingeditor'的版本号")

(defun qingeditor/core/editor-info/version ()
  "获取`qingeditor'的版本号。"
  (interactive)
  (message (format "qingeditor %s@%s (%s)"
	  qingeditor/version
	  emacs-version
	  qingeditor/core/user-cfg/distribution)))

(provide 'qingeditor-editor-info)
