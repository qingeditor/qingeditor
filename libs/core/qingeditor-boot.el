;; 这个文件主要负责整个编辑器的初始化引导等函数的定义
;; 加载依赖的第三方库
(require 'subr-x nil 'noerror)
(require 'page-break-lines)
(require 'ht)
(require 'eieio)

;; 负责`qingeditor'用户界面绘制
(require 'qingeditor-editor)

(defun qingeditor/core/boot/init ()
  "`qingeditor'初始化入口函数。"
  ;; (when qingeditor/debugp (qingeditor/debug/init))
  ;; 当advised函数被重新定义的时候`ad-handle-definition'不输出任何东西
  (setq ad-redefinition-action 'accept)
  ;; 为了更流畅的用户体验 (更少的图形显示问题)
  (hidden-mode-line-mode)
  (qingeditor/ui/editor/remove-unused-gui-elements)
  )
(provide 'qingeditor-boot)
