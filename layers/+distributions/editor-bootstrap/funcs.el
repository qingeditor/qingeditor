;; 定义一些`editor-bootstrap'配置层提供的函数

(defun qingeditor/editor-bootstrap/diminish-hook (_)
  "在Emacs的`mode-line'上显示一个减弱的小图标。"
  (let ((unicodep qingeditor/core/user-cfg/show-mode-line-unicode-symbols))
    (cl-loop for (mode uni nouni) in qingeditor/ui/editor-font/diminished-minor-modes
             do (diminish mode (if unicodep uni nouni)))))

(defun qingeditor/editor-bootstrap/hydra-key-doc-function (key key-width doc doc-width)
  "为`keys'自定义hint文档格式"
  (format (format "[%%%ds] %%%ds" key-width (- -1 doc-width))
          key doc))
