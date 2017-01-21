(require 'eieio)

(defconst qingeditor/start-dir user-emacs-directory
  "qingeditor标准的起始目录, 一般是 `~/.emacs.d/'文件夹")

(defconst qingeditor/libs-dir
  (expand-file-name (concat qingeditor/start-dir "libs/"))
  "qingeditor核心库文件夹路径")
  
;; 定义一个函数，将制定的文件夹全部加入到`load-path'里面去, 在这里我们只要将
;; 标准的libs路径全部将入就可以了，其余的由框架内部函数进行二次加载
(defun qingeditor/register-target-dir-to-load-path (target-dir)
  "将制定的文件夹全部加入到`load-path'里面去"
  (let ((dir-items (directory-files target-dir t)))
    ;; 添加本身
    (if (file-directory-p target-dir)
        (add-to-list 'load-path target-dir))
    (dolist (item dir-items)
      (if (and (not (string-equal "." (substring item -1 )))
               (file-directory-p item))
          (qingeditor/register-target-dir-to-load-path item)))))

(qingeditor/register-target-dir-to-load-path qingeditor/libs-dir)
