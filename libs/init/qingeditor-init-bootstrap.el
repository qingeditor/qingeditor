;; 第二阶段初始化脚本
;; 这个阶段我们主要实例化初始化类，配置相关监听器以及加载全局函数
(require 'eieio)
(require 'qingeditor-initializer)
(require 'qingeditor-hash-table)

(defvar qingeditor/initialzer nil "获取全局初始化管理器引用。")

(defvar qingeditor/init/initializer-maker-clousure
  (lambda ()
    (let ((initializer 1))
      (setq qingeditor/initialzer initializer)
      )))

(fset 'qingeditor/initialzer (funcall qingeditor/init/initializer-maker-clousure))
(makunbound 'qingeditor/init/initializer-maker-clousure)

(provide 'qingeditor-init-bootstrap)
