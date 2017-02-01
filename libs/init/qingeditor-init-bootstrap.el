;; 第二阶段初始化脚本
;; 这个阶段我们主要实例化初始化类，配置相关监听器以及加载全局函数
(require 'eieio)
(require 'qingeditor-cfg)
(require 'qingeditor-initializer)
(require 'qingeditor-hash-table)

(defvar qingeditor/shared-eventmgr (qingeditor/eventmgr/shared-mgr)
  "The global shared event manager object.")

(defvar qingeditor/initializer-ref (qingeditor/initializer)
  "The global initializer object.")

(let ((eventmgr (qingeditor/eventmgr/mgr/init qingeditor/shared-eventmgr))
      (initializer qingeditor/initializer-ref))
  (qingeditor/initializer/set-eventmgr initializer eventmgr))



(provide 'qingeditor-init-bootstrap)
