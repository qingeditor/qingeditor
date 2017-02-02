;; 第二阶段初始化脚本
;; 这个阶段我们主要实例化初始化类，配置相关监听器以及加载全局函数
(require 'eieio)
(require 'qingeditor-config)
(require 'qingeditor-initializer)
(require 'qingeditor-hash-table)
(require 'qingeditor-eventmgr-shared-mgr)
(require 'qingeditor-eventmgr-mgr)

(defvar qingeditor/shared-eventmgr (qingeditor/eventmgr/shared-mgr)
  "The global shared event manager object.")

(defvar qingeditor/initializer-ref (qingeditor/initializer)
  "The global initializer object.")

(let ((listener))
  )

(let ((eventmgr (qingeditor/eventmgr/mgr/init qingeditor/shared-eventmgr))
      (initializer qingeditor/initializer-ref))
  (qingeditor/initializer/set-eventmgr initializer eventmgr)
  (qingeditor/initializer/init initializer))

(defmethod qingeditor/cls/attach)

(provide 'qingeditor-init-bootstrap)
