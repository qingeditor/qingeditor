;; 第二阶段初始化脚本
;; 这个阶段我们主要实例化初始化类，配置相关监听器以及加载全局函数
(require 'eieio)
(require 'qingeditor-gvars)
(require 'qingeditor-funcs)
(require 'qingeditor-config)
(require 'qingeditor-initializer)
(require 'qingeditor-hash-table)
(require 'qingeditor-eventmgr-shared-mgr)
(require 'qingeditor-eventmgr-mgr)
(require 'qingeditor-emacs-setup-listener)
(require 'qingeditor-hidden-mode-line-mode)

(defvar qingeditor/shared-eventmgr (qingeditor/eventmgr/shared-mgr)
  "The global shared event manager object.")

;; setup shared eventmgr
;; we attach some important global event listeners
(defadvice server-create-window-system-frame
    (after qingeditor/advice/init-display activate)
  "After emacs server create a frame, we dispatch global event
`qingeditor/display-system-ready-event', you can attach listener
to this event if you want to do some that needs to have the display system
initialized."
  (progn
    (qingeditor/cls/trigger
     qingeditor/shared-eventmgr qingeditor/display-system-ready-event)
    (ad-disable-advice 'server-create-window-system-frame
                       'after
                       'qingeditor/advice/init-display)
    (ad-activate 'server-create-window-system-frame)))

(defvar qingeditor/initializer-ref (qingeditor/initializer)
  "The global initializer object.")

;; setup default event listeners
(qingeditor/cls/add-default-listener
 qingeditor/initializer-ref "emacs-setup" (qingeditor/init/emacs-setup-listener))

(let ((eventmgr (qingeditor/eventmgr/mgr/init qingeditor/shared-eventmgr))
      (initializer qingeditor/initializer-ref))
  (qingeditor/cls/set-eventmgr initializer eventmgr)
  (qingeditor/cls/init initializer)
  (qingeditor/cls/bootstrap initializer))

(provide 'qingeditor-init-bootstrap)
