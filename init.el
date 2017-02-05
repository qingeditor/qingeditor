;; qing editor 初始化入口文件，主要将核心库加入到 `load-path' 里面来，然后将控制权交由库本身的初始化函数
;; 进行进一步初始化
;; 加载最基本的核心库

;; 没有这个注释 emacs25 会在这里自动添加 (package-initialize)
;; (package-initialize)

(setq gc-cons-threshold 100000000)
(load-file (expand-file-name (concat user-emacs-directory "init-autoload.el")))

(if (not (version<= qingeditor/emacs-min-version emacs-version))
    (message (concat "Your version of Emacs (%s) is to old. "
                     "qingeditor requires Emacs version %s or above.")
	     emacs-version qingeditor/emacs-min-version)
  ;; 将`qingeditor'核心库加入到`load-path'列表中
  (qingeditor/register-target-dir-to-load-path qingeditor/libs-dir)
  (require 'qingeditor-init-bootstrap)
  ;;(qingeditor/initializer/init qingeditor/initializer-ref)
  ;; (qingeditor/core/boot/init)
  ;; (qingeditor/core/user-cfg/maybe-install-user-cfg-file)
  ;; (qingeditor/layer/layer/sync)
  ;; ;(qingeditor/ui/editor/display-info-box)
  ;; (qingeditor/core/boot/setup-startup-hook)
  ;; (require 'server)
  ;; (unless (server-running-p) (server-start))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (quelpa package-build spacemacs-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
