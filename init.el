;; qing editor 初始化入口文件，主要将核心库加入到 `load-path' 里面来，然后将控制权交由库本身的初始化函数
;; 进行进一步初始化
;; 加载最基本的核心库

;; 没有这个注释 emacs25 会在这里自动添加 (package-initialize)
;; (package-initialize)

(setq gc-cons-threshold 100000000)
(load-file (expand-file-name (concat user-emacs-directory "init-autoload.el")))

(if (not (version<= qingeditor/emacs-min-version emacs-version))
    (message "您的Emacs版本过低, 请升级Emacs编辑器. 当前版本号:(%s), qws editor要求版本号大于等于: (%s)"
	     emacs-version qingeditor/emacs-min-version)
  ;; 将`qingeditor'核心库加入到`load-path'列表中
  (qingeditor/register-target-dir-to-load-path qingeditor/libs-dir)
  (require 'qingeditor-boot)
  (qingeditor/core/boot/init)
  (qingeditor/core/user-cfg/maybe-install-user-cfg-file)
  )

