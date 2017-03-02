;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; The helm module class

(defclass qingeditor/module/helm
  (qingeditor/modulemgr/module)
  ()
  :documentaion "The helm module class")

(defmethod qingeditor/cls/define-package-specs
  ((this qingeditor/module/helm))
  "Declare the require package specs of this module."
  '(ace-jump-helm-line
    auto-highlight-symbol
    (bookmark :has-init)
    helm
    helm-ag
    helm-descbinds
    helm-flx
    helm-make
    helm-mode-manager
    helm-projectile
    helm-swoop
    helm-themes
    (qingeditor/helm-help-mode :location local)
    (imenu :has-init nil)
    popwin
    (projectile :has-init nil)))

(defmethod qingeditor/cls/get-package-post-init-list
  ((this qingeditor/module/helm))
  '(bookmark imenu projectile))
