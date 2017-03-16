;;; qingeditor --- a distribution of Emacs editor
;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; The editor-base module class
;;; Code:

(qingeditor/define-module
 helm
 "The helm config module"
 :has-extra-funcs-defs t
 :has-extra-config t
 :require-packages
 '(ace-jump-helm-line
   auto-highlight-symbol
   bookmark
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
   imenu
   popwin
   projectile)
 )
