;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(qingeditor/define-module
 editor-ui-visual
 "The editor-ui-visual config module"
 :has-extra-funcs-defs t
 :require-packages
 '((ansi-colors :location built-in)
   fancy-battery
   fill-column-indicator
   golden-ratio
   hl-todo
   neotree
   popwin
   popup
   (smooth-scrolling :location built-in)
   spaceline
   )
 )

