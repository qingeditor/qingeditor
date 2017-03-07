;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(qingeditor/define-module
 editor-completion
 "The editor-completion config module"
 :has-extra-funcs-defs t
 :has-extra-config t
 :require-packages
 '((default-helm-config :location built-in)
   (default-ivy-config :location built-in)
   (ido :location built-in)
   ido-vertical-mode)
 )
