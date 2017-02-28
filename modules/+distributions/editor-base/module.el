;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; The editor-base module class

(defclass qingeditor/module/editor-base
  (qingeditor/modulemgr/module)
  ()
  :documentaion "The editor-base module class")

(defmethod qingeditor/cls/define-package-specs
  ((this qingeditor/module/editor-base))
  "Declare the require package specs of this module."
  '((abbrev :location built-in)
    ace-window
    (archive-mode :location built-in)
    (bookmark :location built-in)
    (conf-mode :location built-in)
    (dired :location built-in)
    (dired-x :location built-in)
    (electric-indent-mode :location built-in)
    (ediff :location built-in)
    (eldoc :location built-in)
    ;; some packages need to look for binaries,
    ;; which means the path must be ready by then
    (exec-path-from-shell :step pre)
    help-fns+
    (hi-lock :location built-in)
    (image-mode :location built-in)
    (imenu :location built-in)
    (linum :location built-in)
    (occur-mode :location built-in)
    (package-menu :location built-in)
    (qingeditor/page-break-lines :location built-in)
    pcre2el
    (process-menu :location built-in)
    projectile
    (recentf :location built-in)
    (savehist :location built-in)
    (saveplace :location built-in)
    spacemacs-theme
    (subword :location built-in)
    (tar-mode :location built-in)
    (uniquify :location built-in)
    (url :location built-in)
    (visual-line-mode :location built-in)
    (whitespace :location built-in)
    (winner :location built-in)))

(defmethod qingeditor/cls/provide-extra-func-definitions
  ((this qingeditor/module/editor-base))
  t)

(defmethod qingeditor/cls/provide-extra-module-config
  ((this qingeditor/module/editor-base))
  t)


