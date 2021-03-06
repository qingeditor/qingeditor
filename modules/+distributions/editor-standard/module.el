;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; The editor-standard module class

(qingeditor/define-module
 editor-standard
 "The editor-standard config module"
 :require-modules
 (let* ((target-modules qingeditor/modulemgr/target-modules)
        modules
        completion-found)
   (setq modules '(editor-base
                   editor-completion
                   editor-layouts
                   editor-editing
                   editor-editing-visual
                   editor-language
                   editor-misc
                   editor-ui
                   editor-ui-visual
                   editor-org))
   (dolist (spec target-modules)
     (let ((m-name (if (listp spec) (car spec) spec)))
       (when (memq m-name '(helm ivy))
         (setq completion-found t))))
   (unless completion-found
     (add-to-list modules 'helm t))
   modules))
