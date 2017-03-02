;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; editor-completion init method defs

(defmethod qingeditor/cls/init-default-helm-config ((this qingeditor/module/editor-completion))
  (setq helm-prevent-escaping-from-minibuffer t)
  (setq helm-bookmark-show-location t)
  (setq helm-display-header-line nil)
  (setq helm-split-window-in-side-p t)
  (setq helm-always-two-windows t)
  (setq helm-echo-input-in-header-line t)
  (setq helm-imenu-execute-action-at-once-if-one nil)
  (setq helm-org-format-outline-path t)
  (setq helm-display-function 'qingeditor/editor-completion/display-helm-window))

(defmethod qingeditor/cls/init-default-ivy-config ((this qingeditor/module/editor-completion))
  )

(defmethod qingeditor/cls/init-ido ((this qingeditor/module/editor-completion))
  )

(defmethod qingeditor/cls/init-ido-vertical-mode ((this qingeditor/module/editor-completion))
  )