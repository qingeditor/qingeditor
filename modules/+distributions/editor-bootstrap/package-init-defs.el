;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; editor-base module packages init methods defs

(defmethod qingeditor/cls/init-async ((this qingeditor/module/editor-bootstrap)))

(defmethod qingeditor/cls/init-bind-map ((this qingeditor/module/editor-bootstrap))
  "init bind map."
  (require 'bind-map)
  (bind-map qingeditor/default-map
    :prefix-cmd qingeditor/cmds
    :keys (qingeditor/config/leader-key)
    :override-minor-modes t
    :override-mode-name qingeditor/leader-override-mode))

(defmethod qingeditor/cls/init-bind-key ((this qingeditor/module/editor-bootstrap)))

(defmethod qingeditor/cls/init-diminish ((this qingeditor/module/editor-bootstrap))
  "init diminish."
  (let ((modulemgr (oref this :modulemgr)))
    (when (not (qingeditor/cls/package-usedp modulemgr 'spaceline))
      (add-hook 'after-load-functions 'qingeditor/editor-bootstrap/diminish-hook))))

(defmethod qingeditor/cls/init-hydra ((this qingeditor/module/editor-bootstrap))
  "init hydra."
  (require 'hydra)
  (setq hydra-key-doc-function 'qingeditor/editor-bootstrap/hydra-key-doc-function)
  (setq hydra-head-format "[%s] "))

(defmethod qingeditor/cls/init-use-package ((this qingeditor/module/editor-bootstrap))
  "init use package."
  (require 'use-package)
  (setq use-package-verbose init-file-debug)
  ;; inject use-package hooks for easy customization of stock package
  ;; configuration
  (setq use-package-inject-hooks t))

(defmethod qingeditor/cls/init-which-key ((this qingeditor/module/editor-bootstrap))
  "init which key."
  (require 'which-key)
  (qingeditor/toggle/add-toggle
   which-key
   :mode which-key-mode
   :documentation "Display a buffer with available key bindings."
   ))

