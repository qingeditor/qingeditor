;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define the feature listeners
;;
;; the load path listener
(defun qingeditor/modulemgr/loadpath-provider (event)
  "The load path listener handler."
  (let ((module (qingeditor/cls/get-module event))
        (module-dir (qingeditor/cls/get-module-dir module))
        load-paths)
    (when (object-of-class-p module qingeditor/modulemgr/feature/load-path-provider)
      (setq load-paths (qingeditor/cls/get-load-paths module))
      (dolist (path load-paths)
        (add-to-list 'load-path path t))
      ;; auto add module dir into load path.
      (add-to-list 'load-path module-dir t))))

(provide 'qingeditor-modulemgr-feature-listeners)
