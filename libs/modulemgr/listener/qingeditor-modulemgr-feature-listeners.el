;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define the feature listeners

(defun qingeditor/modulemgr/dependency-indicator-handler (event)
  "The dependency indicator listeners. Here we get dependencies of
current module, check them wether already loaded."
  )

(defun qingeditor/modulemgr/extra-files-loader-handler (event)
  "The extra files loader."
  (let* ((module (qingeditor/cls/get-module event))
         (module-dir (qingeditor/cls/get-module-dir module))
         filenames)
    (when (object-of-class-p module qingeditor/modulemgr/feature/extra-files-loader)
      (setq filenames (qingeditor/cls/get-target-filenames module))
      (dolist (filename filenames)
        (setq filename (concat module-dir filename))
        (when (file-exists-p filename)
          (load filename))))))

(defun qingeditor/modulemgr/loadpath-provider-handler (event)
  "The load path listener handler."
  (let* ((module (qingeditor/cls/get-module event))
         (module-dir (qingeditor/cls/get-module-dir module))
         load-paths)
    (when (object-of-class-p module qingeditor/modulemgr/feature/load-path-provider)
      (setq load-paths (qingeditor/cls/get-load-paths module))
      (dolist (path load-paths)
        (add-to-list 'load-path path t))
      ;; auto add module dir into load path.
      (add-to-list 'load-path module-dir t))))

(defun qingeditor/modulemgr/service-provider-handler ()
  "The service provider."
  (let* ((module (qingeditor/cls/get-module event)))
    (when (object-of-class-p module qingeditor/modulemgr/feature/service-provider)
      (qingeditor/cls/register-service module))))

(provide 'qingeditor-modulemgr-feature-listeners)
