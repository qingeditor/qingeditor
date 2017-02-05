;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The module manager class

;; Define some important const
(defconst qingeditor/modulemgr/module-directory
  (expand-file-name (concat qingeditor/start-dir "modules/"))
  "`qingeditor' distributions modules base directory.")

(defconst qingeditor/modulemgr/module-private-directory
  (expand-file-name (concat qingeditor/start-dir "private/"))
  "`qingeditor' private modules base directory.")

(defclass qingeditor/modulemgr/mgr ()
  ((module-repo
    :initarg :module-repo
    :initform (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "`qingeditor' supported modules repo.
modules in repo is released with `qingeditor'.")

   (used-modules
    :initarg :enabled-modules
    :initform (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "`qingeditor' used modules, the modules defined in
`~/.qingeditor' file.")

   (pakage-repo
    :initarg :pakage-repo
    :initform (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "All package that required by `qingeditor' supported modules.")

   (used-packages
    :initarg :used-packages
    :initform (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "All packages that required by enabled modules."))
  :documentation "The module manager class")

(defmethod qingeditor/cls/init ((this qingeditor/modulemgr/mgr))
  "Initialize modulemgr internal data."
  (qingeditor/call-func qingeditor/module-configuration-setup
                        "Apply user configuration file module settings.")
  (qingeditor/cls/detect-modules-and-packages this))

(defmethod qingeditor/cls/load-modules ((this qingeditor/modulemgr/mgr))
  (prin1 "\n")
  (prin1 "load modules"))

(defmethod qingeditor/cls/detect-modules-and-packages
  ((this qingeditor/modulemgr/mgr))
  "Detect `qingeditor' modules and packages information."
  (let ((search-paths (append ())))))

(provide 'qingeditor-modulemgr-mgr)
