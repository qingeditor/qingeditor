;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; The module manager class

(require 'qingeditor-modulemgr-module)

;; Define some important const
(defconst qingeditor/modulemgr/module-directory
  (expand-file-name (concat qingeditor/start-dir "modules/"))
  "`qingeditor' distributions modules base directory.")

(defconst qingeditor/modulemgr/module-private-directory
  (expand-file-name (concat qingeditor/start-dir "private/"))
  "`qingeditor' private modules base directory.")

(defconst qingeditor/modulemgr/private-module-directory
  (let ((module-dir
         (when qingeditor/config/target-cfg-dir
           (expand-file-name (concat qingeditor/config/target-cfg-dir "layers/")))))
    (if (and qingeditor/config/target-cfg-dir
             (file-exists-p layer-dir))
        module-dir
      qingeditor/modulemgr/module-private-directory))
  "`qingeditor' default directory for private modules.")

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
on    :documentation "All packages that required by enabled modules.")

   (module-categories
    :initarg :module-categories
    :initform '()
    :type list
    :documentation "List of symbols corresponding to category names. A category is
a direcotry with a name starting with `+'.")

   (inhibit-warnings
    :initarg :inhibit-warnings
    :initform nil
    :type boolean
    :documentation "If non-nil then warning message emitted by the module system are ignored.")

   (detected-modules
    :initarg :detected-modules
    :initform nil
    :type list
    :documentation "Detected modules information from `qingeditor/modulemgr/module-directory'.")

   (eventmgr
    :initarg :eventmgr
    :initform nil
    :type (satisfies (lambda (obj) (or (null obj) (object-of-class-p obj qingeditor/eventmgr/mgr))))
    :documentation "The eventmgr object for Module manager.")
   )
  :documentation "The module manager class")

(defmethod qingeditor/cls/init ((this qingeditor/modulemgr/mgr))
  "Initialize modulemgr internal data."
  (qingeditor/call-func qingeditor/module-configuration-setup
                        "Apply user configuration file module settings.")
  (qinegditor/cls/detect-modules this))

(defmethod qingeditor/cls/load-modules ((this qingeditor/modulemgr/mgr))
  (prin1 "\n")
  (prin1 "load modules"))

(defmethod qinegditor/cls/detect-modules ((this qingeditor/modulemgr/mgr))
  "Gather `qingeditor' modules."
  (let ((search-paths (append (list qingeditor/modulemgr/module-directory)
                              qingeditor/config/cfg-module-dir
                              (list qingeditor/modulemgr/private-module-directory)
                              (when qingeditor/config/target-cfg-dir
                                (list qingeditor/config/target-cfg-dir)))))
    ;; depth-first search of subdirectories
    (while search-paths
      (let ((current-path (car search-paths)))
        (setq search-paths (cdr search-paths))
        (dolist (sub (directory-files current-path t nil 'nosort))
          ;; ignore ".", ".." and non-directories
          (unless (or (string-equal ".." (substring sub -2))
                      (string-equal "." (substring sub -1))
                      (not (file-directory-p sub)))
            (let ((type (qingeditor/cls/get-directory-type this sub)))
              (cond
               ((eq 'category type)
                (let ((category (qingeditor/cls/get-category-from-path this sub)))
                  (qingeditor/startup-buffer/message "-> Discovered category: %S"
                                                     category)
                  (object-add-to-list this :module-categories category)
                  (setq search-paths (cons sub search-paths))))
               ((eq 'module type)
                (object-add-to-list this :detected-modules sub))
               (t
                ;; module not found, add it to search path, recursively to search
                (setq search-paths (cons sub search-paths)))))))))))

(defmethod qingeditor/cls/get-module ((this qingeditor/modulemgr/mgr) module-name)
  "Return a module with name `module-name'
Return nil if module is not found."
  (when (qingeditor/cls/has-key (oref this :module-repo) module-name)
    (qingeditor/cls/get (oref this :module-repo))))

(defmethod qingeditor/cls/create-module ((this qingeditor/modulemgr/mgr)
                                         module-specs &optional module usedp dir)
  "Return a `qingeditor/modulemgr/mgr' object based on `module-specs'.
`dir' is the directory where the module is, if it is nil then search in the
indexed modules for the path."
  (let* ((module-name (if (listp module-specs) (car module-specs) module-specs))
         (module (if module module
                   (qingeditor/modulemgr/module (symbol-name module-name)
                                                :name module-name)))
         (dir (or dir (oref module :dir))))
    (message "create module %S" module-specs)))

(defmethod qingeditor/cls/register-module ((this qingeditor/modulemgr/mgr) module &optional usedp)
  "Add a `module' objecy to the system.
`usedp' non-nil means that `pkg' is a used module."
  )

(defmethod qingeditor/cls/get-directory-type ((this qingeditor/modulemgr/mgr) path)
  "Get the type of directory pointed by `path'
Possible return values:
module    - the direcotry is module
category  - the direcotry is category
nil       - the directory is a regular direcotry"
  (when (file-directory-p path)
    (if (string-match
         "^+" (file-name-nondirectory
               (directory-file-name
                (concat qingeditor/modulemgr/module-directory path))))
        'category
      (let ((files (directory-files path)))
        ;; moest frequent encounter in a module are tested first
        (when (member "module.el" files)
          'module)))))

(defmethod qingeditor/cls/get-category-from-path ((this qingeditor/modulemgr/mgr) dirpath)
  "Return a ctaegory symbol from the given `dirpath'
The directory name must start with `+'.
Return `nil' if the direcotry is not a category."
  (when (file-directory-p dirpath)
    (let ((dirname (file-name-nondirectory
                    (directory-file-name
                     (concat qingeditor/modulemgr/module-directory dirpath)))))
      (when (string-match "^+" dirname)
        (intern (substring dirname 1))))))

(defmethod qingeditor/cls/warning ((this qingeditor/modulemgr/mgr) msg &rest args)
  "Display `msg' as a warning message in buffer `*Message*'.
If `qingeditor/modulemgr/mgr:inhibit-warnings' is non nil this method is no-op."
  (unless (oref this :inhibit-warnings)
    (apply 'qingeditor/startup-buffer/warning msg args)))

(defmethod qingeditor/cls/set-eventmgr ((this qingeditor/modulemgr/mgr) eventmgr)
  "Set eventmgr for module manager."
  (qingeditor/cls/set-identifiers eventmgr '(qingeditor/modulemgr/mgr))
  (oset this :eventmgr eventmgr)
  (qingeditor/cls/attact-default-listeners this)
  this)

(defmethod qingeditor/cls/get-eventmgr ((this qingeditor/modulemgr/mgr))
  "Get the event manager object."
  (unless '(object-of-class-p (oref this :eventmgr qingeditor/eventmgr/mgr))
    (qingeditor/cls/set-eventmgr this (qingeditor/eventmgr/mgr/init qingeditor/shared-eventmgr)))
  (oref this :eventmgr))

(provide 'qingeditor-modulemgr-mgr)
