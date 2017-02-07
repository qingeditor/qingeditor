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
(require 'qingeditor-modulemgr-event)
(require 'qingeditor-modulemgr-service-provider)
(require 'qingeditor-eventmgr-event-handler)

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
    :initarg :used-modules
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
    :documentation "All packages that required by enabled modules.")

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
    :initform (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "Detected modules information from `qingeditor/modulemgr/module-directory'.")

   (eventmgr
    :initarg :eventmgr
    :initform nil
    :type (satisfies (lambda (obj)
                       (or (null obj)
                           (object-of-class-p obj qingeditor/eventmgr/mgr))))
    :documentation "The eventmgr object for Module manager.")

   (event
    :initarg :event
    :initform nil
    :type (satisfies (lambda (obj)
                       (or (null obj)
                           (object-of-class-p obj qingeditor/eventmgr/event))))
    :documentation "The event object during loading modules.")

   (modules-are-loaded
    :initarg :modules-are-loaded
    :initform nil
    :type boolean
    :documentation "Is the modules already loaded, avoid multi load actions.")

   (target-modules
    :initarg :target-modules
    :intiform nil
    :type list
    :documentation "The target loaded modules specifics, commonly specified in `~/.qingeditor' file.")

   (load-finished
    :initarg :load-finished
    :initform 0
    :type number
    :documentation "The guard variable during recursively load module.")

   (force-distribution
    :initarg :force-distribution
    :initform nil
    :type (satisfies (lambda (x) (or (null x) (symbolp x))))
    :documentation "If set, bypass user set `qingeditor/config/distribution' value, then
`qingeditor' will use this property value for distribution type.")

   (recheck-dependency
    :initarg :recheck-dependency
    :initform nil
    :type boolean
    :documentation "If `non-nil' module manager will recheck dependency modules wether or not loaded.")
   )
  :documentation "The module manager class")

(defmethod qingeditor/cls/init ((this qingeditor/modulemgr/mgr))
  "Initialize modulemgr internal data."
  (qingeditor/call-func qingeditor/module-configuration-setup
                        "Apply user configuration file module settings.")
  (qingeditor/cls/set-target-modules qingeditor/modulemgr
                                     (copy-sequence qingeditor/config/configuration-modules))
  (qinegditor/cls/detect-modules this)
  (let ((event (qingeditor/cls/get-event this))
        (eventmgr (qingeditor/cls/get-eventmgr this)))
    (qingeditor/cls/set-name event qingeditor/modulemgr/module-detect-event)
    (qingeditor/cls/trigger-event eventmgr event)))

(defmethod qingeditor/cls/load-modules ((this qingeditor/modulemgr/mgr))
  "This method do loaded the target modules."
  (catch 'qingeditor-cls-register-module-return
    (when (oref this :modules-are-loaded)
      (throw 'qingeditor-cls-register-module-return this))
    (let ((event (qingeditor/cls/get-event this))
          (eventmgr (qingeditor/cls/get-eventmgr this)))

      ;; dispatch before load modules
      (qingeditor/cls/set-name event qingeditor/modulemgr/before-load-modules-event)
      (qingeditor/cls/trigger-event eventmgr event)

      ;; dispatch load modules
      (qingeditor/cls/set-name event qingeditor/modulemgr/load-modules-event)
      (qingeditor/cls/trigger-event eventmgr event)

      ;; dispatch after load modules
      (qingeditor/cls/set-name event qingeditor/modulemgr/after-load-modules-event)
      (qingeditor/cls/trigger-event eventmgr event)
      this)))

(defmethod qingeditor/cls/load-modules-handler ((this qingeditor/modulemgr/mgr) event)
  "The actually load modules method."
  (catch 'qingeditor-cls-load-modules-handler-return
    (when (oref this :modules-are-loaded)
      (throw 'qingeditor-cls-load-modules-handler-return t))
    (dolist (module-spec (oref this :target-modules))
      (qingeditor/cls/load-module this module-spec))
    (oset this :modules-are-loaded t)))

(defmethod qingeditor/cls/load-module ((this qingeditor/modulemgr/mgr) module-spec)
  "Do load the module specified by `module-spec.'"
  (catch 'qingeditor-cls-load-module-return
    (let ((module-name (if (listp module-spec) (car module-spec) module-spec))
          event
          module)
      (when (qingeditor/cls/has-key (oref this :used-modules) module-name)
        (throw 'qingeditor-cls-load-module-return
               (qingeditor/cls/get (oref this :used-modules) module-name)))
      (if (> (oref this :load-finished) 0)
          (setq event (clone (qingeditor/cls/get-event this)))
        (setq event (qingeditor/cls/get-event this)))
      (qingeditor/cls/set-module-name event (symbol-name module-name))
      (oset this :load-finished (1+ (oref this :load-finished)))
      (setq module (qingeditor/cls/get-module-by-spec this module-spec event))
      ))
  )

(defmethod qingeditor/cls/get-module-by-spec ((this qingeditor/modulemgr/mgr) module-spec event)
  "Get module from `module-spec'."
  (qingeditor/cls/set-name event qingeditor/modulemgr/load-module-resolve-event)
  (let* (result
        module
        (module-name (qingeditor/cls/get-module-name event))
        (module-sym (intern module-name)))
    (unless (qingeditor/cls/has-key (oref this :detected-modules) module-sym)
      (error "Module (%s) is not supported by qingdeditor." module-name))
    ;; dispatch module resove event
    (setq result (qingeditor/cls/trigger-event-until
           eventmgr
           (lambda (response)
             (and response
                  (object-of-class-p response qingeditor/modulemgr/module)))
           event))
    (setq module (qingeditor/cls/last result))
    (unless (and module
                 (object-of-class-p module qingeditor/modulemgr/module))
      (error "Module (%s) could not be initialized." module-name))))

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
                (qingeditor/cls/set (oref this :detected-modules)
                                    (qingeditor/cls/get-module-sym-from-path this sub) sub))
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
               (directory-file-name path)))
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
                    (directory-file-name dirpath))))
      (when (string-match "^+" dirname)
        (intern (substring dirname 1))))))

(defmethod qingeditor/cls/get-module-sym-from-path ((this qingeditor/modulemgr/mgr) dirpath)
  "Get module symbol from `dirpath'."
  (intern (file-name-nondirectory (directory-file-name dirpath))))

(defmethod qingeditor/cls/warning ((this qingeditor/modulemgr/mgr) msg &rest args)
  "Display `msg' as a warning message in buffer `*Message*'.
If `qingeditor/modulemgr/mgr:inhibit-warnings' is non nil this method is no-op."
  (unless (oref this :inhibit-warnings)
    (apply 'qingeditor/startup-buffer/warning msg args)))

(defmethod qingeditor/cls/set-eventmgr ((this qingeditor/modulemgr/mgr) eventmgr)
  "Set eventmgr for module manager."
  (qingeditor/cls/set-identifiers eventmgr '(qingeditor/modulemgr/mgr))
  (oset this :eventmgr eventmgr)
  (qingeditor/cls/attach-default-listeners this)
  this)

(defmethod qingeditor/cls/get-eventmgr ((this qingeditor/modulemgr/mgr))
  "Get the event manager object."
  (unless (and (oref this :eventmgr)
               (object-of-class-p (oref this :eventmgr) qingeditor/eventmgr/mgr))
    (qingeditor/cls/set-eventmgr this (qingeditor/eventmgr/mgr/init qingeditor/shared-eventmgr)))
  (oref this :eventmgr))

(defmethod qingeditor/cls/set-event ((this qingeditor/modulemgr/mgr) event)
  "Set the module event."
  (qingeditor/cls/set-target event this)
  (oset this :event event)
  this)

(defmethod qingeditor/cls/get-event ((this qingeditor/modulemgr/mgr))
  "Get the module event."
  (unless (and (oref this :event)
               (object-of-class-p (oref this :event) qingeditor/modulemgr/event))
    (qingeditor/cls/set-event this (make-instance 'qingeditor/modulemgr/event)))
  (oref this :event))

(defmethod qingeditor/cls/attach-default-listeners ((this qingeditor/modulemgr/mgr))
  "Attach the default module listeners for the event mgr of module manager."
  (let ((eventmgr (oref this :eventmgr)))
    (qingeditor/cls/attach
     eventmgr
     qingeditor/modulemgr/load-modules-event
     (qingeditor/eventmgr/event-handler/init (list #'qingeditor/cls/load-modules-handler this)))))

(defmethod qingeditor/cls/set-target-modules ((this qingeditor/modulemgr/mgr) specs)
  "Set the target modules to be load. Will first `delq' all item
that eq `editor-base' or `editor-standard' or `editor-bootstrap' from `specs'."
  (let ((distribution (if (oref this :force-distribution)
                          (oref this :force-distribution)
                        qingeditor/config/distribution)))
    (setq specs (delq 'editor-base specs))
    (setq specs (delq 'editor-standard specs))
    (setq specs (delq 'editor-bootstrap specs))
    (push distribution specs)
    (push 'editor-bootstrap specs)
    (oset this :target-modules specs)
    this))

(provide 'qingeditor-modulemgr-mgr)
