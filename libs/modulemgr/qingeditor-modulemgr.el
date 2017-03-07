;;; qingeditor --- a distribution of Emacs editor
;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;; Code:

(require 'qingeditor-modulemgr-module)
(require 'qingeditor-modulemgr-package)

(defvar qingeditor/modulemgr/mdoule-detect-hook nil
  "When module manager init, it will detect all the modules under
direcotry `qingeditor/modulemgr/module-directory'. when this stage finished,
run this hook.")

(defvar qingeditor/modulemgr/before-load-modules-hook nil
  "Before module manager begin load modules, run this hook.")

(defvar qingeditor/modulemgr/load-modules-hook nil
  "When module manager load modules, run this hook.")

(defvar qingeditor/modulemgr/before-load-module-functions nil
  "Before module manager load target module, run this hook.
the function in this hook receive `module-name'
and `module-spec' arguments.")

(defvar qingeditor/modulemgr/load-module-functions nil
  "When module manager load target module, run this hook
the function in this hook receive `module-name'
and `module-spec' arguments.")

(defvar qingeditor/modulemgr/after-load-module-hook nil
  "After module manager finished load target module,
run this hook.")

(defvar qingeditor/modulemgr/before-install-packages-hook nil
  "Before install/update/uninstall packages, run this hook.")

(defvar qingeditor/modulemgr/before-install-package-hook nil
  "Before module manager install package, run this hook.")

(defvar qingeditor/modulemgr/install-package-hook nil
  "Before module manager install package, run this hook.")

(defvar qingeditor/modulemgr/after-install-package-hook nil
  "After module manager installed package, run this hook.")

(defvar qingeditor/modulemgr/before-configure-packages-hook nil
  "Before configure packages, run this hook.")

(defvar qingeditor/modulemgr/before-configure-packages-hook nil
  "Before configure package, run this hook.")

(defvar qingeditor/modulemgr/configure-packages-hook nil
  "When configure packages, run this hook.")

(defvar qingeditor/modulemgr/before-configure-packages-hook nil
  "Before configure packages, run this hook.")

(defvar qingeditor/modulemgr/after-configure-package-hook nil
  "After configure package, run this hook.")

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
           (expand-file-name (concat qingeditor/config/target-cfg-dir "modules/")))))
    (if (and qingeditor/config/target-cfg-dir
             (file-exists-p layer-dir))
        module-dir
      qingeditor/modulemgr/module-private-directory))
  "`qingeditor' default directory for private modules.")

(defvar qingeditor/modulemgr/module-repo (make-hash-table)
  "`qingeditor' supported modules repo. modules in repo is released
with `qingeditor'.")

(defvar qingeditor/modulemgr/used-modules nil
  "`qingeditor' used modules, the modules defined in `~/.qingeditor' file.")

(defvar qingeditor/modulemgr/package-repo (make-hash-table)
  "All package that required by `qingeditor' supported modules.")

(defvar qingeditor/modulemgr/used-packages nil
  "All packages that required by enabled modules.")

(defvar qingeditor/modulemgr/protected-packages nil
  "A list of packages that will be protected from remove as orphans.")

(defvar qingeditor/modulemgr/module-categories nil
  "List of symbols corresponding to category names. A category is
a direcotry with a name starting with `+'.")

(defvar qingeditor/modulemgr/inhibit-warnings nil
  "If non-nil then warning message emitted by the module system are ignored.")

(defvar qingeditor/modulemgr/detected-modules nil
  "Detected modules information from `qingeditor/modulemgr/module-directory'.")

(defvar qingeditor/modulemgr/modules-are-loaded nil
  "Is the modules already loaded, avoid multi load actions.")

(defvar qingeditor/modulemgr/target-modules nil
  "The target loaded modules specifics, commonly specified in
`~/.qingeditor' file.")

(defvar qingeditor/modulemgr/load-recursive-level 0
  "The guard variable during recursively load module.")

(defvar qingeditor/modulemgr/force-distribution nil
  "If set, bypass user set `qingeditor/config/distribution' value, then
`qingeditor' will use this property value for distribution type.")

(defvar qingeditor/modulemgr/recheck-dependency nil
  "If `non-nil' module manager will recheck dependency modules wether
or not loaded.")

(defvar qingeditor/modulemgr/error-count 0
  "Non nil indicates the number of errors occurred during the
    installation of initialization.")

(defun qingeditor/modulemgr/initialize ()
  "Initialize modulemgr internal data."
  (qingeditor/call-func qingeditor/module-configuration-setup
                        "Apply user configuration file module settings.")
  ;; Set the target modules to be load. Will first `delq' all item
  ;; that eq `editor-base' or `editor-standard' or `editor-bootstrap'
  ;; from `specs'
  (let ((distribution (if qingeditor/modulemgr/force-distribution
                          qingeditor/modulemgr/force-distribution
                        qingeditor/config/distribution))
        (specs qingeditor/config/configuration-modules))
    (setq specs (delq 'editor-base specs))
    (setq specs (delq 'editor-standard specs))
    (setq specs (delq 'editor-bootstrap specs))
    (push distribution specs)
    (setq qingeditor/modulemgr/target-modules specs))
  (qingeditor/modulemgr/detect-modules)
  (qingeditor/modulemgr/init-module-and-package-repo)
  (run-hooks 'qingeditor/modulemgr/mdoule-detect-hook))

(defun qingeditor/modulemgr/module-usedp (module-name)
  "Return `non-nil' if the module named `module-name' been
used."
  (memq module-name qingeditor/modulemgr/used-modules))

(defun qingeditor/modulemgr/package-usedp (package-name)
  "Return non-nil if NAME is the name of a used package."
  (let ((package (qingeditor/hash-table/get
                  qinegditor/modulemgr/package-repo
                  package-name)))
    (and package
         (qingeditor/cls/get-safe-owner package)
         (not (oref package :excluded)))))

(defun qingeditor/modulemgr/package-enable-p (package-name moudle-name)
  "Returns true if `package-name' should be configured for `module-name'.
`module-name' must not be the owner of `package-name'."
  (let* ((qingeditor/hash-table/get qingeditor/modulemgr/package-repo
                                    package-name)
         (owner (when package
                  (car (qingeditor/cls/get-owners package))))
         (disabled (when owner (oref owner :disabled-for)))
         (enabled (when owner (oref owner :enabled-for))))
    (when package
      (if (not (eq 'unspecified enable))
          (memq module-name enabled)
        (not (memq module-name disabled))))))

(defun qingeditor/modulemgr/register-lazy-load-modules ()
  "Reguster lazy load modules."
  (let* ((module-dir qingeditor/modulemgr/module-directory)
         (register-filename (concat module-dir "lazy-module-register.el")))
    (when (file-exists-p register-filename)
      (load register-filename))))

(defun qingeditor/modulemgr/process ()
  "Do actually module process."
  (qingeditor/modulemgr/load-modules)
  (qingeditor/modulemgr/register-lazy-load-modules)
  (qingeditor/modulemgr/install-packages)
  (qingeditor/modulemgr/configure-packages))

(defun qingeditor/modulemgr/load-modules ()
  "This method do loaded the target modules."
  (catch 'qingeditor-modulemgr-register-module-return
    (when qingeditor/modulemgr/modules-are-loaded
      (throw 'qingeditor-modulemgr-register-module-return this))
    ;; run before load modules hook
    (run-hooks 'qingeditor/modulemgr/before-load-modules-hook)
    (dolist (module-spec qingeditor/modulemgr/target-modules)
      (qingeditor/modulemgr/load-module module-spec))
    ;; run after load modules hook
    (run-hooks 'qingeditor/modulemgr/after-load-modules-hook)
    (setq qingeditor/modulemgr/modules-are-loaded t)))

(defun qingeditor/modulemgr/load-module (module-spec)
  "Do load the module specified by `module-spec'."
  (catch 'qingeditor-modulemgr-load-module-return
    (let* ((module-name (if (listp module-spec) (car module-spec) module-spec))
           (module (qingeditor/hash-table/get
                    qingeditor/modulemgr/module-repo module-name)))
      ;; we must first check wether `qingeditor' support the module named `module-name'
      (unless module
        (error "qingeditor doesn't support module: %S" module-name))
      (when (memq module-name qingeditor/modulemgr/used-modules)
        (throw 'qingeditor-modulemgr-load-module-return
               (qingeditor/hash-table/get
                qingeditor/modulemgr/module-repo module-name)))
      (run-hook-with-args
       'qingeditor/modulemgr/before-load-module-functions
       module-name module-spec)
      (setq qingeditor/modulemgr/load-recursive-level
            (1+ qingeditor/modulemgr/load-recursive-level))
      (run-hook-with-args
       'qingeditor/modulemgr/load-module-functions
       module-name module-spec)
      ;; we need check dependencies of current module
      (let ((require-modules-sym (intern (format "qingeditor/%S/require-modules" module-name))))
        (when (and (boundp require-modules-sym)
                   (listp (symbol-value require-modules-sym))
                   (> (length (symbol-value require-modules-sym)) 0))
          ;; we load module recursively
          ;; TODO we just leave this function, when finished first version, we
          ;; devel this function.
          (dolist (spec (symbol-value require-modules-sym))
            (qingeditor/modulemgr/load-module spec))))
      ;; setup self
      (qingeditor/modulemgr/setup-module module-name module-spec)
      (setq qingeditor/modulemgr/load-recursive-level
            (1- qingeditor/modulemgr/load-recursive-level))
      (run-hook-with-args
       'qingeditor/modulemgr/after-load-module-hook
       module-name)
      (add-to-list 'qingeditor/modulemgr/used-modules module-name)
      module)))

(defun qingeditor/modulemgr/setup-module (module-name module-spec)
  "This function handle the module resolve, it setup infomation for
`qingeditor/modulemgr/module' object using module `specific'. you must
ensure module object exist."
  (let ((module (qingeditor/hash-table/get
                 qingeditor/modulemgr/module-repo module-name))
        (module-dir (qingeditor/cls/get-module-dir module))
        (disabled (when (listp module-spec)
                    (qingeditor/mplist-get module-spec :disabled-for)))
        (enabled (when (listp module-spec)
                   (if (memq :enable-for module-spec)
                       (qingeditor/mplist-get module-spec :enable-for)
                     'unspecified)))
        (variables (when (listp module-spec)
                     (qingeditor/mplist-get module-spec :variables)))
        (has-extra-funcs-defs
         (intern-soft (format "qingeditor/%S/has-extra-funcs-defs" module-name)))
        (has-init-funcs (intern-soft (format "qingeditor/%S/has-extra-funcs-defs" module-name))))
    (when (and has-extra-funcs-defs
               (symbol-value has-extra-funcs-defs))
      (let* ((extra-funcs-filename (concat module-dir "funcs.el")))
        (when (file-exists-p extra-funcs-filename)
          (load extra-funcs-filename))))
    (let ((init-funcs-filename (concat module-dir "package-init-defs.el")))
      (when (file-exists-p init-funcs-filename)
        (load init-funcs-filename)))
    (oset module :disabled-for disabled)
    (oset module :enable-for enabled)
    (oset module :variables variables)
    (qingeditor/modulemgr/setup-module-packages module-name)))

(defun qingeditor/modulemgr/setup-module-packages (module-name)
  "This function setup the packages of module, using infomation
defined in `module.el' of target module."
  (let* ((module (qingeditor/hash-table/get
                  qingeditor/modulemgr/module-repo module-name)))
    (dolist (spec (qingeditor/modulemgr/get-module-require-packages module-name))
        (let* ((pkg-name (if (listp spec) (car spec) spec))
               (pkg-name-str (symbol-name pkg-name))
               (pkg (qingeditor/hash-table/get
                     qingeditor/modulemgr/package-repo pkg-name))
               (excluded (when (listp spec) (plist-get (cdr spec) :excluded)))
               (toggle (when (listp spec) (plist-get (cdr spec) :toggle)))
               (location (when (listp spec) (plist-get (cdr spec) :location)))
               (init-func (intern (format "qingeditor/%S/init-%S"
                                          module-name pkg-name)))
               (pre-init-func (intern (format "qingeditor/%S/pre-init-%S" module-name
                                              pkg-name)))
               (post-init-func (intern (format "qingeditor/%S/post-init-%S" module-name
                                               pkg-name)))
               (ownerp (or (and (eq 'config (qingeditor/cls/get-from-source pkg))
                                (null (qingeditor/cls/get-owners pkg)))
                           (fboundp init-func))))
          ;; TODO when set like this maybe not right
          (qingeditor/cls/set-property pkg :excluded excluded)
          ;; toggle is always an expr to be eval.
          (when toggle
            (qingeditor/cls/set-property pkg :toggle toggle))
          ;; setup package location
          (when location
            (let* ((pkg-from-source (qingeditor/cls/get-from-source pkg)))
              (if (and (listp location)
                       (eq (car location) 'recipe)
                       (eq (plist-get (cdr location) :fetcher) 'local))
                  (cond
                   ((eq 'modue pkg-from-source)
                    (let ((path (expand-file-name
                                 (format "%s%s/%s.el"
                                         (qingeditor/cls/get-local-dir module)
                                         pkg-name-str
                                         pkg-name-str))))
                      (qingeditor/cls/set-property pkg :location
                                                   `(recipe :fetcher file :path ,path))))
                   ((eq 'config pkg-from-source)
                    ;; TODO what is the local path for a package owned by the user 
                    nil))
                (qingeditor/cls/set-property pkg :location location))))
          (when ownerp
            ;; warn about multiple owners
            (when (and (qingeditor/cls/get-owners pkg)
                       (not (memq module-name (qingeditor/cls/get-owners pkg))))
              (qingeditor/modulemgr/warning
               (format (concat "More than one init function found for "
                               "package %S. previous owner was %S, "
                               "replacing it with module %S.")
                       pkg-name (qingeditor/cls/get-name (car (qingeditor/cls/get-owners pkg)))
                       (qingeditor/cls/get-name module))))
            ;; last owner wins over the previous one
            (object-add-to-list pkg :owners module))
          ;; check consistency between package and defined init functions
          (unless (or ownerp
                      (eq 'config (qingeditor/cls/get-from-source pkg))
                      (fboundp pre-init-func)
                      (fboundp post-init-func)
                      (oref pkg :excluded))
            (qingeditor/modulemgr/warning
             (format (concat "package %s not initialized in module %s, "
                             "you may consider removing this package from "
                             "the package list or use the :toggle keyword "
                             "instead of a `when' form.")
                     pkg-name (qingeditor/cls/get-name module))))
          ;; check if toggle can be applied
          (when (and (not ownerp)
                     (and (not (eq 'unspecified toggle))
                          toggle))
            (qingeditor/modulemgr/warning
             (format (concat "Ignoring :toggle for package %s because "
                             "module %S does not own it.")
                     pkg-name
                     (qingeditor/cls/get-name module))))
          (when (fboundp pre-init-func)
            (object-add-to-list pkg :pre-init-modules module-name))
          (when (fboundp post-init-func)
            (object-add-to-list pkg :post-init-modules module-name))
          ;; add to used package used package list
          (add-to-list 'qingeditor/modulemgr/used-packages pkg)))))

(defun qingeditor/modulemgr/install-packages ()
  "Install used packages."
  (let* ((display-buffer-alist
          '(("\\(\\*Compile-Log\\*\\)\\|\\(\\*Warnings\\*\\)"
             (display-buffer-in-side-window)
             (inhibit-same-window . t)
             (side . bottom)
             (window-height . 0.2))))
         packages)
    (dolist (package-name qingeditor/modulemgr/used-packages)
      )))

(defun qingeditor/modulemgr/configure-packages ()
  )

(defun qingeditor/modulemgr/detect-modules ()
  "Gather `qingeditor' modules."
  (let ((search-paths
         (append (list qingeditor/modulemgr/module-directory)
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
            (let ((type (qingeditor/modulemgr/get-directory-type sub)))
              (cond
               ((eq 'category type)
                (let ((category (qingeditor/modulemgr/get-category-from-path sub)))
                  (qingeditor/startup-buffer/message "-> Discovered category: %S"
                                                     category)
                  (add-to-list 'qingeditor/modulemgr/module-categories category)
                  (setq search-paths (cons sub search-paths))))
               ((eq 'module type)
                (add-to-list 'qingeditor/modulemgr/detected-modules
                             (cons (qingeditor/modulemgr/get-module-sym-from-path sub)  sub)))
               (t
                ;; module not found, add it to search path, recursively to search
                (setq search-paths (cons sub search-paths)))))))))))

(defun qingeditor/modulemgr/get-directory-type (path)
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

(defun qingeditor/modulemgr/get-category-from-path (dirpath)
  "Return a ctaegory symbol from the given `dirpath' The directory name must start
with `+'.Return `nil' if the direcotry is not a category."
  (when (file-directory-p dirpath)
    (let ((dirname (file-name-nondirectory
                    (directory-file-name dirpath))))
      (when (string-match "^+" dirname)
        (intern (substring dirname 1))))))

(defun qingeditor/modulemgr/get-module-sym-from-path (dirpath)
  "Get module symbol from `dirpath'."
  (intern (file-name-nondirectory (directory-file-name dirpath))))

(defun qingeditor/modulemgr/init-module-and-package-repo ()
  "Before we do module init, configure module, we must init all module object
that `qingeditor' support and all the packages that module require."
  (dolist (item qingeditor/modulemgr/detected-modules)
    (let* ((module-name (car item))
           (module-dir (file-name-as-directory (cdr item)))
           (module-filename (concat module-dir "module.el"))
           module
           package-sepcs)
     ;; first phase, we just load module define file and instance
     ;; module class
     (unless (file-exists-p module-filename)
       (error "The module.el file of %S is not exist." key))
     (load module-filename)
     (setq module (make-instance 'qingeditor/modulemgr/module))
     ;; invoke the `qingeditor/cls/init' method of module object.
     (qingeditor/cls/set-name module module-name)
     (qingeditor/cls/init module)
     (qingeditor/cls/set-module-dir module module-dir)
     (qingeditor/hash-table/set
      qingeditor/modulemgr/module-repo
      module-name module)
     ;; we setup basic infomation for package 
     (let ((module-require-packages
            (qingeditor/modulemgr/get-module-require-packages module-name)))
       (dolist (spec module-require-packages)
         ;; we just simple instance the package class
         ;; and set the name of package object.
         (let* ((package-sym (if (listp spec) (car spec) spec))
                (package-name (symbol-name package-sym))
                (package (qingeditor/hash-table/get
                          qingeditor/modulemgr/package-repo package-sym nil))
                (min-version (when (listp spec) (plist-get (cdr spec) :min-version)))
                (stage (when (listp spec) (plist-get (cdr spec) :stage)))
                (toggle (when (listp spec) (plist-get (cdr spec) :toggle)))
                (protected (when (listp spec) (plist-get (cdr spec) :protected)))
                package-installed)
           (unless package
             (setq package (qingeditor/modulemgr/package package-name :name package-sym))
             (qingeditor/hash-table/set qingeditor/modulemgr/package-repo
                                        package-sym package)
             ;; a bootstrap package is protected
             (qingeditor/cls/set-property package :protected
                                          (or protected
                                              (eq 'bootstrap stage)))
             (when protected
               (add-to-list 'qingeditor/modulemgr/protected-packages
                            package-sym)))
           (when toggle
             (qingeditor/cls/set-property package :toggle toggle))
           (when stage
               (qingeditor/cls/set-property package :stage stage))
           (if min-version
               (progn
                 (qingeditor/cls/set-property package :min-version (version-to-list min-version))
                 (setq package-installed (if (package-installed-p package-sym min-version) t nil)))
             (setq package-installed (if (package-installed-p package-sym)
                                           t nil)))
           (oset package :installed package-installed)))))))

(defun qingeditor/modulemgr/get-module-require-modules (module-name)
  "Get the require modules of module `module-name'."
  (let ((sym (intern (format "qingeditor/%S/require-modules"))))
    (when (boundp sym)
      (symbol-value sym))))

(defun qingeditor/modulemgr/get-module-require-packages (module-name)
  "Get require packages of module `module-name'."
  (let ((sym (intern (format "qingeditor/%S/require-packages" module-name))))
    (when (boundp sym)
      (symbol-value sym))))

(defun qingeditor/modulemgr/get-error-count ()
  "Get this error count during startup."
  qingeditor/modulemgr/error-count)

(defun qingeditor/modulemgr/warning (msg &rest args)
  "Display `msg' as a warning message in buffer `*Message*'.
If `qingeditor/modulemgr/inhibit-warnings' is non nil this method is no-op."
  (unless qingeditor/modulemgr/inhibit-warnings
    (apply 'qingeditor/startup-buffer/warning msg args)))

(provide 'qingeditor-modulemgr)
