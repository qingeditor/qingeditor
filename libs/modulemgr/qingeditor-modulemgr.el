;;; qingeditor --- a distribution of Emacs editor
;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;; Code:

(defvar qingeditor/modulemgr/mdoule-detect-hook nil
  "When module manager init, it will detect all the modules under
direcotry `qingeditor/modulemgr/module-directory'. when this stage finished,
run this hook.")

(defvar qingeditor/modulemgr/before-load-modules-hook nil
  "Before module manager begin load modules, run this hook.")

(defvar qingeditor/modulemgr/load-modules-hook nil
  "When module manager load modules, run this hook.")

(defvar qingeditor/modulemgr/load-module-resolve-hook nil
  "Before start load module cycle begin, we must get a module object,
run this hook.")

(defvar qingeditor/modulemgr/before-load-module-hook nil
  "Before module manager load target module,
run this hook.")

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

(defvar qingeditor/modulemgr/load-finished 0
  "The guard variable during recursively load module.")

(defvar qingeditor/modulemgr/force-distribution nil
  "If set, bypass user set `qingeditor/config/distribution' value, then
`qingeditor' will use this property value for distribution type.")

(defvar qingeditor/modulemgr/recheck-dependency nil
  "If `non-nil' module manager will recheck dependency modules wether
or not loaded.")

(defvar qingeditor/modulemgr/error-count nil
  "Non nil indicates the number of errors occurred during the
    installation of initialization.")

(defun qingeditor/modulemgr/module-registered (module-name)
  nil)

(defun qingeditor/modulemgr/register-module (module-name)
  t)

(defun qingeditor/modulemgr/initialize ()
  "Initialize modulemgr internal data."
  (qingeditor/call-func qingeditor/module-configuration-setup
                        "Apply user configuration file module settings.")
  (setq qingeditor/modulemgr/target-modules qingeditor/config/configuration-modules)
  (qingeditor/modulemgr/detect-modules)
  (qingeditor/modulemgr/init-module-and-package-repo)
  (run-hooks 'qingeditor/modulemgr/mdoule-detect-hook))

(defun qingeditor/modulemgr/process ()
  )

(defun qingeditor/modulemgr/detect-modules ()
  )

(defun qingeditor/modulemgr/init-module-and-package-repo ()
  )

(provide 'qingeditor-modulemgr)
