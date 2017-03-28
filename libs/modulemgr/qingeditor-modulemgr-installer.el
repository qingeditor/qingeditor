;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; Define some usefull packge installer functions
;;
;; qingeditor will use it to install pakcage

(defvar qingeditor/modulemgr/installer/package-archives-refreshed nil
  "Non nil if package archives have already been refreshed.")

(defvar qingeditor/modulemgr/installer/refresh-package-timeout 0
  "Timeout in seconds to reach a package archive page.")

(defvar qingeditor/modulemgr/installer/protected-packages nil
  "A list of packages that will be protected from remove as orphans.")

(defvar qingeditor/modulemgr/installer/rollback-directory
  (concat qingeditor/cache-dir ".rollback/")
  "`qingeditor' rollback directory.")

(defvar qingeditor/modulemgr/installer/rollback-info "rollback-info"
  "`qingeditor' rollback information file.")

(defvar qingeditor/modulemgr/installer/elpa-archives
  '(("melpa"     . "melpa.org/packages/")
    ("popkit"    . "elpa.popkit.org/packages/")
    ("org"       . "orgmode.org/elpa/")
    ("marmalade" . "marmalade-repo.org/packages/")
    ("gnu"       . "elpa.gnu.org/packages/"))
  "List of ELPA archives required by `qingeditor'.")

(defun qingeditor/modulemgr/installer/initialize ()
  "Initialize `package.el'."
  (setq qingeditor/modulemgr/installer/refresh-package-timeout qingeditor/config/elpa-timeout)
  (unless package--initialized
    (setq qingeditor/modulemgr/installer/rollback-directory
          (qingeditor/modulemgr/installer/get-elpa-directory qingeditor/modulemgr/installer/rollback-directory))
    (setq package-archives (qingeditor/modulemgr/installer/resolve-package-archive-addresses
                            qingeditor/modulemgr/installer/elpa-archives))
    ;; optimization, no need to archive all the packages so early.
    (setq package-enable-at-startup nil)
    (package-initialize 'noactivate)))

(defun qingeditor/modulemgr/installer/load-or-install-package (pkg-name &optional log file-to-load)
  "Load `pkg' package. `pkg' will be installed if it is not already installed.
Whenever the initial require fails the absolute path to the package
directory is returned.
if `log' is non-nil a message is displayed in `qinegditor-buffer-mode' buffer.
`file-to-load' is an explicit file to load after the installation."
  (let ((warning-minimum-level :error))
    (unless (require pkg-name nil 'noerror)
      ;; not installed, we try to initialize package.el only if required to
      ;; previous seconds during boot time.
      (require 'cl)
      (let ((pkg-elpa-dir (qingeditor/modulemgr/installer/get-package-installed-directory pkg-name)))
        (if pkg-elpa-dir
            (add-to-list 'load-path pkg-elpa-dir)
          ;; install the package
          (when log
            (qingeditor/startup-buffer/append
             (format "(bootstrap) installing %s...\n" pkg-name))
            (qingeditor/redisplay))
          (qingeditor/modulemgr/installer/refresh-package-archives 'quiet)
          (package-install pkg-name)
          (setq pkg-elpa-dir (qingeditor/modulemgr/installer/get-package-installed-directory pkg)))
        (require pkg-name nil 'noerror)
        (when file-to-load
          (load-file (concat pkg-elpa-dir file-to-load)))
        pkg-elpa-dir))))

(defun qingeditor/modulemgr/installer/load-or-install-protected-package
    (pkg-name &optional log file-to-load)
  "Load `pkg' package, and protect it against being deleted as an orphan.
See `qingeditor/modulemgr/installer/load-or-install-package'."
  (push pkg-name qingeditor/modulemgr/installer/protected-packages)
  (qingeditor/modulemgr/installer/load-or-install-package pkg-name log file-to-load))

(defun qingeditor/modulemgr/installer/refresh-package-archives (&optional quiet force)
  "Refresh all archivees declared in current `package-archives'.

This function first performs a simple GET request with a timeout in order to
fix very long refresg time when an archive is not reachable.

Note that this simple GEt is a huristic to determine the availability
likelihood of an archive, so it can give false positive if the archive
page is served but the archive is not.

if `quiet' is non `nil' then the function does not print message in the `qingeditor'
startup buffer.

If `force' is non `nil' then refresh the archives event if they have been already
refreshed during the current session."
  (unless (and qingeditor/modulemgr/installer/package-archives-refreshed
               (not force))
    (setq qingeditor/modulemgr/installer/package-archives-refreshed t)
    (let ((count (length package-archives))
          (i 1))
      (dolist (archive package-archives)
        (unless quiet
          (qingeditor/startup-buffer/replace-last-line
           (format "--> refreshing package archive: %s... [%s/%s]"
                   (car archive) i count)))
        (qingeditor/redisplay)
        (setq i (1+ i))
        (unless
            (eq 'error
                ;; why not use `qingeditor/modulemgr/installer/refresh-package-timeout'
                (with-timeout (qingeditor/config/elpa-timeout
                               (progn
                                 (display-warning
                                  'qingeditor
                                  (format
                                   "\nError connection time out for %s repository!"
                                   (car archive))
                                  :warning)
                                 'error))
                  (condition-case err
                      (url-retrieve-synchronously (cdr archive))
                    ('error
                     (display-warning
                      'qingeditor
                      (format "\nError while concating %s repository!"
                                           (car archive)) :warning)
                     'error))))
          (let ((package-archives (list archive)))
            (package-refresh-contents))))
      (package-read-all-archive-contents)
      (unless quiet (qingeditor/startup-buffer/append "\n")))))

(defun qingeditor/modulemgr/installer/resolve-package-archive-addresses
    (archives)
  "Resolve HTTP handlers for each archive in `archives' and return a list
of all reachable ones.
If the address of an archive already contains the protocol then this address is
left untouched. The return list has a `package-archives' compliant format."
  (mapcar
   (lambda (archive)
     (cons (car archive)
           (if (or (string-match-p "http" (cdr archive))
                   (string-prefix-p "/" (cdr archive)))
               (cdr archive)
             (concat
              (if (and qingeditor/config/elpa-https
                       (not qingeditor/insecure)
                       ;; for now org ELPA repository does
                       ;; not support HTTPS
                       ;; TODO when org ELPA repo support
                       ;; HTTPS remove the check
                       ;; `(not (equal "org" (car archive)))'
                       (not (equal "org" (car archive))))
                  "https://"
                "http://")
              (cdr archive)))))
   archives))

(defun qingeditor/modulemgr/installer/get-elpa-directory (root-dir)
  "Evaluate the correct package subdirectory of `root-dir'. This is
done according to the value if `qingeditor/config/elpa-subdirectory'.
if it is `nil', then `root-dir' is returned. Otherwise a subdirectory of `root-dir'
is returned."
  (if (not qingeditor/config/elpa-subdirectory)
      root-dir
    (let ((subdir (if (eq 'emacs-version qingeditor/config/elpa-subdirectory)
                      (format "%d%s%d"
                              emacs-major-version
                              version-separator
                              emacs-minor-version)
                    (eval qingeditor/config/elpa-subdirectory))))
      (file-name-as-directory (expand-file-name subdir root-dir)))))

(defun qingeditor/modulemgr/installer/get-package-installed-directory (pkg-name)
  "Get the directory of `pkg-name', return nil if not found."
  (let ((elpa-dir (file-name-as-directory package-user-dir)))
    (when (file-exists-p elpa-dir)
      (let* ((pkg-match (concat "\\`" (symbol-name pkg-name) "-[0-9]+"))
             (dir (car (directory-files elpa-dir 'full pkg-match))))
        (when dir (file-name-as-directory dir))))))

(defun qingeditor/modulemgr/installer/get-packages-with-deps
  (pkg-names filter &optional use-archive)
  "Return a filtered `pkg-names' list where each elements satisfies `filter'."
  (when pkg-names
    (let (result)
      (dolist (pkg-name pkg-names)
        ;; recursively check dependencies
        (let* ((deps
                (if use-archive
                    (qingeditor/modulemgr/installer/get-package-deps-from-archive pkg-name)
                  (qingeditor/modulemgr/installer/get-package-deps-from-alist pkg-name)))
               (install-deps
                (when deps (qingeditor/modulemgr/installer/get-packages-with-deps
                            (mapcar 'car deps) filter))))
          (when install-deps
            (setq result (append install-deps result))))
        (when (funcall filter pkg-name)
          (add-to-list 'result pkg-name t)))
      (delete-dups result))))

(defun qingeditor/modulemgr/installer/get-package-deps-from-alist (pkg-name)
  "Return the dependencies alist for package with name `pkg-name'."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (when pkg-desc
      (package-desc-reqs (cadr pkg-desc)))))

(defun qingeditor/modulemgr/installer/get-package-deps-from-archive (pkg-name)
  "Return the dependencies alist for a PKG-NAME from the archive data."
  (let* ((pkg-arch (assq pkg-name package-archive-contents))
         (reqs (when pkg-arch (package-desc-reqs (cadr pkg-arch)))))
    ;; recursively get the requirements of reqs
    (dolist (req reqs)
      (let* ((pkg-name2 (car req))
             (reqs2 (qingeditor/modulemgr/installer/get-package-deps-from-archive
                     pkg-name2)))
        (when reqs2 (setq reqs (append reqs2 reqs)))))
    reqs))

(defun qingeditor/modulemgr/installer/install-package (package)
  "Unconditionally install the package `package'."
  (let* ((package-name (when package (qingeditor/cls/get-name package)))
         (location (when package (qingeditor/cls/get-location package)))
         (min-version (when package (qingeditor/cls/get-min-version package))))
    (unless (package-installed-p package-name min-version)
      (cond
       ((or (null package) (eq 'elpa location))
        (qingeditor/modulemgr/installer/install-from-elpa package-name)
        (when package (qingeditor/cls/set-property package :lazy-install nil)))
       ((and (listp location) (eq 'recipe (car location)))
        (qingeditor/modulemgr/installer/install-from-recipe package)
        (qingeditor/cls/set-property package :lazy-install nil))
       (t (error "Cannot install package %S." package-name))))))

(defun qingeditor/modulemgr/installer/install-quelpa ()
  "Install `quelpa'."
  (setq quelpa-verbose init-file-debug
        quelpa-dir (concat qingeditor/cache-dir "quelpa/")
        quelpa-build-dir (expand-file-name "build" quelpa-dir)
        quelpa-persistent-cache-file (expand-file-name "cache" quelpa-dir)
        quelpa-update-melpa-p nil)
  (qingeditor/modulemgr/installer/load-or-install-protected-package 'package-build)
  (qingeditor/modulemgr/installer/load-or-install-protected-package 'quelpa))

(defun qingeditor/modulemgr/installer/install-from-recipe (package)
  "Install `pkg' from a recipe."
  (let* ((pkg-name (qingeditor/cls/get-name package))
         (recipe (cons pkg-name (cdr (qingeditor/cls/get-location package)))))
    (if recipe
        (quelpa recipe)
      (qingeditor/modulemgr/warning
       (concat "Cannot find any recipe for package %S! Be sure "
               "to add a recipe for it in alist %S.")
       pkg-name recipes-var))))

(defun qingeditor/modulemgr/installer/install-from-elpa (package-name)
  "Install `package' from ELPA."
  (if (not (assq package-name package-archive-contents))
      (qingeditor/startup-buffer/append
       (format (concat "\nPackage %s is unavailable. "
                       "Is the package name misspelled?\n")
               package-name))
    (let ((pkg-desc (assq package-name package-archive-contents)))
      (dolist
          (dep (qingeditor/modulemgr/installer/get-package-deps-from-archive
                package-name))
        (if (package-installed-p (car dep) (cadr dep))
            (qingeditor/modulemgr/installer/activate-package (car dep))
          (qingeditor/modulemgr/installer/install-from-elpa (car dep))))
      (if pkg-desc
          (package-install (cadr pkg-desc))
        (package-install package-name)))))

(defun qingeditor/modulemgr/installer/activate-package (pkg)
  "Activate `pkg'."
  (unless (memq pkg package-activated-list)
    (package-activate pkg)))

(provide 'qingeditor-modulemgr-installer)
