;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
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
  (unless (and (qingeditor/modulemgr/installer/package-archives-refreshed)
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
                "http://"
                (cdr archive))))))
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

(provide 'qingeditor-modulemgr-installer)
