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

(defvar qingeditor/modulemgr/installer/refresh-package-timeout qingeditor/config/elpa-timeout
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
    (setq package-archives (qingeditor/modulemgr/installer/resolve-package-archives
                            qingeditor/modulemgr/installer/elpa-archives))
    ;; optimization, no need to archive all the packages so early.
    (setq package-enable-at-startup nil)
    (package-initialize 'noactivate)))

(defun qingeditor/modulemgr/installer/resolve-package-archives
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
