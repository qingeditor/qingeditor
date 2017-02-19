(defconst qingeditor/version "0.1.2" "qingeditor version number.")
(defconst qingeditor/emacs-min-version "24.4" "Minimal version of Emacs.")
(defconst qingeditor/start-time (current-time))
(defconst qingeditor/start-dir user-emacs-directory
  "qingeditor standard startup directory, default `~/.emacs.d/'.")

(defconst qingeditor/libs-dir
  (expand-file-name (concat qingeditor/start-dir "libs/"))
  "qingeditor core lib directory.")
(setq message-log-max 16384)

(defun qingeditor/register-target-dir-to-load-path (target-dir)
  "add target directory into `load-path' recursively."
  (let ((dir-items (directory-files target-dir t)))
    ;; add self
    (if (file-directory-p target-dir)
        (add-to-list 'load-path target-dir))
    (dolist (item dir-items)
      (if (and (not (string-equal "." (substring item -1)))
               (file-directory-p item))
          (qingeditor/register-target-dir-to-load-path item)))))
