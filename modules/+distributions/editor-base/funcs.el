;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; editor base mode extra funcs defs
(require 'cl-lib)

(defun qingeditor/editor-base/run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (format "%S-local-vars-hook" major-mode))))

(defun qingeditor/editor-base/split-and-new-line ()
  "Split a quoted strings or s-expression and insert a new line with
auto-indent."
  (interactive)
  (sp-split-sexp 1)
  (sp-newline))

(defun qingeditor/editor-base/push-mark-and-goto-begnning-of-line ()
  "Push a mark at current location and go to the beginning of the line."
  (interactive)
  (push-mark (point))
  (qingeditor/beginning-of-line))

(defun qingeditor/editor-base/push-mark-and-goto-end-of-line ()
  "Push a mark at current location and go to the end of the line."
  (interactive)
  (push-mark (point))
  (qingeditor/end-of-line))

;; from Prelude
;; TODO: dispatch these in the modules
(defvar qingeditor/editor-base/indent-sensitive-modes
  '(coffee-mode
    elm-mode
    haml-mode
    haskell-mode
    slim-mode
    makefile-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    python-mode
    yaml-mode)
  "Modes for which auto-indenting is suppressed.")

(defcustom qingeditor/editor-base/yank-indent-threshold 1000
  "Threshold (# char) over which indentation does not automatically occur.")

(defcustom qingeditor/editor-base/large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                 doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                 pdf-view-mode)
  "Major modes which `qingeditor/editor-base/check-large-file' will not be
automatically applied to.")

;; check when opening large files - literal file open
(defun qingeditor/editor-base/check-large-file ()
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and
           (not (memq major-mode qingeditor/editor-base/large-file-modes-list))
           size (> size (* 1024 1024 qingeditor/config/large-file-size))
           (y-or-n-p (format (concat "%s is a large file, open literally to "
                                     "avoid performance issues?")
                             filename)))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(defun qingeditor/editor-base/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode qingeditor/editor-base-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning)
                           (region-end))
            (message "Indented selected region."))
        (progn
          (qingeditor/indent (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))

;; from https://gist.github.com/3402786
(defun qingeditor/editor-base/toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

;; https://tsdh.wordpress.com/2007/03/28/deleting-windows-vertically-or-horizontally/
(defun qingeditor/editor-base/maximize-horizontally ()
  "Delete all windows left or right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun qingeditor/editor-base/useful-buffer-p (buffer)
  "Determines if  a buffer is useful."
  (let ((buf-name (buffer-name buffer)))
    (or (with-current-buffer buffer
          (derived-mode-p 'comint-mode))
        (cl-loop for useful-regexp in qingeditor/editor-base/useful-buffers-regexp
                 thereis (string-match-p useful-regexp buf-name))
        (cl-loop for useless-regexp in qingeditor/editor-base/useless-buffers-regexp
                 never (string-match-p useless-regexp buf-name)))))

(defun qingeditor/editor-base/useless-buffer-p (buffer)
  "Determines if a buffer is useless."
  (not (qingeditor/useful-buffer-p buffer)))

;; from magnars modified by ffevotte for dedicated windows support
(defun qingeditor/editor-base/rotate-windows (count)
  "Rotate each window forwards.
A negative prefix arguments rotates each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (let* ((non-dedicated-windows (remove-if 'window-dedicated-p (window-list)))
         (num-windows (length non-dedicated-windows))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))
                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))
                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))
                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))

(defun qingeditor/editor-base/rotate-windows-backward (count)
  "Rotate each window backwards.
Decicded (locked) windows are left untouched."
  (interactive "p")
  (qingeditor/editor-base/rotate-windows (* -1 count)))

(defun qingeditor/editor-base/rename-file (filename &optional new-filename)
  "Rename `filename' to `new-filename'.

When `new-filename' is not specified, asked user for a new name.

Also renames assocated buffer (if any exists), invalidates
projectile cache when it's possible and update recentf list."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let* ((buffer (find-buffer-visiting filename))
           (short-name (file-name-nondirectory filename))
           (new-name (if new-filename new-filename
                       (read-file-name
                        (format "Rename %s to: " short-name))))
           (modulemgr (qingeditor/modulemgr)))
      (cond ((get-buffer new-name)
             (error "A buffer named '%s' already exists!" new-name))
            (t
             (let ((dir (file-name-directory new-name)))
               (when (and (not (file-exists-p dir))
                          (yes-or-no-p (format "Create directory '%s' ?" dir)))
                 (make-directory dir t)))
             (rename-file filename new-name 1)
             (when buffer
               (kill-buffer buffer)
               (find-file new-name))
             (when (fboundp 'recentf-add-file)
               (recentf-add-file new-filename)
               (recentf-remove-if-non-kept filename))
             (when (and (qingeditor/cls/package-usedp modulemgr 'projectile)
                        (projectile-project-p))
               (call-interactively #'projectile-invalidate-cache))
             (message "File '%s' sucessfully renamed to '%s'" short-name
                      (file-name-nondirectory new-name)))))))

;; from magnars
(defun qingeditor/editor-base/rename-current-buffer-name ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir (file-name-directory filename))
         (modulemgr (qingeditor/modulemgr)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " dir)))
        (cond
         ((get-buffer-name new-name)
          (error "A buffer named '%s' already exists!" new-name))
         (t
          (let ((dir (file-name-directory new-name)))
            (when (and (not (file-exists-p dir))
                       (yes-or-no-p (format "Create directory '%s'?" dir)))
              (make-directory dir t)))
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (when (fboundp 'recentf-add-file)
            (recentf-add-file new-name)
            (recentf-remove-if-non-kept filename))
          (when (and (qingeditor/cls/package-usedp modulemgr 'projectile)
                     (projectile-project-p))
            (call-interactively #'projectile-invalidate-cache))
          (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))
