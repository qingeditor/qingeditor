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

(defun qingeditor/editor-base/delete-file (filename &optional ask-user)
  "Remove specified file or directory.

Also kill assocated buffer (if any exists) and invalidates
projectile cache when it's possible.

When `ask-user' is non-nil, user will be asked to confirm file
removal."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename)
      (let ((modulemgr (qingeditor/modulemgr)))
        (when (and (qingeditor/cls/package-usedp modulemgr 'projectile)
                   (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))))))

;; from magnars
(defun qingeditor/editor-base/delete-current-buffer-file ()
  "Removes file connected to the current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (let ((modulemgr (qingeditor/modulemgr)))
          (when (and (qingeditor/cls/package-usedp modulemgr 'projectile)
                     (projectile-project-p))
            (call-interactively #'projectile-invalidate-cache))
          (message "File '%s' successfully removed" filename))))))

;; from magnars
(defun qingeditor/editor-base/sudo-edit (&optional arg)
  (interactive "p")
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (cond
      ((string-match-p "^/ssh:" fname)
       (with-temp-buffer
         (insert fname)
         (search-backward ":")
         (let ((last-match-end nil)
               (last-ssh-hostname nil))
           (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
             (setq last-ssh-hostname (or (match-string 1 fname)
                                         last-ssh-hostname))
             (setq last-match-end (match-end 0)))
           (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
         (t (concat "/sudo:root@localhost:" fname))))))))

(defun qingeditor/editor-base/delete-window (&optional arg)
  "Delete the current window.

If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

(defun qingeditor/editor-base/ace-delete-window (&optional arg)
  "Ace delete window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (require 'ace-window)
  (aw-select
   "Ace - Delete Window"
   (lambda (window)
     (when (equal '(4) arg)
       (with-selected-window window
         (qingeditor/editor-base/kill-this-buffer arg)))
     (aw-delete-window window))))

;; our own implementation of kill-this-buffer from menu-bar.el
(defun qingeditor/editor-base/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix arguments is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun qingeditor/editor-base/ace-kill-this-buffer (&optional arg)
  "Ace kill visible buffer in a window.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (require 'ace-window)
  (let (golden-ratio-mode)
    (aw-select
     "Ace - Kill buffer in window"
     (lambda (window)
       (with-selected-window window
         (qingeditor/editor-base/kill-this-buffer arg))))))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun qingeditor/editor-base/kill-other-buffers (&optioanl arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (defq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

;; from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun qingeditor/editor-base/toggle-current-window-dedication ()
  "Toggle dedication state of a window."
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun qingeditor/editor-base/show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((filename (or (buffer-file-name)
                      list-buffers-directory)))
    (if filename
        (message (kill-new filename))
      (error "Buffer not visiting a file."))))

;; adapted from bozhidar
;; http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun qingeditor/editor-base/find-user-init-filename ()
  "Edit the `user-init-file', in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun qingeditor/editor-base/find-user-config-file ()
  "Edit the user config file in the current window."
  (interactive)
  (find-file-existing (qingeditor/user-config-filename)))

(defun qingeditor/editor-base/ediff-user-config-and-template ()
  "ediff the current user config with the template."
  (interactive)
  (ediff (qingeditor/user-config-filename)
         (concat qingeditor/template-dir ".qingeditor.template")))

(defun qingeditor/editor-base/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

;; from https://gist.github.com/timcharper/493269
(defun qingeditor/editor-base/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun qingeditor/editor-base/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun qingeditor/editor-base/layout-tripe-columns ()
  "Set the layout to tripe columns."
  (interactive)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

(defun qingeditor/editor-base/layout-double-columns ()
  "Set layout to double colmuns."
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun qingeditor/editor-base/insert-line-above-no-indent (count)
  "Insert a new line above with no indentation."
  (interactive "p")
  (let ((p (+ (point) count)))
    (save-excursion
      (if (eq (line-number-at-pos) 1)
          (qingeditor/move-beginning-of-line)
        (progn
          (qingeditor/previous-line)
          (qingeditor/move-end-of-line)))
      (while (> count 0)
        (insert "\n")
        (setq count (1- count))))
    (goto-char p)))

(defun qingeditor/editor-base/insert-line-below-no-indent (count)
  "Insert a new line below with no indentation."
  (interactive "p")
  (save-excursion
    (qingeditor/move-end-of-line)
    (while (> count 0)
      (insert "\n")
      (setq count (1- count)))))

;; from https://github.com/gempesaw/dotemacs/blob/emacs/dg-defun.el
(defun qingeditor/editor-base/kill-matching-buffers-rudely (regexp &optional internal-too)
  "kill buffers whose name matches the specified `refexp'. This function, unlike the build in
`kill-matching-buffers' does so without asking. The optional second argument indicates whether to
kill internal buffer too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name
                 (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))

;; advise to prevent server from closing
(defvar qingeditor/editor-base/really-kill-emacs nil
  "prevent window manager close from closing instance.")

(defun qingeditor/editor-base/persistent-server-running-p ()
  "Requires `qingeditor/editor-base/really-kill-emacs' to be toggled and
`qingeditor/config/persistent-server' to be t."
  (and (fboundp 'server-running-p)
       (server-running-p)
       qingeditor/config/persistent-server))

(defadvice kill-emacs (around qingeditor/editor-base/really-exit activate)
  "Only kill emacs if prefix is set."
  (if (and (not qingeditor/editor-base/really-kill-emacs)
           (qingeditor/editor-base/persistent-server-running-p))
      (qingeditor/editor-base/frame-killer)
    ad-do-it))

(defadvice save-buffers-kill-emacs (around qingeditor/editor-base/really-exit activate)
  "only kill emacs if a prefix is set."
  (if (or qingeditor/editor-base/really-kill-emacs
          (not qingeditor/config/persistent-server))
      ad-do-it
    (qingeditor/editor-base/frame-killer)))

(defun qingeditor/editor-base/save-buffers-kill-emacs ()
  "Save all changed buffers and exit qingeditor."
  (interactive)
  (setq qingeditor/editor-base/really-kill-emacs t)
  (save-buffers-kill-emacs))

(defun qingeditor/editor-base/kill-emacs ()
  "Lose all changes and exit qingeditor."
  (interactive)
  (setq qingeditor/editor-base/really-kill-emacs t)
  (kill-emacs))

(defun qingeditor/editor-base/prompt-kill-emacs ()
  "Prompt to save changed buffer and exit qingeditor."
  (interactive)
  (setq qingeditor/editor-base/really-kill-emacs t)
  (save-some-buffers)
  (kill-emacs))

(defun qingeditor/editor-base/frame-killer ()
  "Kill server buffer and hide the main Emacs window."
  (interactive)
  (condition-case-unless-debug nil
      (delete-frame nil t)
    (error
     (make-frame-invisible nil t))))

(defun qingeditor/editor-base/toggle-frame-fullscreen ()
  "Respect the `qingeditor/config/fullscreen-use-non-native' variable when
toggling fullscreen."
  (interactive)
  (if qingeditor/config/fullscreen-use-non-native
      (qingeditor/editor-base/toggle-frame-fullscreen-non-native)
    (toggle-frame-fullscreen)))

;; have no effect
(defun qingeditor/editor-base/toggle-fullscreen ()
  "toggle full screen on X11 and Carbon."
  (interactive)
  (cond
   ((eq window-system 'x)
    (set-frame-parameter nil 'fullscreen
                         (when (not (frame-parameter nil 'fullscreen))
                           'fullboth)))
   ((eq window-system 'mac)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullscreen)))))

(defun qingeditor/editor-base/toggle-frame-fullscreen-non-native ()
  "Toggle full screen non-natively. Uses the `fullboth' frame paramerter
   rather than `fullscreen'. Useful to fullscreen on OSX w/o animations."
  (interactive)
  (modify-frame-parameters
   nil
   `((maximized
      . ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
           (frame-parameter nil 'fullscreen)))
     (fullscreen
      . ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
             (if (eq (frame-parameter nil 'maximized) 'maximized)
                 'maximized)
           'fullboth)))))
