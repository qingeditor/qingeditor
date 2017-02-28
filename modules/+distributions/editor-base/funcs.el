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

;; taken from Prelude: https://github.com/bbatsov/prelude
(defmacro qingeditor/editor/advise-commands (advice-name commands class &rest body)
  "Apply advice named `advice-name' to multiple `commands'.
The body of the advice is in `body'."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                      (,class ,(intern (format "%S-%s" command advice-name))
                              activate)
                    ,@body))
               commands)))

(defun qingeditor/editor-base/safe-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))


(defun qingeditor/editor-base/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

(defun qingeditor/editor-base/find-ert-test-buffer (ert-test)
  "Return the buffer where `ert-test' is defined."
  (car (find-definition-noselect (ert-test-name ert-test) 'ert-deftest)))

(defun qingeditor/editor-base/ert-run-tests-buffer ()
  "Run all the tests in the current buffer."
  (interactive)
  (save-buffer)
  (load-file (buffer-file-name))
  (let ((cbuf (current-buffer)))
    (ert '(satisfies
           (lambda (test)
             (eq cbuf (qingeditor/editor-base/find-ert-test-buffer test)))))))

(defun qingeditor/editor-base/open-in-external-app (file-path)
  "Open `file-path' in external application."
  (cond
   ((qingeditor/system-is-mswindows)
    (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
   ((qingeditor/system-is-mac) (shell-command (format "open \"open \"" file-path)))
   ((qingeditor/system-is-liunx)
    (let ((process-connection-type nil))
      (start-process "" nil "xdg-open" file-path)))))

(defun qingeditor/editor-base/open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used open the folder
containing the current file by the default explorer."
  (interactive "P")
  (if arg
      (qingeditor/editor-base/open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (qingeditor/editor-base/open-in-external-app file-path)
        (message "No file associated to this buffer.")))))

(defun qingeditor/editor-base/switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;; http://stackoverflow.com/a/10216338/4869
(defun qingeditor/editor-base/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun qingeditor/editor-base/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;; align functions
;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun qingeditor/align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If `justify-right' is non nil justify to the right instead of the
left. If `after' is non nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))
    (message "%S" complete-regexp)
    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun qingeditor/editor-base/align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro qingeditor/editor-base/create-align-repeat-x
    (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "qingeditor/editor-base/aligin-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (qingeditor/editor-base/align-repeat start end ,regexp ,justify-right after)))))

(qingeditor/editor-base/create-align-repeat-x "comma" "," nil t)
(qingeditor/editor-base/create-align-repeat-x "semicolon" ";" nil t)
(qingeditor/editor-base/create-align-repeat-x "colon" ":" nil t)
(qingeditor/editor-base/create-align-repeat-x "equal" "=")
(qingeditor/editor-base/create-align-repeat-x "math-oper" "[+\\-*/]")
(qingeditor/editor-base/create-align-repeat-x "ampersand" "&")
(qingeditor/editor-base/create-align-repeat-x "bar" "|")
(qingeditor/editor-base/create-align-repeat-x "left-paren" "(")
(qingeditor/editor-base/create-align-repeat-x "right-paren" ")" t)
(qingeditor/editor-base/create-align-repeat-x "backslash" "\\\\")

(defun qingeditor/editor-base/dos2unix ()
  "Convert the current buffer to unix file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun qingeditor/editor-base/unix2dos ()
  "Convert the current buffer to dos file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun qingeditor/editor-base/copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

(defun qingeditor/editor-base/uniquify-lines ()
  "Remove duplicate adjacent lines in region or current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((beg (if (region-active-p) (region-beginning) (point-min)))
            (end (if (region-active-p) (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))))

(defun qingeditor/editor-base/sort-lines ()
  "Sort lines in region or current buffer."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (sort-lines nil beg end)))

;; linum mouse helpers
(defvar qingeditor/editor-base/linum-mdown-line nil
  "Define persistent variable for linum selection.")

(defun qingeditor/editor-base/line-at-click ()
  "Determine the visual line at click"
  (save-excursion
    (let ((click-y (cddr (mouse-position)))
          (debug-on-error t)
          (line-move-visual t))
      (goto-char (window-start))
      (next-line (1- click-y))
      (1+ (line-number-at-pos)))))

(defun qingeditor/editor-base/md-select-linum (event)
  "Select code block between point and `qingeditor/editor-base/linum-mdown-line'."
  (interactive "e")
  (mouse-select-window event)
  (goto-line (qingeditor/editor-base/line-at-click))
  (set-mark (point))
  (setq qingeditor/editor-base/linum-mdown-line
        (line-number-at-pos)))

(defun qingeditor/editor-base/mu-select-linum ()
  "Select code block between point and `qingeditor/editor-base/linum-mdown-line'."
  (interactive)
  (when qingeditor/editor-base/linum-mdown-line
    (let (mu-line)
      (setq mu-line (qingeditor/editor-base/line-at-click))
      (goto-line (max qingeditor/editor-base/linum-mdown-line mu-line))
      (set-mark (line-end-position))
      (goto-line (min qingeditor/editor-base/linum-mdown-line mu-line))
      (setq qingeditor/editor-base/linum-mdown-line nil))))

(defun qingeditor/editor-base/select-current-block ()
  "Select the current block of the between blank lines."
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil t)
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil t)
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point)))
        (setq p2 (point))))))

;; From http://xugx2007.blogspot.ca/2007/06/benjamin-rutts-emacs-c-development-tips.html
(setq compilation-finish-function
      (lambda (buf str)
        (if (or (string-match "exited abnormally" str)
                (string-match "FAILED" (buffer-string)))
            ;; there wer errors
            (message "There were errors. SPC-e-n to visit.")
          (unless (or (string-match "Grep finished" (buffer-string))
                      (string-match "Ag finished" (buffer-string))
                      (string-match "nosetests" (buffer-name)))
            ;; no errors
            (message "compilation ok.")))))

;; from http://www.emacswiki.org/emacs/WordCount
(defun qingeditor/editor-base/count-words-analysis (start end)
  "Count how many times each word is used in the region. Punctuation
is ignored."
  (interactive "r")
  (let (words alist-words-compare (formated ""))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons word 1) words)))))
    (defun alist-words-compare (a b)
      "Compare elements from an associative list of words count.
Compare them on count first,and in case of tie sort them alphabetically."
      (let ((a-key (car a))
            (a-val (cdr a))
            (b-key (car b))
            (b-val (cdr b)))
        (if (eq a-val b-val)
            (string-lessp a-key b-key)
          (> a-val b-val))))
    (setq words (cl-sort words 'alist-words-compare))
    (while words
      (let* ((word (pop words))
             (name (car word))
             (count (cdr word)))
        (setq formated (concat formated (format "[%s: %d], " name count)))))
    (when (interactive-p)
      (if (> (length formated) 2)
          (message (substring 0 -2))
        (message "No Words.")))))

;; indent on paste
;; from Prelude: https://github.com/bbatsov/prelude
(defun qingeditor/editor-base/yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) qingeditor/editor-base/yank-indent-threshold)
      (indent-region beg end)))

(qingeditor/editor-base/advise-commands
 "indent" (yank yank-pop ) around
 "If current mode is not one of `qingeditor/editor-base/indent-sensitive-modes'
 indent yanked text (with universal arg don't indent).")
