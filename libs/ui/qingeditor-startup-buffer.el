;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; some function about qingeditor start up buffer

(defconst qingeditor/startup-buffer/version-info "0.100"
  "Current version used to display addtion release information.")

(defconst qingeditor/startup-buffer/logo-title "[Q I N G E D I T O R]"
  "The title displayed beneath the logo.")

(defconst qingeditor/startup-buffer/buttons-startup-lists-offset 25
  "Relative position in characters of the home buffer buttons and the home
 buffer startup lists.")

(defconst qingeditor/startup-buffer/banner-length 75
  "Width of a banner.")

(defconst qingeditor/startup-buffer/name "*qingeditor*"
  "The name of the qingeditor editor.")

(defconst qingeditor/startup-buffer/banner-official-png
  (expand-file-name (concat qingeditor/banner-dir "img/qingeditor3.png"))
  "qingeditor official banner image.")

(defconst qingeditor/startup-buffer/purple-heart-png
  (expand-file-name (concat qingeditor/banner-dir "img/heart.png"))
  "Purple heart emoji.")

(defconst qingeditor/startup-buffer/cache-file
  (expand-file-name (concat qingeditor/cache-dir "qingeditor-startup-buffer.el"))
  "Cache file for various persistent data for the qingeditor startup buffer.")

(defvar qingeditor/startup-buffer/startup-lists-length 20
  "Length used fro startup lists with otherwise unspecified bounds.
Set to nil for unbounded.")

(defvar qingeditor/startup-buffer/release-note-version nil
  "If `nil' the release note is displayed. If non-nil it contains
a version number, if the version number is lesser than the current
version the release not it displayed.")

(defvar qingeditor/startup-buffer/note-widgets nil
  "List of widgets used to display the release note.")

(defvar qingeditor/startup-buffer/previous-insert-type nil
  "Previous type of note inserted.")

(defvar qingeditor/startup-buffer/fresh-install
  (not (file-exists-p qingeditor/config/target-cfg-filename))
  "`non-nil' if this emacs instance if a fresh install.")

(defvar qingeditor/startup-buffer/buttons-position nil
  "Offset in characters between the edge of the screen and the beginning of the
home buffer buttons. Do not set this variable.")

(defvar qingeditor/startup-buffer/warnings nil
  "This variable is to record the warnings of `qingeditor' startup.")

(defvar qingeditor/startup-buffer/default-mode-line mode-line-format
  "Backup of default mode line format.")

(defun qingeditor/startup-buffer/message (msg &rest args)
  "Display `msg' in message prepend with `(qingeditor)'
The message is displayed only if `init-file-debug' is non `nil'."
  (when init-file-debug
    (message "(qingeditor) %s" (apply 'format msg args))))

(defun qingeditor/startup-buffer/warning (msg &rest args)
  "Display `msg' as a warning message but in buffer `*Message*'.
The message is aways displayed."
  (let ((msg (apply 'format msg args)))
    (message "(qingeditor) Warning: %s" msg)
    (add-to-list 'qingeditor/startup-buffer/warnings msg 'append)))

(defun qingeditor/startup-buffer/append (msg &optional message-buffer)
  "Append `msg' to `qingeditor' buffer. If `message-buffer' is not nil then
`msg' is also written in `message' buffer."
  (with-current-buffer (get-buffer-create qingeditor/startup-buffer/name)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (if message-buffer
          (message "(qingeditor) %s" msg)))
    (qingeditor/startup-buffer/set-mode-line "")))

(defun qingeditor/startup-buffer/set-mode-line (format)
  "Set mode-line format for `qingeditor' buffer."
  (with-current-buffer (get-buffer-create qingeditor/startup-buffer/name)
    (setq mode-line-format format)))

(defun qingeditor/startup-buffer/replace-last-line (msg &optional messagebuf)
  "Replace the last line of the `qingeditor' buffer with `msg'.
If `messagebuf' is not `nil' then `msg' is also written in message buffer."
  (with-current-buffer (get-buffer-create qingeditor/startup-buffer/name)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (if messagebuf (message "(qingeditor) %s" msg)))
    (qingeditor/startup-buffer/set-mode-line "")))

(defvar qingeditor/startup-buffer/last-width nil
  "Previous width of qingeditor-buffer.")

(defvar qingeditor/startup-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "J") 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "K") 'widget-backward)
    (define-key map (kbd "C-r") 'qingeditor/startup-buffer/refresh)
    (define-key map "q" 'quit-window)
    map)
  "Keymap for qingeditor buffer mode.")

(define-derived-mode qingeditor/startup-buffer-mode fundamental-mode "qingeditor buffer"
  "qingeditor major node for startup screen.
\\<qingeditor/startup-buffer-mode-map>"
  :group 'qingeditor
  :syntax-table nil
  :abbrev-table nil
  (qingeditor/page-break-lines-mode)
  (setq buffer-read-only t
        truncate-lines t))

(defun qingeditor/startup-buffer/render (&optional refresh)
  "Create the special buffer for `qingeditor/startup-buffer-mode' if it doesn't
already exist, and switch to it."
  (interactive)
  (let ((buffer-exists (buffer-live-p (get-buffer qingeditor/startup-buffer/name)))
        (save-line nil))
    (when (or (not (eq qingeditor/startup-buffer/last-width (window-width)))
              (not buffer-exists)
              refresh)
      (setq qingeditor/startup-buffer/banner-length (window-width)
            qingeditor/startup-buffer/last-width qingeditor/startup-buffer/banner-length)
      (with-current-buffer (get-buffer-create qingeditor/startup-buffer/name)
        (qingeditor/page-break-lines-mode)
        (save-excursion
          (when (> (buffer-size) 0)
            (set 'save-line (line-number-at-pos))
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (qingeditor/startup-buffer/set-mode-line "")
          ;; needed in case the buffer was deleted and we are recreating it
          (setq qingeditor/startup-buffer/note-widgets nil)
          (qingeditor/startup-buffer/insert-banner-and-buttons)
          ;; non-nil if emacs-startup-hook was run
          (if (bound-and-true-p qingeditor/initialized)
              (progn
                (qingeditor/startup-buffer/insert-footer)
                (qingeditor/startup-buffer/set-mode-line qingeditor/startup-buffer/default-mode-line)
                (force-mode-line-update)
                (qingeditor/startup-buffer-mode))
            (add-hook 'emacs-startup-hook 'qingeditor/startup-buffer/startup-hook t))))
      (if save-line
          (progn (goto-char (point-min))
                 (forward-line (1- save-line))
                 (forward-to-indentation 0))
        (qingeditor/startup-buffer/goto-link-line))
      (switch-to-buffer qingeditor/startup-buffer/name)
      (qingeditor/redisplay))))

(defmethod qingeditor/startup-buffer/startup-hook ()
  "Code executed when Emacs has finished loading."
  (with-current-buffer (get-buffer qingeditor/startup-buffer/name)
    (qingeditor/startup-buffer/insert-footer)
    (if (> (qingeditor/cls/get-error-count qingeditor/modulemgr) 0)
        (progn
          (qingeditor/startup-buffer-mode)
          (qingeditor/startup-buffer/set-mode-line
           (format
            (concat "%s error(s) at startup!"
                    "qingeditor may not be able to operate properly.")
            (qingeditor/cls/get-error-count qingeditor/modulemgr)))
          (face-remap-add-relative 'mode-line
                                    '((:background "red") mode-line)))
      (qingeditor/startup-buffer/set-mode-line qingeditor/startup-buffer/default-mode-line)
      (qingeditor/startup-buffer-mode))
    (force-mode-line-update)
    (qingeditor/startup-buffer/goto-link-line)))

(defmethod qingeditor/startup-buffer/insert-footer ()
  (save-excursion
    (let* ((maxcol qingeditor/startup-buffer/banner-length)
           (heart-path qingeditor/startup-buffer/purple-heart-png)
           (heart (when (and (display-graphic-p)
                             (image-type-available-p
                              (intern (file-name-extension heart-path))))
                    (create-image heart-path)))
           (heart-size (when heart (car (image-size heart))))
           (build-lhs "Make with ")
           (build-rhs " by the community")
           (buffer-read-only nil))
      (when heart
        (goto-char (point-max))
        (qingeditor/startup-buffer/insert-page-break)
        (insert "\n")
        (insert (make-string (floor (/ (- maxcol
                                          (length build-lhs)
                                          heart-size
                                          (length build-rhs)) 2)) ?\ ))
        (insert build-lhs)
        (insert-image heart)
        (insert build-rhs)
        (insert "\n")))))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-configuration-change-hook 'qingeditor/startup-buffer/resize-on-hook)
            (qingeditor/startup-buffer/resize-on-hook)))

(defun qingeditor/startup-buffer/resize-on-hook ()
  (let ((qingeditor-win (get-buffer-window qingeditor/startup-buffer/name))
        (frame-win (frame-selected-window)))
    (when (and qingeditor/config/startup-buffer-responsive
               qingeditor-win
               (not (window-minibuffer-p frame-win)))
      (with-selected-window qingeditor-win
        (qingeditor/startup-buffer/render)))))

(defun qingeditor/startup-buffer/insert-page-break ()
  "Insert a page break line in spacemacs buffer."
  (qingeditor/startup-buffer/append "\n\n"))

(defun qingeditor/startup-buffer/refresh ()
  "Force recreation of qingeditor buffer."
  (interactive)
  (setq qingeditor/startup-buffer/last-width nil)
  (qingeditor/startup-buffer/render t))

(defun qingeditor/startup-buffer/goto-link-line ()
  "Set point to the beginning of the link line."
  (interactive)
  (with-current-buffer qingeditor/startup-buffer/name
    (goto-char (point-min))
    (with-demoted-errors "qingeditor buffer error: %s"
      (widget-forward 1))))

(defun qingeditor/startup-buffer/insert-banner-and-buttons ()
  "Choose a banner according to `qingeditor.config/startup-banner' and insert it
in qingeditor buffer along with quick underneath.

Easter egg:
Doge spacial text banner can be reachable via `999', `doge' or `random*'.
Cate special text banner can be reachable via `998', `cat' or `random*'.
`random' ignore special banners wheres `random*' does not."
  (let ((banner (qingeditor/startup-buffer/choose-banner))
        (buffer-read-only nil))
    (progn
      (when banner
        (qingeditor/startup-buffer/message (format "Banner: %s" banner))
        (if (image-type-available-p (intern (file-name-extension banner)))
            (qingeditor/startup-buffer/insert-image-banner banner)
          (qingeditor/startup-buffer/insert-ascii-banner-centered banner))
        (qingeditor/startup-buffer/inject-version))
      (qingeditor/startup-buffer/insert-buttons)
      (qingeditor/redisplay))))

(defun qingeditor/startup-buffer/insert-image-banner (banner)
  "Display an image banner."
  (when (file-exists-p banner)
    (let* ((title qingeditor/startup-buffer/logo-title)
           (spec (create-image banner))
           (size (image-size spec))
           (width (car size))
           (left-margin (max 0 (floor (- qingeditor/startup-buffer/banner-length width) 2))))
      (goto-char (point-min))
      (insert "\n")
      (insert (make-string left-margin ?\ ))
      (insert-image spec)
      (insert "\n\n")
      (insert (make-string (max 0 (floor (/ (- qingeditor/startup-buffer/banner-length
                                               (+ (length title) 1)) 2))) ?\ ))
      (insert (format "%s\n\n" title)))))

(defun qingeditor/startup-buffer/inject-version ()
  "Inject the current version of qingeditor in the first line of the
buffer, right justified."
  (with-current-buffer (get-buffer-create qingeditor/startup-buffer/name)
    (save-excursion
      (let ((maxcol qingeditor/startup-buffer/banner-length)
            (version (format "%s@%s (%s)"
                             qingeditor/version
                             emacs-version
                             qingeditor/config/distribution))
            (buffer-read-only nil))
        (goto-char (point-min))
        (delete-region (point) (progn (end-of-line) (point)))
        (insert (format (format "%%%ds" maxcol) version))))))

(defun qingeditor/startup-buffer/insert-buttons ()
  (goto-char (point-max))
  (qingeditor/startup-buffer/insert-shortcut "m" "[?]" t)
  (widget-create 'url-link
                 :tag (propertize "?" 'face 'font-lock-doc-face)
                 :help-echo "Open the quickhelp."
                 :action (lambda (&rest ignore)
                           (qingeditor/startup-buffer/toggle-note
                            (concat qingeditor/info-dir "quickhelp.txt")
                            ;; if nil if required,
                            ;; just delete the current note widgets
                            (qingeditor/startup-buffer/insert-note-p 'quickhelp)))
                 :mouse-face 'highlight
                 :follow-link "\C-m")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Homepage" 'face 'font-lock-keyword-face)
                 :help-echo "Open the qingeditor Github page in your browser."
                 :mouse-face 'highlight
                 :follow-link "\C-m")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Documentation" 'face 'font-lock-keyword-face)
                 :help-echo "Open the qingeditor documentation in your browser."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "http://qingeditor.org/documentations/")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Forum" 'face 'font-lock-keyword-face)
                 :help-echo
                 "Ask questions and talk with other users in our forum."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "http://bbs.qingeditor.org")
  (insert " ")
  (widget-create 'push-button
                 :help-echo "Update qingeditor core and modules."
                 :action (lambda (&rest ignore) (spacemacs/switch-to-version))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Update Qingeditor" 'face 'font-lock-keyword-face))
  (let ((len (- (line-end-position)
                (line-beginning-position))))
    (qingeditor/startup-buffer/center-line)
    (setq qingeditor/startup-buffer/buttons-position
          (- (line-end-position)
             (line-beginning-position)
             len)))
  (insert "\n")
  (widget-create 'push-button
                 :help-echo "Update all ELPA packages to the latest versions."
                 :action (lambda (&rest ignore)
                           (configuration-layer/update-packages))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Update Packages" 'face 'font-lock-keyword-face))
  (insert " ")
  (widget-create 'push-button
                 :help-echo
                 "Rollback ELPA package updates if something got borked."
                 :action (lambda (&rest ignore)
                           (call-interactively 'configuration-layer/rollback))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Rollback Package Update"
                             'face 'font-lock-keyword-face))
  (qingeditor/startup-buffer/center-line)
  (insert "\n")
  (widget-create 'push-button
                 :tag (propertize "Release Notes"
                                  'face 'font-lock-preprocessor-face)
                 :help-echo "Hide or show the Changelog"
                 :action (lambda (&rest ignore)
                           (spacemacs-buffer/toggle-note
                            (concat spacemacs-release-notes-directory
                                    spacemacs-buffer-version-info
                                    ".txt")
                            ;; if nil is returned,
                            ;; just delete the current note widgets
                            (spacemacs-buffer//insert-note-p 'release-note)))
                 :mouse-face 'highlight
                 :follow-link "\C-m")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Search in Qingeditor"
                                  'face 'font-lock-function-name-face)
                 :help-echo "Search Spacemacs contents."
                 :action
                 (lambda (&rest ignore)
                   (let ((comp-frontend
                          (cond
                           ((configuration-layer/layer-usedp 'helm)
                            'helm-spacemacs-help)
                           ((configuration-layer/layer-usedp 'ivy)
                            'ivy-spacemacs-help))))
                     (call-interactively comp-frontend)))
                 :mouse-face 'highlight
                 :follow-link "\C-m")
  (qingeditor/startup-buffer/center-line)
  (insert "\n\n"))

(defun qingeditor/startup-buffer/center-line ()
  (let* ((width (current-column))
         (margin (max 0 (floor (/ (- qingeditor/startup-buffer/banner-length width) 2)))))
    (beginning-of-line)
    (insert (make-string margin ?\ ))
    (end-of-line)))

(defmacro qingeditor/startup-buffer/insert-shortcut (shortcut-char search-label
                                                                   &optional no-next-line)
  `(define-key qingeditor/startup-buffer-mode-map
     ,shortcut-char
     (lambda ()
       (interactive)
       (unless (search-forward ,search-label (point-max) t)
         (search-backward ,search-label (point-min) t))
       ,@(unless no-next-line
           '((forward-line 1)))
       (back-to-indentation))))

(defun qingeditor/startup-buffer/choose-banner ()
  "Return the full path of a banner based on the config value."
  (when qingeditor/config/startup-banner
    (cond ((eq 'official qingeditor/config/startup-banner)
           (if (and (display-graphic-p)
                    (image-type-available-p 'png))
               qingeditor/startup-buffer/banner-official-png
             (qingeditor/startup-buffer/get-banner-path 1)))
          ((eq 'random qingeditor/config/startup-banner)
           (qingeditor/startup-buffer/choose-random-text-banner))
          ((eq 'random* qingeditor/config/startup-banner)
           (qingeditor/startup-buffer/choose-random-text-banner t))
          ((eq 'dage qingeditor/config/startup-banner)
           (qingeditor/startup-banner/get-banner-path 999)f)
          ((eq 'cat qingeditor/config/startup-banner)
           (qingeditor/startup-banner/get-banner-path 998))
          ((integerp qingeditor/config/startup-banner)
           (qingeditor/startup-banner/get-banner-path qingeditor/config/startup-banner))
          ((and qingeditor/config/startup-banner
                (image-type-available-p (intern (file-name-extension
                                                 qingeditor/config/startup-banner)))
                (display-graphic-p))
           (if (file-exists-p qingeditor/config/startup-banner)
               qingeditor/config/startup-banner
             (qingeditor/startup-buffer/warning (format "could not find banner %s"
                                                        qingeditor/config/startup-banner))
             (qingeditor/startup-banner/get-banner-path 1)))
          (t (qingeditor/startup-banner/get-banner-path 1)))))

(provide 'qingeditor-startup-buffer)
