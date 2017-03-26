;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defvar qingeditor/helm/popwin-mode nil
  "Temp variable to store the value of `popwin-mode'.")

(defun qingeditor/helm/clean-up ()
  "Cleanup some helm related states when quitings."
  (setq overriding-terminal-local-map nil))

(defun qingeditor/helm/prepare-display ()
  "Prepare necessary settings to make Helm display properly."
  (setq qingeditor/editor-completion/display-buffer-alist
        display-buffer-alist)
  ;; the only buffer to display is Helm, nothing else we must set this
  ;; otherwise Helm cannot reuse its own windows for copyinng/deleting
  ;; etc... because of existing popwin buffers in the alist
  (setq display-buffer-alist nil)
  (setq qingeditor/helm/popwin-mode popwin-mode)
  (when popwin-mode
    (popwin-mode -1)))

(defun qingeditor/helm/restore-display ()
  ;; we must enable popwin-mode first then restore `display-buffer-alist'
  ;; Otherwise, popwin keeps adding up its own buffers to
  ;; `display-buffer-alist' and could slow down Emacs as the list grows
  (when qingeditor/helm/popwin-mode
    (popwin-mode))
  (setq display-buffer-alist qingeditor/editor-completion/display-buffer-alist))


;; REPLs integration

(defun qing-helm-available-repls ()
  "Show all the repls available."
  (interactive)
  (let ((helm-available-repls
         `((name . "HELM available REPLs")
           (candidates . ,(mapcar #'car qingeditor/repl-list))
           (action . (lambda (candidate)
                       (let ((repl (cdr (assoc candidate qingeditor/repl-list))))
                         (require (car repl))
                         (call-interactively (cdr repl))))))))
    (helm :sources '(helm-available-repls)
          :buffer "*helm repls*")))


;; Search tools integration

(defun qing-helm-do-grep-region-or-symbol
    (&optional targs use-region-or-symbol-p)
  "Version of `helm-do-grep' with a default input."
  (interactive)
  (require 'helm)
  (cl-letf*
      (((symbol-function 'this-fn) (symbol-function 'helm-do-grep-1))
       ((symbol-function 'helm-do-grep-1)
        (lambda (targets &optional recurse zgrep exts
                         default-input region-or-symbol-p)
          (let* ((new-input (when region-or-symbol-p
                              (if (region-active-p)
                                  (buffer-substring-no-properties
                                   (region-beginning) (region-end))
                                (thing-at-point 'symbol t))))
                 (quoted-input (when new-input
                                 (rxt-quote-pcre new-input))))
            (this-fn targets recurse zgrep exts
                     default-input quoted-input))))
       (preselection (or (dired-get-filename nil t)
                         (buffer-file-name (current-buffer))))
       (targets (if targs
                    targs
                  (helm-read-file-name
                   "Search in file(s)"
                   :marked-candidates t
                   :preselect (if helm-ff-transformer-show-only-basename
                                  (helm-basename preselection)
                                preselection)))))
    (helm-do-grep-1 targets nil nil nil nil use-region-or-symbol-p)))

(defun qing-helm-file-do-grep ()
  "Search in current file with `grep' using a default input."
  (interactive)
  (qing-helm-do-grep-region-or-symbol
   (list (buffer-file-name (current-buffer))) nil))

(defun qing-helm-file-do-grep-region-or-symbol ()
  "Search in current file with `grep' using a default input."
  (interactive)
  (qing-helm-do-grep-region-or-symbol
   (list (buffer-file-name (current-buffer))) t))

(defun qing-helm-files-do-grep ()
  "Search in files with `grep'."
  (interactive)
  (qing-helm-do-grep-region-or-symbol nil nil))

(defun qing-helm-files-do-grep-region-or-symbol ()
  "Search in files with `grep' using a default input."
  (interactive)
  (qing-helm-do-grep-region-or-symbol nil t))

(defun qing-helm-buffers-do-grep ()
  "Search in opened buffers with `grep'."
  (interactive)
  (let ((buffers (cl-loop for buffer in (buffer-list)
                          when (buffer-file-name buffer)
                          collect (buffer-file-name buffer))))
    (qing-helm-do-grep-region-or-symbol buffers nil)))

(defun qing-helm-buffers-do-grep-region-or-symbol ()
  "Search in opened buffers with `grep' with a default input."
  (interactive)
  (let ((buffers (cl-loop for buffer in (buffer-list)
                          when (buffer-file-name buffer)
                          collect (buffer-file-name buffer))))
    (qing-helm-do-grep-region-or-symbol buffers t)))

(defun qing-resume-last-search-buffer ()
  "open last helm-ag or hgrep buffer."
  (interactive)
  (cond ((get-buffer "*helm ag results*")
         (switch-to-buffer-other-window "*helm ag results*"))
        ((get-buffer "*helm-ag*")
         (helm-resume "*helm-ag*"))
        ((get-buffer "*hgrep*")
         (switch-to-buffer-other-window "*hgrep*"))
        (t
         (message "No previous search buffer found"))))

(defun qing-helm-find-files (arg)
  "Custom spacemacs implementation for calling helm-find-files-1.
Removes the automatic guessing of the initial value based on thing at point."
  (interactive "P")
  (let* ((hist (and arg helm-ff-history (helm-find-files-history)))
         (default-input hist)
         (input (cond
                 ((and (eq major-mode 'dired-mode) default-input)
                  (file-name-directory default-input))
                 ((and (not (string= default-input ""))
                       default-input)
                  (t (expand-file-name (helm-current-directory)))))))
    (set-text-properties 0 (length input) nil input)
    (helm-find-files-1 input)))



;; Key bindings

(defmacro qingeditor/set-helm-key (keys func)
  "Define a key bindings for `func' using `keys'.
Ensure that helm is required before calling `func'."
  (let ((func-name (intern (format "qing-%s" (symbol-name func)))))
    `(progn
       (defun ,func-name ()
         ,(format "Wrapper to ensure that `helm' is loaded before calling %s."
                  (symbol-name func))
         (interactive)
         (require 'helm)
         (call-interactively ',func))
       (qingeditor/key-binder/set-leader-keys ,keys #',func-name))))

(defun qingeditor/helm/find-files-edit (candidate)
  "Open a dired buffer and immediately switches to editable mode."
  (dired (file-name-directory candidate))
  (dired-goto-file candidate)
  (dired-toggle-read-only))

(defun qing-helm-find-files-edit ()
  "Exits helm, open a dired buffer and immediately switches to editable mode."
  (interactive)
  (helm-exit-and-execute-action #'qingeditor/helm/find-files-edit))

(defun qing-helm-jump-in-buffer ()
  "Jump in buffer using `imenu' facilities and helm."
  (interactive)
  (call-interactively
   (cond
    ((eq major-mode 'org-mode) 'helm-org-in-buffer-headings)
    (t 'helm-semantic-or-imenu))))



(defun qing-helm-find-files (arg)
  "Custom qingeditor implementation for calling heml-find-files-1.
Removes the automatic guessing of the initvalue based on thing at point."
  (interactive "P")
  (let* ((hist (and arg helm-ff-history (helm-find-files-history)))
         (default-input hist)
         (input (cond ((and (eq major-mode 'dired-mode) default-input)
                       (file-name-directory default-input))
                      ((and (not (string= default-input ""))
                            default-input))
                      (t (expand-file-name (helm-current-directory))))))
    (set-text-properties 0 (length input) nil input)
    (helm-find-files-1 input)))

(defun qingeditor/helm/helm-cleanup ()
  "Cleanup some helm related states when quitings."
  ;; deactivate any running transient map (transient-state)
  (setq overriding-terminal-local-map nil))



;; Generalized next-error interface
(defun qingeditor/helm/gne-init-helm-ag (&rest args)
  (with-current-buffer "*helm ag results*"
    (setq qingeditor/gne-min-line 5
          qingeditor/gne-max-line
          (save-excursion
            (goto-char (point-max))
            (previous-line)
            (line-number-at-pos))
          qingeditor/gne-line-func
          (lambda (c)
            (helm-ag--find-file-action
             c 'find-file helm-ag--search-this-file-p))
          next-error-function #'qingeditor/gne-next)))

(defun qingeditor/helm/gne-init-helm-grep (&rest args)
  (with-current-buffer "*hgrep*"
    (setq qingeditor/gne-min-line 5
          qingeditor/gne-max-line
          (save-excursion
            (goto-char (point-max))
            (previous-line)
            (line-number-at-pos))
          qingeditor/gne-line-func #'helm-grep-action
          next-error-function #'qingeditor/gne-next)))
