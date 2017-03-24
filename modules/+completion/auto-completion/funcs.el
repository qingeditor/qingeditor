;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

;; auto-completion key bindings functions

;; key sequence to complete selection

(defvar qingeditor/auto-completion/time nil)
(defvar qingeditor/auto-completion/complete-last-candidate nil)
(defvar qingeditor/auto-completion/shadowed-insert-binding nil)

(defun qing-auto-completion-key-sequence-start ()
  "Initialization auto-completion sequence."
  (interactive)
  (self-insert-command 1)
  (setq qingeditor/auto-completion/complete-last-candidate
        (cond
         ((bound-and-true-p company-mode)
          (nth company-selection company-candidates))))
  ;; enable second key of the sequence
  (let ((second-key (kbd (char-to-string
                          (elt qingeditor/auto-completion/complete-with-key-sequence 1)))))
    (setq qingeditor/auto-completion/shadowed-insert-binding
          (lookup-key (current-local-map) second-key))
    (define-key (current-local-map)
      second-key
      'qing-auto-completion-key-sequence-end))
  ;; set a timer to restore the old bindings
  (run-at-time qingeditor/auto-completion/complete-with-key-sequence-delay
               nil
               'qingeditor/auto-completion/key-sequence-restore)
  (when qingeditor/auto-completion/complete-last-candidate
    (setq qingeditor/auto-completion/time (current-time))))

(defun qing-auto-completion-key-sequence-end ()
  "Check if the auto-completion key sequence has been entered."
  (interactive)
  (if (or (null qingeditor/auto-completion/time)
          (< qingeditor/auto-completion/complete-with-key-sequence-delay
             (float-time (time-since qingeditor/auto-completion/time))))
      (self-insert-command 1)
    (cond
     ((bound-and-true-p company-mode)
      (unless company-candidates
        ;; if the auto-company menu is still active then we don't need to
        ;; delete the last inserted first key of the sequence
        (delete-char -1))
      (let ((company-idle-delay))
        (company-auto-begin)
        (company-finish qingeditor/auto-completion/complete-last-candidate)))))
  (qingeditor/auto-completion/key-sequence-restore)
  (setq qingeditor/auto-completion/time nil))

(defun qingeditor/auto-completion/key-sequence-restore ()
  "Restore the shadowed key bindings used to auto-complete."
  (let ((seconds-key (kbd (char-to-string
                           (elt qingeditor/auto-completion/complete-with-key-sequence 1)))))
    (define-key (current-local-map) seconds-key qingeditor/auto-completion/shadowed-insert-binding)))

(defun qingeditor/auto-completion/set-RET-key-behavior (package)
  "Bind TRE key appropriately for the given `package' and value of
`auto-completion-return-key-behavior'."
  (cond
   ((eq 'company package)
    (let ((map company-active-map))
      (cond
       ((eq 'complete auto-completion-return-key-behavior)
        (define-key map [return] 'company-complete-selection)
        (define-key map (kbd "RET") 'company-complete-selection))
       (t
        (define-key map [return] 'nil)
        (define-key map (kbd "RET") 'nil)))))
   (t (message "Not yet implemented for package %S" package))))

(defun qingeditor/auto-completion/set-TAB-key-behavior (package)
  "Bind TAB key appropriately for the given `package' and value of
`auto-completion-tab-key-behavior'."
  (cond
   ((eq 'company package)
    (let ((map company-active-map))
      (cond
       ((eq 'complete auto-completion-tab-key-behavior)
        (define-key map (kbd "TAB") 'company-complete-selection)
        (define-key map (kbd "<tab>") 'company-complete-selection))
       ((eq 'cycle auto-completion-tab-key-behavior)
        (define-key map (kbd "TAB") 'company-complete-common-or-cycle)
        (define-key map (kbd "<tab>") 'company-complete-common-or-cycle)
        (define-key map (kbd "<S-table>") 'qing-company-complete-common-or-cycle-backward)
        (define-key map (kbd "<backtab>") 'qing-company-complete-common-or-cycle-backward))
       (t
        (define-key map (kbd "TAB") nil)
        (define-key map (kbd "<tab>") nil)))))
   (t (message "Not yet implemented for package %S" package))))

(defun qingeditor/auto-completion/setup-key-sequence (package)
  "Setup the key sequence to complete current selection."
  (when qingeditor/auto-completion/complete-with-key-sequence
    (let ((first-key (elt qingeditor/auto-completion/complete-with-key-sequence 0)))
      (cond ((eq 'company package)
             (define-key company-active-map (kbd (char-to-string first-key))
               'qing-auto-completion-key-sequence-start))
            (t (message "Not yet implemented for package %S" package))))))

;; Editing style

(defun qingeditor/auto-completion/company-active-navigation ()
  "Set navigation."
  (let ((map company-active-map))
    (define-key map (kbd "C-n") 'company-select-next)
    (define-key map (kbd "C-p") 'company-select-previous)
    (define-key map (kbd "C-f") 'company-complete-selection)))

;; Transformers
(defun qingeditor/auto-completion/company-transformer-cancel (candidates)
  "Cancel completion if prefix is in the list
`company-mode-completion-cancel-keywords'"
  (unless (member company-prefix
                  qingeditor/auto-completion/company-mode-completion-cancel-keywords)
    candidates))

(defvar-local qingeditor/auto-completion/company-fci-mode-on-p nil)

(defun qingeditor/auto-completion/company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq qingeditor/auto-completion/company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun qingeditor/auto-completion/company-maybe-turn-on-fci (&rest ignore)
       (when qingeditor/auto-completion/company-fci-mode-on-p (fci-mode 1)))
