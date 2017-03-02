;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; define qingeditor commands
;; functions from evil packages

(require 'qingeditor-funcs)

(defun qingeditor/next-line (&optional count)
  "Move the cursor `count' lines down."
  (let (line-move-visual)
    (qingeditor/line-move (or count 1))))

(defun qingeditor/previous-line (&optional count)
  "Move the cursor `count' lines up."
  (let (line-move-visual)
    (qingeditor/line-move (- (or count 1)))))

(defun qingeditor/next-visual-line (count)
  "Move the cursor `count' screen lines down."
  (let ((line-move-visual t))
    (qingeditor/line-move (or count 1))))

(defun qingeditor/previous-visual-line (count)
  "Move the cursor `count' screen lines up."
  (let ((line-move-visual t))
    (qingeditor/line-move (- (or count 1)))))

(defun qingeditor/line (count)
  "Move `count - 1' lines down."
  (let (line-move-visual)
    ;; Catch bob and eob errors. These are caused when not moving
    ;; point starting in the first or last line, respectively. In this
    ;; case the current line should be selected.
    (condition-case err
        (qingeditor/line-move (1- (or count 1)))
      ((beginning-of-buffer end-of-buffer)))))

(defun qingeditor/beginning-of-line (count)
  "Move the cursor to the beginning of the current line."
  (move-beginning-of-line nil))

(defun qingeditor/end-of-line (count)
  "Move the cursor to the end of the current line.
If `count' is given, move `count - 1' lines downward first."
  (move-end-of-line count)
  (when qingeditor/track-eol
    (setq temporary-goal-column most-positive-fixnum
          this-command 'next-lien))
  (qingeditor/adjust-cursor)
  (when (eolp)
    (setq qingeditor/this-type 'exclusive)))

(defun qingeditor/indent (beg end)
  "Indent text."
  (if (and (= beg (line-beginning-position))
           (= end (line-end-position)))
      ;; since some Emacs modes can only indent one line at a time,
      ;; implement "==" as a call to `indent-according-to-mode'
      (indent-according-to-mode)
    (goto-char beg)
    (indent-region beg end))
  ;; We also need to tabify or untabify the leading white characters
  (when qingeditor/indent-convert-tabs
    (let* ((beg-line (line-number-at-pos beg))
           (end-line (line-number-at-pos end))
           (ln beg-line)
           (convert-white (if indent-tabs-mode 'tabify 'untabify)))
      (save-excursion
        (while (<= ln end-line)
          (goto-char (point-min))
          (forward-line (- ln 1))
          (back-to-indentation)
          ;; Wether tab or space should be used is determined by indent-tabs-mode
          (funcall convert-white (line-beginning-position) (point))
          (setq ln (1+ ln)))))
    (back-to-indentation)))

(defun qing-home ()
  "the command wrapper of `qingeditor/startup-buffer/refresh'."
  (interactive)
  (qingeditor/startup-buffer/refresh))

(defun qing-home-delete-other-windows ()
  "Open home qingeditor buffer and delete other windows.
Useful for making the home buffer the only visible buffer in the frame."
  (interactive)
  (qing-home)
  (delete-other-windows))

(defun qing-cycle-qingeditor-theme ()
  "command wrapper of `qingeditor/theme/cycle-qingeditor-theme'."
  (interactive)
  (qingeditor/theme/cycle-qingeditor-theme))

(defun qing-alternate-buffer (&optional window)
  "command wrapper of `qingeditor/alternate-buffer'."
  (interactive)
  (qingeditor/alternate-buffer window))

;; error commands

(defun qing-previous-error (&optional n reset)
  "command wrapper of `qingeditor/previous-error'"
  (interactive "P")
  (qingeditor/previous-error n reset))

(defun qing-next-error (&optional n reset)
  "command wrapper of `qingeditor/next-error'."
  (interactive "P")
  (qingeditor/next-error n reset))

(provide 'qingeditor-commands)
