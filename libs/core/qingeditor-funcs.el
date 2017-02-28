;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define some untils functions
(require 'qingeditor-funcs-common)

(defun qingeditor/mplist-get (plist prop)
  "Get the values associated to `prop' in `plist', a modified plist.

A modified plist is none where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun qingeditor/mplist-remove (plist prop)
  "Return a copy of a modified `plist' without `prop' and its values.

If there are multiple properties with the same keyword, only the first property
and its values are removed."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (push (pop tail) result))
    (when (eq prop (car tail))
      (pop tail)
      (while (and (consp tail) (not (keywordp (car tail))))
        (pop tail)))
    (while (consp tail)
      (push (pop tail) result))
    (nreverse result)))

;; Originally base on http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun qingeditor/dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in `varlist' to a file `filename'."
  (with-temp-file filename
    (qingeditor/core/dump varlist (current-buffer))
    (make-directory (file-name-directory filename) t)))

;; Originally base on http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun qingeditor/dump (varlist buffer)
  "insert into buffer the setq statement to recreate the variables in `varlist'."
  (cl-loop for var in varlist do
           (print (list 'setq var (list 'quote (symbol-value var)))
                  buffer)))

;; from https://gist.github.com/3402786
(defun qingeditor/toggle-maximize-buffer ()
  "toggle maximized buffer state"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(defun qingeditor/run-text-mode-hooks ()
  "Runs `text-mode-hook'. useful for modes that don't derive from
`progn-mode' but should."
  (run-hooks 'text-mode-hook))

(defun qingeditor/system-is-mac ()
  (eq system-type 'darwin))

(defun qingeditor/system-is-linux ()
  (eq system-type 'gnu/linux))

(defun qingeditor/system-is-mswindows ()
  (eq system-type 'windows-nt))

(defun qingeditor/window-system-is-mac ()
  ;; in Emacs 25+的mac `(window-system)' return `ns'
  (memq (window-system) '(mac ns)))

(defvar qingeditor/init-redisplay-count-private 0
  "The number of calls to `redisplay'.")

(defun qingeditor/redisplay ()
  "`redisplay' wrapper."
  (setq qingeditor/init-redisplay-count-private
        (1+ qingeditor/init-redisplay-count-private)))

(defun qingeditor/modulemgr ()
  "Return the global module manager object."
  qingeditor/modulemgr)

(defun qingeditor/user-config-filename ()
  "Get the user config filename."
  qingeditor/config/target-cfg-filename)

(provide 'qingeditor-funcs)
