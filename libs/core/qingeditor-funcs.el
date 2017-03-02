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

(defun qingeditor/run-text-mode-hooks ()
  "Runs `text-mode-hook'. usefull for modes that don't derive from
`text-mode' but should."
  (run-hooks 'text-mode-hook))

(defun qingeditor/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(defun qingeditor/error-delegate ()
  "Decide which error API to delegate to.

Delegates to flycheck if it is enabled and the next-error buffer
is not visible. Otherwise delegates to regular Emacs next-error."
  (if (and (bound-and-true-p flyspell-mode)
           (let ((buf (ignore-errors (next-error-find-buffer))))
             (not (and buf (get-buffer-window buf)))))
      'flycheck
    'emacs))

(defun qingeditor/next-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (let ((sys (qingeditor/error-delegate)))
    (cond
     ((eq 'flycheck sys)
      (call-interactively 'flycheck-next-error))
     ((eq 'emacs sys)
      (call-interactively 'next-error)))))

(defun qingeditor/previous-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (let ((sys (qingeditor/error-delegate)))
    (cond
     ((eq 'flycheck sys)
      (call-interactively 'flycheck-previous-error))
     ((eq 'emacs sys)
      (call-interactively 'next-error)))))

(defun qingeditor/gne-next (num reset)
  "A generalized next-error function. This function can be used
as `next-error-function' in any buffer that conforms ti the
qingeditor generalized next-error API.

The variables `qingeditor/gne-min-line', `qingeditor/gne-max-line',
and `qingeditor/line-func' must be set."
  (when reset
    (setq qingeditor/gne-cur-line qingeditor/gne-min-line))
  (setq qingeditor/gne-cur-line
        (min qingeditor/gne-max-line
             (max qingeditor/gne-min-line
                  (+ num qingeditor/gne-cur-line))))
  (goto-line qingeditor/gne-cur-line)
  (funcall qingeditor/gne-line-func
           (buffer-substring (point-at-bol) (point-at-eol))))

(defun qingeditor/display-and-copy-version ()
  "Echo the current qingeditor version and copy it."
  (let ((msg (format "qingeditor v.%s" qingeditor/version)))
    (message msg)
    (kill-new msg)))

(defun qingeditor/display-startup-echo-area-message ()
  "Change the default welcome message of minibuffer to another one."
  (message "qingeditor is ready."))

(provide 'qingeditor-funcs)
