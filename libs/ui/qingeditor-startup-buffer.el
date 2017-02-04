;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; some function about qingeditor start up buffer

(defconst qingeditor/startup-buffer/name "*qingeditor*"
  "The name of the qingeditor editor.")

(defvar qingeditor/startup-buffer/warnings nil
  "This variable is to record the warnings of `qingeditor' startup.")

(defun qingeditor/startup-buffer/message (msg &rest args)
  "Display `msg' in message prepend with `(qingeditor)'
The message is displayed only if `init-file-debug' is non `nil'."
  (when init-file-debug
    (qingeditor/message "(qingeditor) %s" (apply 'format msg args))))

(defun qingeditor/startup-buffer/warning (msg &rest args)
  "Display `msg' as a warning message but in buffer `*Message*'.
The message is aways displayed."
  (let ((msg (apply 'format msg args)))
    (qingeditor/message "(qingeditor) Warning: %s" msg)
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

(provide 'qingeditor-startup-buffer)
