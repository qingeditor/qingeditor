;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defvar qingeditor/default-jump-handlers '()
  "List of jump handlers available in every mode.")

(defvar-local qingeditor/jump-handlers '()
  "List of jump handlers local to this buffer.")

(defmacro qingeditor/define-jump-handlers (mode &rest handlers)
  "Defines jump handlers for the given `mode'.
This defines a variable `qingeditor/jump-handlers-MODE' to which
handlers can be added and a function added to MODE-hook of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "qingeditor/init-jump-handlers-%S" mode)))
        (handlers-list (intern (format "qingeditor/jump-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific jump handlers for %S. "
                          "These take priority overy those in "
                          "`qingeditor/default-jump-handlers'.")
                  mode))

       (defun ,func ()
         (setq qingeditor/jump-handlers
               (append ,handlers-list qingeditor/default-jump-handlers)))
       (add-hook ',mode-hook #',func)
       (with-eval-after-load 'bind-map
         (qingeditor/key-binder/set-leader-keys-for-major-mode
          ',mode
          "gg" 'qingeditor/jump-to-definition)))))

(defun qing-jump-to-definition ()
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
          (old-point (point)))
      (dolist (handler-spec qingeditor/jump-handlers)
        (let ((handler (if (listp handler-spec) (car handler-spec) handler-spec))
              (async (when (listp handler-spec)
                       (plist-get (cdr handler-spec) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or async
                    (not (eq old-point (point)))
                    (not (equal old-buffer (current-buffer))))
            (throw 'done t)))))))

(provide 'qingeditor-jump)
