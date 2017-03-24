;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
(defvar qingeditor/default-company-backends
  '((company-dabbrev-code company-gtags company-etags company-keywords)
    company-files company-dabbrev)
  "The list of default company backends used by qingeditor.
This variable is used to configure mode-specific comany backends in qingeditor.
Backends in this list will always be active in these modes, as well as any
backends added by individual qingeditor modules.")

(defmacro qingeditor/defvar-company-backends (mode)
  "Define a `mode' specific company backend variable with default backends.
The variable name format is company-backends-MODE."
  `(defvar ,(intern (format "company-backends-%S" mode))
     ',qingeditor/default-company-backends
     ,(format "Company backend list for %S" mode)))

(defmacro qingeditor/add-company-hook (mode module)
  "Enable company for the given `mode'.
`mode' must match the symbol passed in `qingeditor/defvar-company-backends'.
The initialization function is hooked to `MODE-hook'."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (init-hook-name (intern (format "qingeditor/%S/init-commany-%S-hook" module mode)))
        (backend-list (intern (format "company-backends-%S" mode))))
    `(when (qingeditor/modulemgr/package-usedp 'company)
       (defun ,init-hook-name ()
         ,(format "Initialize company for %S" mode)
         (when auto-completion-enable-snippets-in-popup
           (setq ,backend-list (mapcar 'qingeditor/show-snippets-in-company
                                       ,backend-list)))
         (set (make-variable-buffer-local 'auto-completion-front-end)
              'company)
         (set (make-variable-buffer-local 'company-backends)
              ,backend-list))
       (add-hook ',mode-hook ',init-hook-name t)
       (add-hook ',mode-hook 'company-mode t))))

(defmacro qingeditor/disable-company (mode)
  "Disable company for the given `mode'.
MODE parameter must match the parameter used in the call to
`qongeditor/add-company-hook'."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (init-hook-name (intern (format "qingeditor/%S/init-commany-%S-hook" module mode))))
    `(progn
       (remove-hook ',mode-hook ',init-hook-name)
       (remove-hook ',mode-hook 'company-mode))))

(defun qingeditor/show-snippets-in-company (backend)
  (if (or (not auto-completion-enable-snippets-in-popup)
          (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defmacro qinegditor/enable-auto-complete (mode module)
  "Enable auto-complete for the given MODE.
The initialization function is hooked to `MODE-hook'."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (init-hook-name (intern (format "qingeditor/%S/init-auto-complete-%S-hook" module mode))))
    `(when (qingeditor/modulemgr/package-usedp 'auto-complete)
       (defun ,init-hook-name ()
         ,(format "Initialize auto-complete for %S" mode)
         (set (make-variable-buffer-local 'auto-completion-front-end)
              'auto-complete)
         (set (make-variable-buffer-local 'company-backends)
              ,(intern (format "company-backends-%S" mode))))
       (defun ,init-hook-name ()
         (,method-name ,module))
       (add-hook ',mode-hook ',init-hook-name)
       (add-hook ',mode-hook 'auto-complete-mode))))

(provide 'qingeditor-auto-completion)
