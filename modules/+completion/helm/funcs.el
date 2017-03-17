;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; helm config module funcs

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
