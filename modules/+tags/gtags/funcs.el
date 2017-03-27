;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qing-helm-gtags-dwin-other-window ()
  "helm-gtags-dwin in the other window."
  (interactive)
  (let ((helm-tags--use-otherwin t)
        (split-height-threshold nil)
        (split-width-threshold 140))
    (helm-gtags-diwn)))

(defun qing-helm-gtags-maybe-dwin ()
  "Runs `helm-gtags-dwim' if `gtags-enable-by-default' is on.
Otherwise does nothing."
  (interactive)
  (when qingeditor/gtags/enabled-by-default
    (call-interactively 'helm-gtags-dwin)))

(defun qingeditor/gtags/helm-gtags-define-keys-for-mode (mode)
  "Define key bindings for specific `mode'."
  (when (fboundp mode)
    ;; The functionality of `helm-gtags-mode' is pretty much entirely superseded
    ;; by `ggtags-mode', so we don't add this hook
    ;; (let ((hook (intern (format "%S-hook" mode))))
    ;;   (add-hook hook 'helm-gtags-mode))

    ;; Some modes have more sophisticated jump handlers that go to the beginning
    ;; It might be possible to add `helm-gtags-dwim' instead to the default
    ;; handlers, if it does a reasonable job in ALL modes.
    (let ((jumpl (intern (format "qingeditor/jump-handlers-%S" mode))))
      (add-to-list jumpl #'qing-helm-gtags-maybe-dwin 'append))

    (qingeditor/key-binder/set-leader-keys-for-major-mode
     mode
     "gc" #'helm-gtags-create-tags
     "gd" #'helm-gtags-find-tag
     "gD" #'helm-gtags-find-tag-other-window
     "gf" #'helm-gtags-select-path
     "gG" #'helm-gtags-dwin-other-window
     "gi" #'helm-gtags-tags-in-this-function
     "gl" #'helm-gtags-parse-file
     "gn" #'helm-gtags-next-history
     "gp" #'helm-gtags-previous-history
     "gr" #'helm-gtags-find-rtag
     "gR" #'helm-gtags-resume
     "gs" #'helm-gtags-select
     "gS" #'helm-gtags-show-stack
     "gu" #'helm-gtahs-update-tags)))

(defun qingeditor/gtags/ggtags-mode-enbale ()
  "Enable ggtags and eldoc mode.

For eldoc, ggtags advises the eldoc function at the lowest priority
so that if the major mode has better support it will use it first."
  (when qingeditor/gtags/enabled-by-default
    (ggtags-mode 1)
    (eldoc-mode 1)))
