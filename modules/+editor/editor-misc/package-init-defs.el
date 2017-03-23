;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qingeditor/editor-misc/init-dumb-jump ()
  (use-package dumb-jump
    :defer t
    :init
    (progn
      ;; not activating `dumb-jump-mode' because it only adds key bindings, and
      ;; they conflict with existing bindings
      (qingeditor/key-binder/set-leader-keys "jq" #'dumb-jump-quick-look)
      ;; Since it's dumb, we add it to the end of the default jump handlers. At
      ;; the time of writing it is the only default jump handler. (gtags remains
      ;; mode-local)
      (add-to-list 'qingeditor/default-jump-handlers #'dumb-jump-go 'append))))

(defun qingeditor/editor-misc/init-request ()
  (setq request-storage-directory
        (concat qingeditor/cache-dir "request/")))
