;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; editor-base init method defs

(defmethod qingeditor/cls/init-abbrev ((this qingeditor/module/editor-base))
  "init abbrev."
  (qingeditor/font/hide-lighter abbrev-mode))

(defmethod qingeditor/cls/init-ace-window ((this qingeditor/module/editor-base))
  "init ace window."
  (use-package ace-window
    :defer t
    :init
    (progn
      (qingeditor/key-binder/set-leader-keys
        "bD" 'spacemacs/ace-kill-this-buffer
        ;; FIXME: Needs new binding.
        ;; "wC" 'spacemacs/ace-center-window
        "wD" 'spacemacs/ace-delete-window
        "wM" 'ace-swap-window
        "wW" 'ace-window)
      ;; set ace-window keys to home-row
      (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))))

(defmethod qingeditor/cls/init-archive-mode ((this qingeditor/module/editor-base))
  "init archive mode."
  (use-package archive-mode
    :commands archive-mode))

(defmethod qingeditor/cls/init-bookmark ((this qingeditor/module/editor-base))
  (use-package bookmark
    :defer t
    :init
    (progn
      (setq bookmark-default-file (concat qingeditor/cache-dir "bookmarks"))
      ;; autosave each change
      (setq bookmark-save-flag 1)
      (qingeditor/key-binder/set-leader-keys "fb" 'bookmark-jump))))
