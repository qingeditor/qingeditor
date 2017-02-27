;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; editor base mode extra funcs defs
(require 'cl-lib)

(defun qingeditor/editor-base/run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (format "%S-local-vars-hook" major-mode))))

(defun qingeditor/editor-base/split-and-new-line ()
  "Split a quoted strings or s-expression and insert a new line with
auto-indent."
  (interactive)
  (sp-split-sexp 1)
  (sp-newline))

(defun qingeditor/push-mark-and-goto-begnning-of-line ()
  "Push a mark at current location and go to the beginning of the line."
  (interactive)
  (push-mark (point))
  (qingeditor/beginning-of-line))

(defun qingeditor/push-mark-and-goto-end-of-line ()
  "Push a mark at current location and go to the end of the line."
  (interactive)
  (push-mark (point))
  (qingeditor/end-of-line))

(insert-before-markers)

;; from Prelude
;; TODO: dispatch these in the modules
(defvar qingeditor/editor-base/indent-sensitive-modes
  '(coffee-mode
    elm-mode
    haml-mode
    haskell-mode
    slim-mode
    makefile-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    python-mode
    yaml-mode)
  "Modes for which auto-indenting is suppressed.")

(defcustom qingeditor/editor-base/yank-indent-threshold 1000
  "Threshold (# char) over which indentation does not automatically occur.")

(defcustom qingeditor/editor-base/large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                 doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                 pdf-view-mode)
  "Major modes which `qingeditor/editor-base/check-large-file' will not be
automatically applied to.")

;; check when opening large files - literal file open
(defun qingeditor/editor-base/check-large-file ()
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and
           (not (memq major-mode qingeditor/editor-base/large-file-modes-list))
           size (> size (* 1024 1024 qingeditor/config/large-file-size))
           (y-or-n-p (format (concat "%s is a large file, open literally to "
                                     "avoid performance issues?")
                             filename)))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))
