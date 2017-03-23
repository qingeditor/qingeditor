;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

;; golden ratio
(defun qingeditor/editor-ui-visual/no-golden-artio-for-buffers (bufname)
  "Disable golden-ratio if `bufname' is the name of a visible buffer."
  (and (get-buffer bufname) (get-buffer-window bufname 'visible)))

(defun qinegditor/editor-ui-visual/no-golden-ratio-guide-key ()
  "Disable golden-ratio for guide-key popwin buffer."
  (or (qingeditor/editor-ui-visual/no-golden-artio-for-buffers " *guide-key*")
      (qingeditor/editor-ui-visual/no-golden-artio-for-buffers " *popwin-dummy*")))

;; ansi-colors
(defun qingeditor/editor-ui-visual/compilation-buffer-apply-ansi-colors ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
