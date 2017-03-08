;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; editor-bootstrap module extra funcs

(defun qingeditor/editor-bootstrap/diminish-hook (_)
  "Display diminished lighter in vanilla Emacs mode-line."
  ;; TODO only when qingeditor initialize complete
  (let ((unicodep (qingeditor/symbol-value
                   qingeditor/config/show-mode-line-unicode-symbols)))
    (cl-loop for (mode uni nouni) in qingeditor/font/diminished-minor-modes
             do (diminish mode (if unicodep uni nouni)))))

(defun qingeditor/editor-bootstrap/hydra-key-doc-function (key key-width doc doc-width)
  "Custom hint documentation format for keys."
  (format (format "[%%%ds] %%%ds" key-width (- -1 doc-width))
          key doc))
