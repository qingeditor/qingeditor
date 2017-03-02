;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; qingeditor types defs
(require 'qingeditor-macros)
(require 'qingeditor-funcs)

;;; Standard interactive codes
(qingeditor/define-interactive-code "*"
  "Signal error if the buffer is read only."
  (when buffer-read-only
    (signal 'buffer-read-only nil)))

(qingeditor/define-interactive-code "b" (prompt)
  "Name of existing buffer."
  (list (read-buffer prompt (current-buffer) t)))

(qingeditor/define-interactive-code "c"
  "Read character."
  (list (read-char)))

(qingeditor/define-interactive-code "p"
  "Prefix argument converted to number."
  (list (prefix-numeric-value current-prefix-arg)))

(qingeditor/define-interactive-code "P"
  "Prefix argument in raw form."
  (list current-prefix-arg))

;;; Custom interactive codes
(qingeditor/define-interactive-code "<c>"
  "Count."
  (list (when current-prefix-arg
          (prefix-numeric-value current-prefix-arg))))

(provide 'qingeditor-types)
