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

;; custom interactive codes
(qingeditor/define-interactive-code "<c>"
  "Count."
  (list (when current-prefix-arg
          (prefix-numeric-value
           current-prefix-arg))))

(provide 'qingeditor-types)
