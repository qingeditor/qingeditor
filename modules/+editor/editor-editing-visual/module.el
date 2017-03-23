;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(qingeditor/define-module
 editor-editing-visual
 "The editor-editing-visual config module"
 :require-packages
 '(
   ;; default
   adaptive-wrap
   auto-highlight-symbol
   column-enforce-mode
   hide-comnt
   highlight-indentation
   highlight-numbers
   highlight-parentheses
   ;; waiting for an overlay bug to be fixed
   ;; see https://github.com/syl20bnr/spacemacs/issues/2529
   (hl-anything :exclude t)
   indent-guide
   rainbow-delimiters
   volatile-highlights))
