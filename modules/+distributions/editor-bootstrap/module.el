;;; qingeditor --- a distribution of Emacs editor
;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; The editor-bootstrap module class
;;; Code:

(qingeditor/define-module
 editor-bootstrap
 "The editor-bootstrap config  module"
 :has-extra-funcs-defs t
 :has-extra-config t
 :require-packages
 '((async :stage bootstrap)
   (bind-map :stage bootstrap)
   (bind-key :stage bootstrap)
   (diminish :stage bootstrap)
   (hydra :stage bootstrap)
   (use-package :stage bootstrap)
   (which-key :stage bootstrap))
 )
