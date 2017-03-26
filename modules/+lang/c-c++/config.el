;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defvar qingeditor/c-c++/enable-clang-support nil
  "If non nil clang related packages and configuration are enabled.")

(defvar qingeditor/c-c++/default-mode-for-headers 'c++-mode
  "Default mode to open header files. Can be `c-mode' or `c++-mode'.")

(qingeditor/defvar-company-backends c-mode-common)
(qingeditor/defvar-company-backends cmake-mode)

(qingeditor/define-jump-handlers c++-mode)
(qingeditor/define-jump-handlers c-mode)


