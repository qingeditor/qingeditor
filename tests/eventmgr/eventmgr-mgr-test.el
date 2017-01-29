;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; 测试事件管理器

(require 'ert)
(load-file (expand-file-name (concat user-emacs-directory "/tests/env-init.el")))

(require 'qingeditor-eventmgr-event)

(defun qingeditor/test/eventmgr/prepare-event (test-func)
  "为测试准备一个`qingeditor/eventmgr/event`对象'。'"
  (let ((event (qingeditor/eventmgr/event/init)))
    (funcall test-func)))
