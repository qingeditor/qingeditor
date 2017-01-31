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

(require 'qingeditor-eventmgr-mgr)

(defun qingeditor/test/eventmgr/mgr/prepare-mgr (test-func)
  "为测试准备一个`qingeditor/eventmgr`对象'。'"
  (let ((mgr (qingeditor/eventmgr/mgr/init)))
    (funcall test-func)))

(ert-deftest qingeditor/test/eventmgr/mgr-set-identifiers-test ()
  :tags '(qingeditor/test/eventmgr/mgr/prepare-mgr)
  (qingeditor/test/eventmgr/mgr/prepare-mgr
   (lambda ()
     (qingeditor/eventmgr/mgr/set-identifiers
      mgr '("identifier1" "identifier1" "identifier2"))
     (should (equalp (oref mgr :identifiers) '("identifier1" "identifier2"))))))


(ert-deftest qingeditor/test/eventmgr/mgr-add-identifiers-test ()
  :tags '(qingeditor/test/eventmgr/mgr/prepare-mgr)
  (qingeditor/test/eventmgr/mgr/prepare-mgr
   (lambda ()
     (qingeditor/eventmgr/mgr/set-identifiers
      mgr '("identifier1"))
     (qingeditor/eventmgr/mgr/add-identifiers
      mgr '("identifier1" "identifier2" "identifier3"))
     (should (equalp (oref mgr :identifiers) '("identifier1" "identifier2" "identifier3")))
     (qingeditor/eventmgr/mgr/add-identifiers
      mgr '("identifier1" "identifier2" "identifier3"))
     (should (equalp (oref mgr :identifiers) '("identifier1" "identifier2" "identifier3"))))))
