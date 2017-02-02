(require 'ert)
(load-file (expand-file-name (concat user-emacs-directory "/tests/env-init.el")))

(require 'qingeditor-eventmgr-response-collection)

(defun qingeditor/test/eventmgr/prepare-response-collection (test-func)
  "为测试准备一个`qingeditor/eventmgr/response-collection`对象'。'"
  (let ((stack (qingeditor/eventmgr/response-collection)))
    (funcall test-func)))

(ert-deftest qingeditor/test/eventmgr/response-collection-test ()
  :tags '(qingeditor/eventmgr/response-collection)
  (qingeditor/test/eventmgr/prepare-response-collection
   (lambda ()
     (should (eq (qingeditor/cls/empty stack) t))
     (should (eq (length (oref stack :data)) 0))
     (qingeditor/cls/push stack 1)
     (should (eq (qingeditor/cls/empty stack) nil))
     (should (eq (length (oref stack :data)) 1))
     (should (eq (qingeditor/cls/count stack) 1))
     (should (eq (qingeditor/cls/top stack) 1))
     (qingeditor/cls/push stack "softboy")
     (should (equal (qingeditor/cls/top stack) "softboy"))
     (should (equal (qingeditor/cls/bottom stack) 1))
     (should (equal (qingeditor/cls/stopped stack) nil))
     (should (equal (qingeditor/cls/contains stack "softboy") t))
     (should (equal (qingeditor/cls/contains stack "not-exist") nil))
     )))
