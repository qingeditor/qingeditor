(require 'ert)
(load-file (expand-file-name (concat user-emacs-directory "/tests/env-init.el")))

(require 'qingeditor-stack)

(defun qingeditor/test/ds/prepare-stack (test-func)
  "为测试准备一个`qingeditor/stack`对象'。'"
  (let ((stack (qingeditor/stack)))
    (funcall test-func)))

(ert-deftest qingeditor/test/ds/stack-test ()
  :tags '(qingeditor/stack/test)
  (qingeditor/test/ds/prepare-stack
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
     (let (ret
	   not-func)
       (qingeditor/cls/iterate
	stack (lambda (item)
		(push item ret)))
       (should (equal ret '(1 "softboy"))))
     (qingeditor/cls/clear stack)
     (should (eq (length (oref stack :data)) 0))
     (should (eq (qingeditor/cls/empty stack) t)))))
