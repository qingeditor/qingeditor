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
     (should (eq (qingeditor/stack/empty stack) t))
     (should (eq (length (oref stack :data)) 0))
     (qingeditor/stack/push stack 1)
     (should (eq (qingeditor/stack/empty stack) nil))
     (should (eq (length (oref stack :data)) 1))
     (should (eq (qingeditor/stack/count stack) 1))
     (should (eq (qingeditor/stack/top stack) 1))
     (qingeditor/stack/push stack "softboy")
     (should (equal (qingeditor/stack/top stack) "softboy"))
     (should (equal (qingeditor/stack/bottom stack) 1))
     (let (ret
	   not-func)
       (qingeditor/stack/iterate
	stack (lambda (item)
		(push item ret)))
       (should (equal ret '(1 "softboy"))))
     (qingeditor/stack/clear stack)
     (should (eq (length (oref stack :data)) 0))
     (should (eq (qingeditor/stack/empty stack) t)))))
