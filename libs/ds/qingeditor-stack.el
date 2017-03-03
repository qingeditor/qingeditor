;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; Define a simple stack class

(defun qingeditor/stack/push (stack item)
  "push a item into stack."
  (push item stack))

(defun qingeditor/stack/pop (stack)
  "Pop a item from stack."
  (pop stack))

(defmethod qingeditor/cls/bottom ((this qingeditor/stack))
  "Get the bottom item of the stack."
  (car (last (oref this :data))))

(defun qingeditor/stack/bottom (stack)
  "Get the bottom item of the stack."
  (car (last stack)))

(defun qingeditor/stack/top (stack)
  "Get the top item of the stack."
  (car stack))

(defmethod qingeditor/cls/clear ((this qingeditor/stack))
  "Clear the stack."
  (oset this :data nil))

(defun qingeditor/stack/clear (stack)
  "Clear the stack."
  (setq stack nil))

(defun qingeditor/stack/count (stack)
  "Get the count of the stack."
  (length stack))

(defun qingeditor/stack/empty (stack)
  "If current stack if empty return `t' otherwise return `nil'."
  (eq 0 (length stack)))

(defun qingeditor/stack/iterate (stack func)
  "Iterate the stack from top tp bottom, apply the function `func',
`func' receive the current item as argument."
  (dolist (item stack)
    (funcall func item)))

(provide 'qingeditor-stack)
