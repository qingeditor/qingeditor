;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; Define a simple stack class

(defclass qingeditor/stack ()
  ((data
    :initarg :data
    :initform nil
    :type list
    :documetation "The data repo that store stack item."))
  :documentation "Define a simple stack class

Provide a simple interface that support stack operation.")

(defmethod qingeditor/cls/push ((this qingeditor/stack) data)
  "push a item into stack."
  (push data (oref this :data))
  this)

(defmethod qingeditor/cls/pop ((this qingeditor/stack))
  "Pop a item from stack."
  (pop (oref this :data)))

(defmethod qingeditor/cls/bottom ((this qingeditor/stack))
  "Get the bottom item of the stack."
  (car (last (oref this :data))))

(defmethod qingeditor/cls/top ((this qingeditor/stack))
  "Get the top item of the stack."
  (car (oref this :data)))

(defmethod qingeditor/cls/clear ((this qingeditor/stack))
  "Clear the stack."
  (oset this :data nil))

(defmethod qingeditor/cls/count ((this qingeditor/stack))
  "Get the count of the stack."
  (length (oref this :data)))

(defmethod qingeditor/cls/empty ((this qingeditor/stack))
  "If current stack if empty return `t' otherwise return `nil'."
  (eq 0 (length (oref this :data))))

(defmethod qingeditor/cls/iterate ((this qingeditor/stack) func)
  "Iterate the stack from top tp bottom, apply the function `func',
`func' receive the current item as argument."
  (dolist (item (oref this :data))
    (funcall func item))
  this)

(provide 'qingeditor-stack)
