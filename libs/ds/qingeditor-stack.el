;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; Commentary:
;;
;; 定义一个简单的栈类

(defclass qingeditor/stack ()
  ((data
   :initarg :data
   :initform nil
   :type list
   :documetation "底层数据储存，`list'对象。"))
  :documentation "对`list'简单封装，提供一个一致的栈操作接口。")

(defmethod qingeditor/cls/push ((this qingeditor/stack) data)
  "将一个数据入栈。"
  (push data (oref this :data))
  this)

(defmethod qingeditor/cls/pop ((this qingeditor/stack))
  "将数据出栈，返回栈顶的数据。"
  (pop (oref this :data)))

(defmethod qingeditor/cls/bottom ((this qingeditor/stack))
  "获取栈底的数据。"
  (car (last (oref this :data))))

(defmethod qingeditor/cls/top ((this qingeditor/stack))
  "获取栈顶的数据。"
  (car (oref this :data)))

(defmethod qingeditor/cls/clear ((this qingeditor/stack))
  "清空数据。"
  (oset this :data nil))

(defmethod qingeditor/cls/count ((this qingeditor/stack))
  "当前栈中的数据。"
  (length (oref this :data)))

(defmethod qingeditor/cls/empty ((this qingeditor/stack))
  "当前的栈是否为空。"
  (eq 0 (length (oref this :data))))

(defmethod qingeditor/cls/iterate ((this qingeditor/stack) func)
  "从栈顶到栈底依次调用`func'函数

`func'传入当前元素作为参数。"
  (dolist (item (oref this :data))
    (funcall func item))
  this)

(provide 'qingeditor-stack)
