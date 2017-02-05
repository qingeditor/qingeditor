;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; Commentary:
;;
;; 定义`qingeditor/fast-priority-queue'类
;;
(require 'qingeditor-hash-table)

(defconst qingeditor/fast-priority-queue/extra-data 1)
(defconst qingeditor/fast-priority-queue/extra-priority 2)
(defconst qingeditor/fast-priority-queue/extra-both 3)

(defclass qingeditor/fast-priority-queue ()
  ((extra-flag
    :initarg :extract-flag
    :initform (eval qingeditor/fast-priority-queue/extra-data)
    :type number)

   (values
    :initarg :values
    :initform (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "队列的值,根据优先级分开。")

   (priorities
    :initarg :priorities
    :initform (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "队列优先级数据。")

   (sub-priorites
    :initarg :sub-priorities
    :initform (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "迭代周期的优先级。")

   (max-priority
    :initarg :max-priority
    :initform 0
    :type number
    :documentation "最大优先级")

   (count
    :initarg :count
    :initform 0
    :type number
    :documentation "队列元素的个数。")

   (index
    :initarg :index
    :initform 0
    :type number
    :documentation "队列当前元素的索引值。")

   (sub-index
    :initarg :sub-index
    :initform 0
    :type number
    :documentation "迭代周期中的元素的索引值。")))

(defmethod qingeditor/cls/insert
  ((this qingeditor/fast-priority-queue) value priority)
  "插入一个元素到队列里面,并指定优先级。"
  (when (not (qingeditor/cls/has-key (oref this :values) priority))
    (qingeditor/cls/set (oref this :values) priority nil))
  (let ((cur-value-list (qingeditor/cls/get (oref this :values) priority)))
    (push value cur-value-list)
    (qingeditor/cls/set (oref this :values) priority cur-value-list))
  (when (not (qingeditor/cls/has-key (oref this :priorities) priority))
    (qingeditor/cls/set (oref this :priorities) priority priority)
    (oset this :max-priority (max priority (oref this :max-priority))))
  (oset this :count (1+ (oref this :count))))

(defmethod qingeditor/cls/count ((this qingeditor/fast-priority-queue))
  "获取队列的元素。"
  (oref this :count))

(defmethod qingeditor/cls/to-list ((this qingeditor/fast-priority-queue) &optional func)
  "将优先级队列转换成`list', 如果指定了`func'参数
则会传入当前的数据和优先级进行调用 \(fucn data index sub-index)。"
  (qingeditor/cls/prepare-iterate this)
  (let ((ret-list))
    (while (qingeditor/cls/has-key
	    (oref this :values) (oref this :max-priority))
      (let ((cur-priority-values
	     (reverse
	      (qingeditor/cls/get (oref this :values) (oref this :max-priority)))))
	(dolist (cur-value cur-priority-values)
	  (let ((flag (oref this :extract-flag))
		data)
	    (cond 
	      ((eq flag qingeditor/fast-priority-queue/extra-data)
	       (setq data cur-value))
	      ((eq flag qingeditor/fast-priority-queue/extra-priority)
	       (setq data (oref this :max-priority)))
	      ((eq flag qingeditor/fast-priority-queue/extra-both)
	       (setq data (cons cur-value (oref this :max-priority)))))
	    (push data ret-list)
	    (when (fboundp func)
	      (funcall func data (oref this :index) (oref this :sub-index)))
	    (oset this :index (1+ (oref this :index)))
	    (oset this :sub-index (1+ (oref this :sub-index)))))
	;; 本轮完成，设置相关标记变量
	(qingeditor/cls/remove (oref this :sub-priorities) (oref this :max-priority))
	(if (qingeditor/cls/empty (oref this :sub-priorities))
	    (oset this :max-priority 0)
	  (let ((max-priority))
	    (qingeditor/cls/iterate-items
	     (oref this :sub-priorities)
	     (progn
	       (when (eq max-priority nil)
		 (setq max-priority value))
	       (if (> value max-priority)
		   (setq max-priority value))))
	    (oset this :max-priority max-priority)))
	(oset this :sub-index -1)))
    (nreverse ret-list)))

(defmethod qingeditor/cls/set-extract-flags
  ((this qingeditor/fast-priority-queue) flag)
  "设置数据获取的方式。"
  (when (or (eq qingeditor/fast-priority-queue/extra-data flag)
	    (eq qingeditor/fast-priority-queue/extra-priority flag)
	    (eq qingeditor/fast-priority-queue/extra-both flag))
    (oset this extra-flag flag))
  this)

(defmethod qingeditor/cls/remove ((this qingeditor/fast-priority-queue) datum)
  "删除指定的`datum'数据。"
  (qingeditor/cls/prepare-iterate this)
  (catch 'remove-success
    (while (qingeditor/cls/has-key
	    (oref this :values) (oref this :max-priority))
      (let ((cur-priority-values
	     (qingeditor/cls/get (oref this :values) (oref this :max-priority))))
	(dolist (cur-value cur-priority-values)
	  (when (equalp cur-value datum)
	    (setq cur-priority-values (delete cur-value cur-priority-values))
	    (qingeditor/cls/set
	     (oref this :values) (oref this :max-priority) cur-priority-values)
	    (oset this :count (1- (oref this :count)))
	    (throw 'remove-success t))
	  (oset this :index (1+ (oref this :index)))
	  (oset this :sub-index (1+ (oref this :sub-index))))
	;; 本轮完成，设置相关标记变量
	(qingeditor/cls/remove (oref this :sub-priorities) (oref this :max-priority))
	(if (qingeditor/cls/empty (oref this :sub-priorities))
	    (oset this :max-priority 0)
	  (let ((max-priority))
	    (qingeditor/cls/iterate-items
	     (oref this :sub-priorities)
	     (progn
	       (when (eq max-priority nil)
		 (setq max-priority value))
	       (if (> value max-priority)
		   (setq max-priority value))))
	    (oset this :max-priority max-priority)))
	(oset this :sub-index 0)))))

(defmethod qingeditor/cls/empty ((this qingeditor/fast-priority-queue))
  "判断当前的队列是否为空。"
  (qingeditor/cls/empty (oref this :values)))

(defmethod qingeditor/cls/contains ((this qingeditor/fast-priority-queue) datum)
  "当前队列是否含有`datum'。"
  (catch 'datum-find
    (qingeditor/cls/iterate-items
     (oref this :values)
     (progn
       (when (equalp datum value)
	 (throw 'datum-find t))))))

(defmethod qingeditor/cls/has-priority
  ((this qingeditor/fast-priority-queue) priority)
  "队列是否存在指定的`priority'。"
  (qingeditor/cls/has-key (oref this :values) priority))

(defmethod qingeditor/cls/get-priority-list
  ((this qingeditor/fast-priority-queue) priority)
  "获取指定优先级的数据列表，返回当前的优先级数据的克隆。"
  (when (qingeditor/cls/has-key (oref this :values) priority)
    (copy-sequence (qingeditor/cls/get (oref this :values) priority))))

(defmethod qingeditor/cls/prepare-iterate ((this qingeditor/fast-priority-queue))
  "准备迭代，设置相关变量。"
  (oset this :sub-priorities (qingeditor/cls/clone (oref this :priorities)))
  (if (qingeditor/cls/empty (oref this :priorities))
      (oset this :max-priority 0)
    (let ((max-priority))
      (qingeditor/cls/iterate-items
       (oref this :priorities)
       (progn
	 (when (eq max-priority nil)
	   (setq max-priority value))
	 (if (> value max-priority)
	     (setq max-priority value))))
      (oset this :max-priority max-priority)))
  (oset this :index 0)
  (oset this :sub-index 0))

(provide 'qingeditor-fast-priority-queue)
