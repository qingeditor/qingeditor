;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; qingeditor (www.qingeditor.org)
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; Define `qingeditor/fast-priority-queue' class
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
    :documentation "The values of queue, group by priority.")

   (priorities
    :initarg :priorities
    :initform (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "The priority data of queue.")

   (sub-priorites
    :initarg :sub-priorities
    :initform (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "The priorities of iterate cycle.")

   (max-priority
    :initarg :max-priority
    :initform 0
    :type number
    :documentation "The max priority.")

   (count
    :initarg :count
    :initform 0
    :type number
    :documentation "The count of current priority queue.")

   (index
    :initarg :index
    :initform 0
    :type number
    :documentation "The index of current item.")

   (sub-index
    :initarg :sub-index
    :initform 0
    :type number
    :documentation "The index of current item in current iterate cycle.")))

(defmethod qingeditor/cls/insert
  ((this qingeditor/fast-priority-queue) value priority)
  "Insert a item into queue and specify the priority."
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
  "Get the count of queue."
  (oref this :count))

(defmethod qingeditor/cls/to-list ((this qingeditor/fast-priority-queue) &optional func)
  "Convert priority queue into `list', if `func' is `non-nil' and invokable
It will receive `data', `index' and `sub-index' arguments like below:
\(fucn data index sub-index)。"
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
        ;; current cycle finished, set the mark variables.
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
  "Set data retrieve type flag."
  (when (or (eq qingeditor/fast-priority-queue/extra-data flag)
            (eq qingeditor/fast-priority-queue/extra-priority flag)
            (eq qingeditor/fast-priority-queue/extra-both flag))
    (oset this extra-flag flag))
  this)

(defmethod qingeditor/cls/remove ((this qingeditor/fast-priority-queue) datum)
  "Delete the item that equal datum."
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
        ;; Current cycle finished, set mark variable.
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
  "If current queue is empty return `t' otherwise return `nil'."
  (qingeditor/cls/empty (oref this :values)))

(defmethod qingeditor/cls/contains ((this qingeditor/fast-priority-queue) datum)
  "If current priority queue contains `datum' return `t' otherwise return `nil'."
  (catch 'datum-find
    (qingeditor/cls/iterate-items
     (oref this :values)
     (progn
       (when (equalp datum value)
         (throw 'datum-find t))))))

(defmethod qingeditor/cls/has-priority
  ((this qingeditor/fast-priority-queue) priority)
  "If current priority queue contains `priority' return `t'
otherwise return `nil'."
  (qingeditor/cls/has-key (oref this :values) priority))

(defmethod qingeditor/cls/get-priority-list
  ((this qingeditor/fast-priority-queue) priority)
  "Get the `datum' list where the item in this list has the priority
equal `priority'."
  (when (qingeditor/cls/has-key (oref this :values) priority)
    (copy-sequence (qingeditor/cls/get (oref this :values) priority))))

(defmethod qingeditor/cls/prepare-iterate ((this qingeditor/fast-priority-queue))
  "Prepare the iterate context."
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
