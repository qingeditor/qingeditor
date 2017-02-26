;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; common functions

(defun qingeditor/normalize-position (pos)
  "Return `pos' if is does not exceed the buffer boundaries.
If `pos' is less than `point-min', return `point-min'.
If `pos' is more than `point-max', return `point-max'.
If `pos' is a marker, return its positions."
  (cond
   ((not (number-or-marker-p pos))
    pos)
   ((< pos (point-min))
    (point-min))
   ((> pos (point-max))
    (point-max))
   ((markerp pos)
    (marker-position pos))
   (t pos)))

(defun qingeditor/count-lines (beg end)
  "Return absolute line-number-difference betweeen `beg' and `end'.
This should give the same results no matter where on the line `beg'
and `end' are."
  (if (= beg end)
      0
    (let* ((last (max beg end))
           (end-at-bol (save-excursion (goto-char last)
                                       (bolp))))
      (if end-at-bol
          (count-lines beg end)
        (1- (count-lines beg end))))))

(defun qingeditor/narrow (beg end)
  "Restrict the buffer to `beg' and `end'.
`beg' or `end' may be nil, specifying a one-sided restriction including
`point-min' or `point-max'. See also `qingeditor/with-restriction'."
  (setq beg (or (qingeditor/normalize-position beg) (point-min)))
  (setq end (or (qinegditor/normalize-position end) (point-max)))
  (narrow-to-region beg end))

(defun qingeditor/concat-lists (&rest sequences)
  "Concatenate lists, removing duplicates.
Elements are compared with `eq'."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (add-to-list 'result elt nil #'eq)))
    (nreverse result)))

(defun qingeditor/concat-alists (&rest sequences)
  "Concatenate association lists, removing duplicates.
An alist is a list of cons cells (`key' . `value') where each key
may ocurr only once. Later values overwrite earlier values."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (setq result (assq-delete-all (car-safe elt) result))
        (push elt result)))
    (nreverse result)))

(defun qingeditor/concat-plist (&rest sequences)
  "Concatenate property lists, removing duplicates.
A property list is a list (:KEYWORD1 VALUE1 :KEYWORD2 VALUE2...)
where each keyword may ocur only once. Later values overwrite
earlier values."
  (let (result)
    (dolist (sequence sequences result)
      (while sequence
        (setq sequence
              (plist-put result (pop sequence) (pop sequence)))))))

(unless (fboundp 'region-active-p)
  (defun region-active-p ()
    "Returns t iff region and mark are active."
    (and transient-mark-mode mark-active)))

;; Emacs < 23 does not know `characterp'
(unless (fboundp 'characterp)
  (defalias 'characterp 'char-valid-p))

;; `make-char-table' requires this property in Emacs 22
(unless (get 'display-table 'char-table-extra-slot)
  (put 'display-table 'char-table-extra-slot 0))

(defun qingeditor/member-if (predicate list &optional pointer)
  "Find the first item satisfies `predicate' in `list'.
Stop when reaching `pointer', which should pointer at a link
in the list."
  (let (elt)
    (catch 'done
      (while (and (consp list) (not (eq list pointer)))
        (setq elt (car list))
        (if (funcall predicate elt)
            (throw 'done elt)
          (setq list (cdr list)))))))

(defun qingeditor/member-recursive-if (predicate tree)
  "Find the first item satisfying `predicate' in tree."
  (cond
   ((funcall predicate tree)
    tree)
   ((listp tree)
    (catch 'done
      (dolist (elt tree)
        (when (setq elt (qingeditor/member-recursive-if predicate elt))
          (throw 'done elt)))))))

(provide 'qingeditor-funcs-common)
