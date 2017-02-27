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
        (setq result
              (plist-put result (pop sequence) (pop sequence)))))))

(defun qingeditor/concat-keymap-alists (&rest sequences)
  "Concatenate keymap association lists, removing duplicates.
A keymap alist is a list of cons cells (VAR . MAP) where each keymap
may ocurr only once, but where the variables may be repeated
\(e.g., (VAR . MAP1) (VAR . MAP2) is allowed). The order matters,
with the highest priority keymaps being listed first."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (unless (rassq (cdr-safe elt) result)
          (push elt result))))
    (nreverse result)))

(defun qingeditor/plist-delete (prop plist)
  "Delete by side effect the property `prop' from `plist'.
If `prop' is the first property in `plist', there is no way
to remove it by side-effect; therefore, write
\(setq foo (qingeditor/plist-delete :prop foo)) to be sure of
changing the value of `foo'."
  (let ((tail plist)
        elt head)
    (while tail
      (setq elt (car tail))
      (cond
       ((eq elt prop)
        (setq tail (cdr (cdr tail)))
        (if head
            (setcdr (cdr head) tail)
          (setq plist tail)))
       (t
        (setq head tail
              tail (cdr (cdr tail))))))
    plist))

(defun qingeditor/get-property (alist key &optional prop)
  "Return property `prop' for `key' in `alist'.
`alist' is an association list with entries of the form
\(KEY . PLIST), where `plist' is a property list.
if `prop' is nil, return all properties for `key'.
if `key' is `t', return an assocation list of keys and
their `prop' values."
  (cond
   ((null prop)
    (cdr (assq key alist)))
   ((eq key t)
    (let (result
          val)
      (dolist (entry alist result)
        (setq key (car entry)
              val (cdr entry))
        (when (plist-member val prop)
          (setq val (plist-get val prop))
          (push (cons key val) result)))))
   (t
    (plist-get (cdr (assq key alist)) prop))))

(defun qingeditor/put-property (alist key prop val &rest properties)
  "Set `prop' to `val' for `key' in `alist'.
`alist' points to an association list with entries of the form
\(KEY . PLIST), where `plist' is a property list storing `prop' and `val'."
  (set alist
       (let* ((alist-ref (symbol-value alist))
              (plist-ref (cdr (assq key alist-ref))))
         (setq plist-ref (plist-put plist-ref prop val))
         (when properties
           (setq plist-ref (qingeditor/concat-plist plist-ref properties))
           (setq val (car (last properties))))
         (setq alist-ref (assq-delete-all key plist-ref))
         (push (cons key plist-ref) alist-ref)))
  val)

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

;; Command properties functions
;; If no properties are defined for the command, serval parts of
;; qingeditor apply certain default rules; e.g., the repeat system decides
;; wether the command is repeatable by monitoring buffer changes.
(defun qingeditor/has-command-property-p (command property)
  "Wether `command' has qingeditor `property'.
See also `qingeditor/has-command-properties-p'."
  (plist-member (qingeditor/get-command-properties command) property))

(defun qingeditor/has-command-properties-p (command)
  "Wether qingeditor properties are defined for `command'.
see also `qingeditor/has-command-property-p'."
  (and (qingeditor/get-command-properties command) t))

(defun qingeditor/get-command-property (command property &optional default)
  "Return the value of `qingeditor' of `command'.
If the command does not have the property, return `default'."
  (if (qingeditor/has-command-property-p command property)
      (qingeditor/get-property qingeditor/command-properties command property)
    default))

(defun qingeditor/get-command-properties (comand)
  "Return all `qingeditor' properties of `command'.
See also `qingeditor/get-command-property'."
  (qingeditor/get-property qingeditor/command-properties command))

(defun qingeditor/set-command-property (command property value)
  "Set `property' to `value' for `command'.
To set multiple properties at once, see
`qingeditor/set-command-properties' and `qingeditor/add-command-properties'."
  (qingeditor/put-property 'qingeditor-command-properties command property value))

(defalias 'qingeditor/put-command-property 'qingeditor/set-command-property)

(defun qingeditor/add-command-properties (command &rest properties)
  "Add `properties' to `command'.
`properties' should be a property list.
To replace all properties at once, use `qingeditor/set-command-properties'."
  (apply #'qingeditor/put-property
         'qingeditor/command-properties command properties))

(defun qingeditor/set-command-properties (command &rest properties)
  "Replace all of `command' properties with `properties'.
`properties' should be a property list.
This erases all previous properties; to only add properties, use
`qingeditor/set-command-property'."
  (setq qingeditor/command-properties
        (assq-delete-all command qingeditor/command-properties))
  (when properties
    (apply #'qingeditor/add-command-properties command properties))
  )

(defun qingeditor/remove-command-properties (command &rest properties)
  "Remove `properties' from `command'.
`properties' should be a list of properties (:prop1 :prop2 ...).
if `properties' is the empty list, all properties are removed."
  (let (plist)
    (when properties
      (setq plist (qingeditor/get-command-properties command))
      (dolist (property properties)
        (setq plist (qingeditor/delete-plist-delete property plist))))
    (apply #'qingeditor/set-command-properties command plist)))

(defun qingeditor/move-beginning-of-line (&optional arg)
  "Move to the beginning of the line as displayed.
Like `move-beginning-of-line', but retains the goal column."
  (qingeditor/save-goal-column
   (move-beginning-of-line arg)
   (beginning-of-line)))

(defun qingeditor/move-end-of-line (&optional arg)
  "Move to the end of the line as displayed.
Like `move-end-of-line', but retains the goal column."
  (qingeditor/save-goal-column
   (move-end-of-line arg)
   (end-of-line)))

;; The purpose of this function is the provide line motions which
;; preserve the column. This is how `previous-line' and `next-line'
;; work, but unfortunately the behaviour is hard-coded: if and only if
;; the last command was `previous-line' or `next-line', the column is
;; preserved. Furthermore, in contrast to Vim, when we cannot go
;; further, those motions move point to the beginning resp. the end of
;; the line (we never want point to leave its column). The code here
;; comes from simple.el, and I hope it will work in future.
(defun qingeditor/line-move (count &optional noerror)
  "A wrapper for line motions which conserves the column.
Signal an error at buffer boundaries unless `noerror' is non-nil."
  (cond
   (noerror
    (condition-case nil
        (qingeditor/line-move count)
      (error nil)))
   (t
    (qingeditor/signal-without-movement
     (setq this-command (if (>= count 0)
                            #'next-line
                          #'previous-line))
     (let ((opoint (point)))
       (condition-case err
           (with-no-warnings
             (funcall this-command (abs count)))
         ((beginning-of-buffer end-of-buffer)
          (let ((col (or goal-column
                         (if (consp temporary-goal-column)
                             (car temporary-goal-column)
                           temporary-goal-column))))
            (if line-move-visual
                (vertical-motion (cons col 0))
              (line-move-finish col opoint (< count 0)))
            ;; maybe we should just `ding'.
            (signal (car err) (cdr err))))))))))

(defun qingeditor/adjust-cursor ()
  "Move point on character back if at the end if a non-empty line.
This behavior is contingent on the variable `qingeditor/move-cursor-back'."
  (when (and (eolp)
             (not qingeditor/move-beyond-eol)
             (not (bolp))
             (= (point)
                (save-excursion
                  (qingeditor/move-end-of-line)
                  (point))))
    (qingeditor/move-cursor-back)))

(defun qingeditor/move-cursor-back ()
  "Move point on character back within the current line.
 Honors field boundaries, i.e., constrains the movement
to the current field as recognize by `line-beginning-position'."
  (unless (or (= (point) (line-beginning-position))
              (and (boundp 'visual-line-mode)
                   visual-line-mode
                   (= (point) (save-excursion
                                (beginning-of-visual-line)
                                (point)))))
    (backward-char)))

;; Interactive forms
(defun qingeditor/match-interactive-code (interactive &optional pos)
  "Match an interactive code at position `pos' in string `interactive'.
Returns the first matching entry in `qingeditor/interactive-alist', or nil."
  (let ((length (length interactive))
        (pos (or pos 0)))
    (catch 'done
      (dolist (entry qingeditor/interactive-alist)
        (let* ((string (car entry))
               (end (+ (length string) pos)))
          (when (and (<= end length)
                     (string= string (substring interactive pos end)))
            (throw 'done entry)))))))

(defun qingeditor/interactive-string (string)
  "Evaluate the interactive string `string'.
The string may contain extended interactive syntax.
The return value is a cons cell (FROM . PROPERTIES),
where `form' is a single list-expression to be passed to a
standard `interactive' statement, and `properties' is a
list of command properties as passed to `qingeditor/define-command'."
  (let ((length (length string))
        (pos 0)
        code expr forms match plist prompt properties)
    (while (< pos length)
      (if (eq (aref string pos) ?\n)
          (setq pos (1+ pos))
        (setq match (qingeditor/match-interactive-code string pos))
        (if (null match)
            (user-error "Unkonow interactive code: `%s'"
                        (substring string pos))
          (setq code (car match)
                expr (car (cdr match))
                plist (cdr (cdr match))
                pos (+ pos (length code)))
          (when (functionp expr)
            (setq prompt
                  (substring string pos
                             (or (string-match "\n" string pos)
                                 length))
                  pos (+ pos (length prompt))
                  expr `(funcall ,expr ,prompt)))
          (setq forms (append forms (list expr))
                properties (append properties plist)))))
    (cons `(append ,@forms) properties)))

(defun qingeditor/interactive-form (&rest args)
  "Evaluate interactive forms ARGS.
The return value is a cons cell (FORM . PROPERTIES),
where FORM is a single list-expression to be passed to
a standard `interactive' statement, and PROPERTIES is a
list of command properties as passed to `qingeditor/define-command'."
  (let (forms properties)
    (dolist (arg args)
      (if (not (stringp arg))
          (setq forms (append forms (list arg)))
        (setq arg (qingeditor/interactive-string arg)
              forms (append forms (cdr (car arg)))
              properties (append properties (cdr arg)))))
    (cons (apply #'qingeditor/concatenate-interactive-forms forms)
          properties)))

(defun qingeditor/concatenate-interactive-forms (&rest forms)
  "Concatenate interactive list expressions `forms'.
Returns a single expression where successive expressions
are joined, if possible."
  (let (result)
    (when forms
      (while (cdr forms)
        (cond
         ((null (car forms))
          (pop forms))
         ((and (eq (car (car forms)) 'list)
               (eq (car (cadr forms)) 'list))
          (setq forms (cons (append (car forms)
                                    (cdr (cadr forms)))
                            (cdr (cdr forms)))))
         (t
          (push (pop forms) result))))
      (when (car forms)
        (push (pop forms) result))
      (setq result (nreverse result))
      (cond
       ((null result))
       ((null (cdr result))
        (car result))
       (t
        `(append ,@result))))))

(provide 'qingeditor-funcs-common)
