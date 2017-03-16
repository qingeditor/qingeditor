;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; Common functions

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

(defun qingeditor/insert-newline-above ()
  "Insert a new line above point and places point in that line
with regard to indentation."
  (qingeditor/narrow-to-field
    (qingeditor/move-beginning-of-line)
    (insert "\n")
    (forward-line -1)
    (back-to-indentation)))

(defun qingeditor/insert-newline-below ()
  "Insert a new line below point and places point in that line
with regard to indentation."
  (qingeditor/move-end-of-line)
  (insert "\n")
  (back-to-indentation))

(defun qingeditor/insert-lines-above (count)
  "Insert one or several lines above the current point's line without
changing the current state and point position."
  (interactive "p")
  (dotimes (_ count)
    (save-excursion
      (qingeditor/insert-newline-above))))

(defun qingeditor/insert-lines-below (count)
  "Insert one or several lines below the current point's line without
changing the current state and point position."
  (interactive "p")
  (dotimes (_ count)
    (save-excursion
      (qingeditor/insert-newline-below))))

(defun qingeditor/goto-next-line-and-indent (&optional count)
  (interactive "p")
  (let ((counter (or count 1)))
    (while (> counter 0)
      (join-line 1)
      (newline-and-indent)
      (setq counter (1- counter)))))

;;; Undo

(defun qingeditor/start-undo-step (&optional continue)
  "Start a undo step.
All following buffer modifications are grouped together as a
single action. If `continue' is non-nil, preceding modifications
are included. The step is terminated with `qingeditor/end-undo-step'."
  (when (and (listp buffer-undo-list)
             (not qingeditor/in-single-undo))
    (if qingeditor/undo-list-pointer
        (qingeditor/refresh-undo-step)
      (unless (or continue (null (car-safe buffer-undo-list)))
        (undo-boundary))
      (setq qingeditor/undo-list-pointer (or buffer-undo-list t)))))

(defun qingeditor/end-undo-step (&optional continue)
  "End a undo step started with `qingeditor/start-undo-step'.
Adds an undo boundary unless `continue' is specified."
  (when (and qingeditor/undo-list-pointer
             (not qingeditor/in-single-undo))
    (qingeditor/refresh-undo-step)
    (unless (or continue (null (car-safe buffer-undo-list)))
      (undo-boundary))
    (setq qingeditor/undo-list-pointer nil)))

(defun qingeditor/refresh-undo-step ()
  "Refresh `buffer-undo-list' entries for current undo step.
Undo boundaries until `qingeditor/undo-list-pointer' are removed to
make the entries undoable as a single action. See
`qingeditor/start-undo-step'."
  (when qingeditor/undo-list-pointer
    (setq buffer-undo-list
          (qingeditor/filter-list #'null buffer-undo-list qingeditor/undo-list-pointer))
    (setq qingeditor/undo-list-pointer (or buffer-undo-list t))))

(defun qingeditor/undo-pop ()
  "Undo the last buffer change.
Removes the last undo information from `buffer-undo-list'.
If undo is disabled in the current buffer, use the information
in `qingeditor/temporary-undo' instead."
  (let ((paste-undo (list nil)))
    (let ((undo-list (if (eq buffer-undo-list t)
                         qingeditor/temporary-undo
                       buffer-undo-list)))
      (when (or (not undo-list) (car undo-list))
        (user-error "Can't undo previous change."))
      (while (and undo-list (null (car undo-list)))
        (pop undo-list)) ;; remove nil
      (while (and undo-list (car undo-list))
        (push (pop undo-list) paste-undo))
      (let ((buffer-undo-list (nreverse paste-undo)))
        (qingeditor/save-echo-area
         (undo)))
      (if (eq buffer-undo-list t)
          (setq qingeditor/temporary-undo nil)
        (setq buffer-undo-list undo-list)))))

(defun qingeditor/echo (string &rest args)
  "Display an unlogged message in the echo area.
  That is, the message is not logged in the *Messages* buffer.
  \(To log the message, just use `message'.)"
  (unless qingeditor/no-display
    (let (message-log-max)
      (apply #'message string args))))

(defun qing-echo (string &rest args)
  "command wrapper for `qingeditor/echo'."
  (interactive)
  (apply #'qingeditor/echo string args))

(defun qingeditor/echo-area-save ()
  "save the current echo area in `qingeditor/echo-area-message'."
  (setq qingeditor/echo-area-message (current-message)))

(defun qingeditor/echo-area-restore ()
  "Restore the echo area from `qingeditor/echo-area-message'.
Does not restore if `evil-write-echo-area' is non-nil."
  (unless qingeditor/write-echo-area
    (if qingeditor/echo-area-message
        (message "%s" qingeditor/echo-area-message)
      (message nil)))
  (setq qingeditor/echo-area-message nil
        qingeditor/write-echo-area nil))

;; window navigation

(defun qingeditor/resize-window (new-size &optional horizontal)
  "Set the current window's width or height to `new-size'.
if `horizontal' is non-nil the width of the window is changed,
otherwise its height is changed."
  (let ((count (- new-size (if horizontal (window-width) (window-height)))))
    (if (>= emacs-major-version 24)
        (enlarge-window count horizontal))
    (let ((wincfg (current-window-configuration))
          (nwins (length (window-list)))
          (inhibit-redisplay t))
      (catch 'done
        (save-window-excursion
          (while (not (zerop count))
            (if (> count 0)
                (progn
                  (enlarge-window 1 horizontal)
                  (setq count (1- count)))
              (progn
                (shrink-window 1 horizontal)
                (setq count (1+ count))))
            (if (= nwins (length (window-list)))
                (setq wincfg (current-window-configuration))
              (throw 'done t)))))
      (set-window-configuration wincfg))))

(defun qingeditor/get-buffer-tree (wintree)
  "Extracts the buffer tree from a given window tree `wintree'."
  (if (consp wintree)
      (cons (car wintree) (mapcar #'qingeditor/get-buffer-tree (cddr wintree)))
    (window-buffer wintree)))

(defun qingeditor/restore-window-tree (win tree)
  "Restore the given buffer tree layout as subwindows of `win'.
`tree' is the tree layout to be restored.
a tree layout is either a buffer or a list of form (DIR TREE ... ),
where `dir' is t for horizontal split and nil otherwise. All other
elements of the list are tre layouts itself."
  (if (bufferp tree)
      (set-window-buffer win tree)
    ;; if tree is buffer list with on buffer only, do not split
    ;; anymore
    (if (not (cddr tree))
        (qingeditor/restore-window-tree win (cadr tree))
      ;; tree is a regular list, split recursive
      (let ((newwin (split-window win nil (not (car tree)))))
        (qingeditor/restore-window-tree win (cadr tree))
        (qingeditor/restore-window-tree newwin (cons (car tree) (cddr tree)))))))

;; Types
(defun qingeditor/type (object &optional default)
  "Return the type of `object', or `default' if none."
  (let (type)
    (cond
     ((overlayp object)
      (setq type (overlay-get object :type)))
     ((qingeditor/range-p object)
      (setq type (nth 2 object)))
     ((listp object)
      (setq type (plist-get object :type)))
     ((commandp object)
      (setq type (qingeditor/get-command-property object :type)))
     ((symbolp object)
      (setq type (get object 'type))))
    (setq type (or type default))
    (and (qingeditor/type-p type) type)))

(defun qingeditor/set-type (type prop)
  "Return property `prop' for `type'."
  (cond
   ((overlayp object)
    (overlay-put object :type type))
   ((qingeditor/range-p object)
    (qingeditor/set-range-type object type))
   ((listp object)
    (plist-put object :type type))
   ((commandp object)
    (qingeditor/set-command-property object :type type))
   ((symbolp object)
    (put object 'type type)))
  object)

(defun qingeditor/type-property (type prop)
  "Return property `prop' for `type'."
  (qingeditor/get-property qingeditor/type-properties type prop))

(defun qingeditor/type-p (sym)
  "Whether `sym' is the name of a type."
  (assq sym qingeditor/type-properties))

(defun qingeditor/expand (beg end type &rest properties)
  "expand `beg' and `end' as `type' with `properties'.
Return a list (beg end type properties ... ), where the tail
may contains a property list."
  (apply #'qingeditor-transform
         ;; don't expand if already expanded
         (unless (plist-get properties :expanded) :expand)
         beg end type properties))

(defun qingeditor/contract (beg end type &rest properties)
  "Contact `beg' and `end' as `type' with `properties'.
Return a list (BEG END TYPE PROPERTIES ... ), where the tail
may contains a property list."
  (apply #'qingeditor/transform :contract beg end type properties))

(defun qingeditor/normalize (beg end type &rest properties)
  "Normalize `beg' and `end' as `type' with `properties'.
return a list (BEG END TYPE PROPERTIES ... ), where the tail
may contain a property list."
  (apply #'qingeditor/transform :normalize beg end type properties))

(defun qingeditor/transform (transform beg end type &rest properties)
  "Apply `transform' on `beg' and `end' with `properties'.
Return a list (BEG END TYPE PROPERTIES ... ), where the tail
may contain a property list. If `transform' is undefined,
return positions unchanged."
  (let* ((type (or type (qingeditor/type properties)))
         (transform (when (and type transform)
                      (qingeditor/type-property type transform))))
    (if transform
        (apply transform beg end properties)
      (apply #'qingeditor/range beg end type properties))))

(defun qingeditor/describe (beg end type &rest properties)
  "return description of `beg' and `end' with `properties'.
if no description is available, return the empty string."
  (let* ((type (or type (qingeditor/type properties)))
         (properties (plist-put properties :type type))
         (describe (qingeditor/type-property type :string)))
    (or (when describe
          (apply describe beg end properties))
        "")))

;;; Ranges

(defun qingeditor/range (beg end &optional type &rest properties)
  "Return a list (BEG END [TYPE] PROPERTIES...).
`beg' and `end' are buffer positions (numbers or markers),
`type' is a type as per `qingeditor/type-p', and `properties'
is a property list."
  (let ((beg (qingeditor/normalize-position beg))
        (end (qingeditor/normalize-position end)))
    (when (and (numberp beg) (numberp end))
      (append (list (min beg end) (max beg end))
              (when (qingeditor/type-p type)
                (list type)
                properties)))))

(defun qingeditor/range-p (object)
  "Whether `object' is a range."
  (and (listp object)
       (>= (length object) 2)
       (numberp (nth 0 object))
       (numberp (nth 1 object))))

(defun qingeditor/tange-beginning (range)
  "Return beginning of `range'."
  (when (qingeditor/range-p range)
    (let ((beg (qingeditor/normalize-position (nth 0 range)))
          (end (qingeditor/normalize-position (nth 1 range))))
      (min beg end))))

(defun qingeditor/tange-end (range)
  "Return end of `range'."
  (when (qingeditor/range-p range)
    (let ((beg (qingeditor/normalize-position (nth 0 range)))
          (end (qingeditor/normalize-position (nth 1 range))))
      (max beg end))))

(defun qingeditor/copy-range (range)
  "Return a copy of `range'."
  (copy-sequence range))

(defun qingeditor/set-range-beginning (range beg &optional copy)
  "Set `range' beginning to `beg'.
if `copy' is non-nil, return a copy of `range'."
  (when copy
    (setq range (qingeditor/copy-range range)))
  (setcar range beg)
  range)

(defun qingeditor/set-range-end (range end &optional copy)
  "Set `range' end to `end'.
if `copy' is non-nil, return a copy of `range'."
  (when copy
    (setq range (qingeditor/copy-range range)))
  (setcar (cdr range) end)
  range)

(defun qingeditor/set-range-type (range type &optional copy)
  "Set type if `range' to `type'.
if `copy' is non-nil, return a copy of range."
  (when copy
    (setq range (qingeditor/copy-range range)))
  (if type
      (setcdr (cdr range)
              (cons type (qingeditor/range-properties range)))
    (setcdr (cdr range) (qingeditor/range-properties range)))
  range)

(defun qingeditor/set-range-properties (range properties &optional copy)
  "Set properties of `range' to `properties'.
if `copy' is non-nil, return a copy of `range'."
  (when copy
    (setq range (qingeditor/copy-range range)))
  (if (qingeditor/type range)
      (setcdr (cdr (cdr range)) properties)
    (setcdr (cdr range) properties))
  range)

(defun qingeditor/range-union (range1 range2 &optional type)
  "Return the union of the ranges `range1' and `range2'.
If the ranges have conflicting types, use the type of `range1'.
This can be overridden with `type'."
  (when (and (qingeditor/range-p range1)
             (qingeditor/range-p range2))
    (qingeditor/range (min (qingeditor/range-beginning range1)
                           (qingeditor/range-beginning range2))
                      (max (qingeditpr/range-end range1)
                           (qingeditor/range-end range2))
                      (or type
                          (qingeditor/type range1)
                          (qingeditor/type range2)))))

(defun qingeditor/subrange-p (range1 range2)
  "Whether `range1' is contained within `range2'."
  (and (qingeditor/range-p range1)
       (qingeditor/range-p range2)
       (<= (qingeditor/range-beginning range2)
           (qingeditor/range-beginning range1))
       (>= (qingeditor/range-end range2)
           (qingeditor/range-end range1))))

(provide 'qingeditor-funcs-common)
