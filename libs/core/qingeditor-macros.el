;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define some untils macros

(defmacro qingeditor/call-func (func &optional msg)
  "Call the function from the configuration only if it is bound.
If `msg' is not nil tehn display a message in `*Message'. Errors
are caught and signalled to user in `qingeditor' buffer."
  `(progn
     (when ,msg (qingeditor/startup-buffer/message ,msg))
     (when (fboundp ',func)
       (condition-case-unless-debug err
           (,func)
         (error
          (qingeditor/cls/increase-error-count
           qingeditor/initializer-ref)
          (qingeditor/startup-buffer/append
           (format "Error in %s: %s\n"
                   ',(symbol-name func)
                   (error-message-string err))
           t))))))

(defmacro qingeditor/do-after-display-system-ready (&rest body)
  "If the display system is initialized, run `BODY', otherwise
we attach a listener to `qingeditor/display-system-ready-event' event."
  `(let ((init
          (cond
           ((boundp 'ns-initialized) ns-initialized)
           ;; w32-initialized gets set too early, so
           ;; if we're on Windows, check the list of fonts
           ;; instead (this is nil until the graphics system
           ;; is initialized)
           ((boundp 'w32-initialized) (font-family-list))
           ;; fallback to normal loading behavior only if in a GUI
           (t (display-graphic-p)))))
     (if init
         (progn
           ,@body)
       (push (qingeditor/cls/attach
              qingeditor/geventmgr
              qingeditor/display-system-ready-event
              (qingeditor/eventmgr/event-handler/init (lambda ()
                                                        ,@body)))
             qingeditor/global-listeners-pool))))

(defmacro qingeditor/symbol-value (symbol)
  "Return the value of SYMBOL corresponding to a dotspacemacs variable.
If SYMBOL value is `display-graphic-p' then return the result of
 `(display-graphic-p)', otherwise return the value of the symbol."
  `(if (eq 'display-graphic-p ,symbol) (display-graphic-p) ,symbol))

(defmacro qingeditor/narrow-to-field (&rest body)
  "Narrow to the current field."
  (declare (indent defun)
           (debug t))
  `(qingeditor/with-restriction (field-beginning) (field-end)
                                ,@body))

(defmacro qingeditor/with-restriction (beg end &rest body)
  "Execute `body' with buffer narrowed to `beg' and `end'.
`beg' or `end' may be `nil' as passed to `qingeditor/narrow'; this creates
a one-sided restriction."
  (declare (indent 2)
           (debug t))
  `(save-restriction
     (let ((qingeditor/restriction-stack
            (cons (cons (point-min) (point-max)) qingeditor/restriction-stack)))
       (qingeditor/narrow ,beg ,end)
       ,@body)))

(defmacro qingeditor/save-goal-column (&rest body)
  "Restores the goal column after execution of `body'.
See also `qingeditor/save-column'."
  (declare (indent defun)
           (debug t))
  `(let ((goal-column goal-column)
         (temporary-goal-column temporary-goal-column))
     ,@body))

(defmacro qingeditor/save-column (&rest body)
  "Restores the column after execution of `body'.
See also `qingeditor/save-goal-column'."
  (declare (indent defun)
           (debug t))
  `(let ((col (current-column)))
     (qingeditor/save-goal-column
      ,@body
      (move-to-column col))))

(defmacro qingeditor/define-command (command &rest body)
  "Define a command `command'.

\(fn COMMAND (ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (debug (&define name
                           [&optional lambda-list]
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let ((interactive '(interactive))
        arg args doc doc-form key keys)
    ;; collect arguments
    (when (listp (car-safe body))
      (setq args (pop body)))
    ;; collect docstring
    (when (> (length body) 1)
      (if (eq (car-safe (car-safe body)) 'format)
          (setq doc-form (pop body))
        (when (stringp (car-safe body))
          (setq doc (pop body)))))
    ;; collect keywords
    (setq keys (plist-put keys :repeat t))
    (while (keywordp (car-safe body))
      (setq key (pop body))
      (setq arg (pop body))
      (unless nil ;; TODO add keyword check
        (setq keys (plist-put keys key arg))))
    ;; collect `interactive' form
    (when (and body (consp (car body))
               (eq (car (car body)) 'interactive))
      (let* ((iform (pop body))
             (result (apply #'qingeditor/interactive-form (cdr iform)))
             (form (car result))
             (attrs (cdr result)))
        (setq interactive `(interactive ,form))
        (setq keys (qingeditor/concat-plist keys attrs))))
    `(progn
       ;; the compiler does not recognize `defun' inside `let'
       ,(when (and command body)
          `(defun ,command ,args
             ,@(when doc `(,doc))
             ,interactive
             ,@body))
       ,(when (and command doc-form)
          `(put ',command 'function-documentation ,doc-form))
       ;; set command properties for symbol or lambda function
       (let ((func ',(if (and (null command) body)
                         `(lambda ,args
                            ,interactive
                            ,@body)
                       command)))
         (apply #'qingeditor/set-command-properties func ',keys)
         func))))

(defmacro qingeditor/called-interactively-p ()
  "Wrapper for `called-interactiovely-p'.
In order versions of Emacs, `called-interactively-p' takes no arguments.
In Emacs 23.2 and newer, it takes one arguments."
  (if (version< emacs-version "23.2")
      '(called-interactively-p)
    '(called-interactively-p 'any)))

(defmacro qingeditor/define-motion (motion args &rest body)
  "Define an motion command `motion'.

\(fn MOTION (COUNT ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let (arg doc interactive key keys type)
    (when args
      (setq args `(&optional ,@(delq '&optional args))
            ;; the count is either numeric or nil
            interactive '("<c>")))
    ;; collect docstring
    (when (and (> (length body) 1)
               (or (eq (car-safe (car-safe body)) 'format)
                   (stringp (car-safe body))))
      (setq doc (pop body)))
    ;; collect keywords
    (setq keys (plist-put keys :repeat 'motion))
    (while (keywordp (car-safe body))
      (setq key (pop body))
      (setq arg (pop body))
      (setq keys (plist-put keys key arg)))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr (pop body))))
    ;; macro expansion
    `(progn
       ;; refresh echo area in Eldpc mode
       (when ',motion
         (eval-after-load 'eldoc
           '(and (fboundp 'eldoc-add-command)
                 (eldoc-add-command ',motion))))
       (qingeditor/define-command ,motion (,@args)
         ,@(when doc `(,doc))
         ,@keys
         :keep-visual t
         (interactive ,@interactive)
         ,@body))))

(defmacro qingeditor/signal-without-movement (&rest body)
  "Catches errors provided point moves within this scope."
  (declare (indent defun)
           (debug t))
  `(let ((p (point)))
     (condition-case err
         (progn ,@body)
       (error
        (when (= p (point))
          (signal (car err) (cdr err)))))))

(defmacro qingeditor/define-local-var (symbol &optional initvalue docstring)
  "Define `symbol' as permanent buffer local variable, and return `symbol'.
The parameters are the same as for `defvar', but the variable
`symbol' is made permanent buffer local."
  (declare (indent defun)
           (debug (symbolp &optional form stringp)))
  `(progn
     (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     (put ',symbol 'permanent-local t)))

(defmacro qingeditor/define-interactive-code (code &rest body)
  "Define an interactive code.
`prompt', if given, is the remainder of the interactive string
up to the next newline. Command properities may be specified
via `key-value' paires. `body' should evaluate to a list of values.

\(fn CODE (PROMPT) [[KEY VALUE]...] BODY...)"
  (declare (indent defun))
  (let* ((args (when (and (> (length body) 1)
                          (listp (car-safe body)))
                 (pop body)))
         (doc (when (stringp (car-safe body)) (pop body)))
         func properties)
    (while (keywordp (car-safe body))
      (setq properties
            (append properties (list (pop body) (pop body)))))
    (cond
     (args
      (setq func `(lambda ,args
                    ,@(when doc `(,doc))
                    ,@body)))
     ((> (length body) 1)
      (setq func `(progn ,@body)))
     (t
      (setq func (car body))))
    `(eval-and-compile
       (let* ((code ,code)
              (entry (assoc code qingeditor/interactive-alist))
              (value (cons ',func ',properties)))
         (if entry
             (setcdr entry value)
           (push (cons code value) qingeditor/interactive-alist))
         code))))

(provide 'qingeditor-macros)
