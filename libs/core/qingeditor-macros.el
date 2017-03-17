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

(defmacro qingeditor/define-module (mname &rest defs)
  "Define a module named `module-name'.

\(fn MODULE_NAME  DOC [[KEY VALUE]...])"
  (let ((doc-string (when (stringp (car-safe defs)) (pop defs)))
        (require-modules (plist-get defs :require-modules))
        (require-packages (plist-get defs :require-packages))
        (has-extra-funcs-defs (plist-get defs :has-extra-funcs-defs))
        (has-keymap-defs (plist-get defs :has-keymap-defs))
        (has-extra-config (plist-get defs :has-extra-config))
        (has-loadpath-provider (plist-get defs :has-loadpath-provider))
        (module-name (intern (format "qingeditor/%S" mname))))
    `(progn
       (unless (bound-and-true-p ,module-name)
         (defvar ,module-name t)
         (defvar ,(intern (format "%S/require-modules" module-name))
           ,require-modules)
         (defvar ,(intern (format "%S/require-packages" module-name))
           ,require-packages)
         (defvar ,(intern (format "%S/has-keymap-defs" module-name))
           ,has-keymap-defs)
         (defvar ,(intern (format "%S/has-extra-funcs-defs" module-name))
           ,has-extra-funcs-defs)
         (defvar ,(intern (format "%S/has-extra-config" module-name))
           ,has-extra-config)
         (defvar ,(intern (format "%S/has-loadpath-provider" module-name))
           ,has-loadpath-provider)))))

(defmacro qingeditor/with-undo (&rest body)
  "Execute BODY with enabled undo.
If undo is disabled in the current buffer, the undo information
is stored in `qingeditor/temporary-undo' instead of `buffer-undo-list'."
  (declare (indent defun)
           (debug t))
  `(unwind-protect
       (let (buffer-undo-list)
         (prog1
             (progn ,@body)
           (setq qingeditor/temporary-undo buffer-undo-list)
           ;; ensure qingeditor/temporary-undo starts with exactly one undo
           ;; boundary marker, i.e. nil
           (unless (null (car-safe qingeditor/temporary-undo))
             (push nil qingeditor/temporary-undo))
           ))
     (unless (eq buffer-undo-list t)
       ;; undo is enabled, so update the global buffer undo list
       (setq buffer-undo-list
             ;; prepend new undos (if there are any)
             (if (cdr qingeditor/temporary-undo)
                 (nconc qingeditor/temporary-undo buffer-undo-list)
               buffer-undo-list)
             qingeditor/temporary-undo nil))))

(defmacro qingeditor/with-single-undo (&rest body)
  "Execute `body as a single undo step."
  (declare (indent defun)
           (debug t))
  `(let (qingeditor/undo-list-pointer)
     (qingeditor/with-undo
      (unwind-protect
          (progn
            (qingeditor/start-undo-step)
            (let ((qingeditor/in-single-undo t))
              ,@body))
        (qingeditor/end-undo-step)))))

;; toggleable version of `with-temp-message'
(defmacro qingeditor/save-echo-area (&rest body)
  "Save the echo area; execute BODY; restore the echo area.
Intermittent messages are not logged in the *Messages* buffer."
  (declare (indent defun)
           (debug t))
  `(let ((inhibit-quit t)
         qingeditor/echo-area-message
         qingeditor/write-echo-area)
     (unwind-protect
         (progn
           (qingeditor/echo-area-save)
           ,@body)
       (qingeditor/echo-area-restore))))

(defmacro qingeditor/define-type (type doc &rest body)
  "Define type `type'.
`doc' is a general description and shows up in all docstrings.
It is followed by a list of keywords and functions:

:expand FUNC         Expansion function. This function should accept
                     two postions in the current buffer, `beg' and `end',
                     and return a pair of expanded buffer positions.
:contract FUNC       The opposite of :expand, optional.
:one-to-one BOOL     Whether expansion is one to one. This means that
                     :expand followed by :contract always returns the
                     original range.
:normalize FUNC      Normalization function, optional. This function should
                     accept two unexpanded positions and adjust them before
                     expansion. May be used to deal with buffer positions
                     and returns a human readable string, for example,
                     \"2 lines\".

If futher keywords and functions are specified, they are assumed to
be transformations ob buffer positions, like :expand and :contract.

\(fn TYPE DOC [[KEY FUNC] ... ])"
  (declare (indent defun)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp function-form]])))
  (let (args defun-forms func key name plist string sym val)
    ;; standard values
    (setq plist (plist-put plist :one-to-one t))
    ;; keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            val (pop body))
      (if (plist-member plist key) ; not a function
          (setq plist (plist-put plist key val))
        (setq func val
              sym (intern (replace-regexp-in-string
                           "^:" "" (symbol-name key)))
              name (intern (format "qingeditor/type/%s-%s" type sym))
              args (car (cdr-safe func))
              string (car (cdr (cdr-safe func)))
              string (if (stringp string)
                         (format "%s\n\n" string) "")
              plist (plist-put plist key `',name))
        (add-to-list
         'defun-forms
         (cond
          ((eq key :string)
           `(defun ,name (beg end &rest properties)
              ,(format "Return size of %s from `beg' to `end' \
with `properties'.\n\n%s%s" type string doc)
              (let ((beg (qingeditor/normalize-position beg))
                    (end (qingeditor/normalize-position end))
                    (type ',type)
                    plist rang)
                (when (and beg end)
                  (save-excursion
                    (qingeditor/sort beg end)
                    (unless (plist-get properties :expand)
                      (setq range (apply #'qingeditor-expand
                                         beg end type properties)
                            beg (qingeditor/range-beginning range)
                            end (qingeditor/range-end range)
                            type (qingeditor/type range type)
                            plist (qingeditor/range-properties range))
                      (setq properties
                            (qingeditor/concat-plist properties plist)))
                    (or (apply #',func beg end
                               (when ,(> (length args) 2)
                                 properties))
                        ""))))))
          (t
           `(defun ,name (beg end &rest properties)
              ,(format "Perform %s transformation on %s from `beg' to `end'\
with `properties'.\n\n%s%s" sym type string doc)
              (let ((beg (qingeditor/normalize-position beg))
                    (end (qingeditor/normalize-position end))
                    (type ',type)
                    plist range)
                (when (and beg end)
                  (save-excursion
                    (qingeditor/sort beg end)
                    (when (memq ,key '(:expand :contract))
                      (setq properties
                            (plist-put properties
                                       :expanded
                                       ,(eq key :expand))))
                    (setq range (or (apply #',func beg end
                                           (when ,(> (length args) 2)
                                             properties))
                                    (apply #'qingeditor/range
                                           beg end type properties))
                          beg (qingeditor/range-beginning range)
                          end (qingeditor/range-end range)
                          type (qingeditor/type range type)
                          plist (qingeditor/range-properties range))
                    (setq properties
                          (qingeditor/concat-plists properties plist))
                    (apply #'qingeditor/range beg end type properties)))))))
         t)))
    ;; one-to-one requires both or neither of :expand and :contract
    (when (plist-get plist :expand)
      (setq plist (plist-put plist :one-to-one
                             (and (plist-get plist :contract)
                                  (plist-get plist :one-to-one)))))
    `(progn
       (qingeditor/put-property 'qingeditor/type-properties ',type ,@plist)
       ,@defun-forms
       ',type)))

(defmacro qingeditor/use-package-add-hook (name &rest plist)
  "Add post hooks to `:list' or `:config' arguments of an existing
configuration.

In order to use this macro the variable `use-package-inject-hooks'
must be non-nil.

This is useful in the config file to override the default configuration
of a package.

Usage:

   (qingeditor/use-package-add-hook package-name
       [:keyword [option]] ... )

:pre-init       Code to run before the default `:init' configuration.
:post-init      Code to run after the default `:init' configuration.
:pre-config     Code to run before the default `:config' configuration.
:post-config    Code to run after the default `:config' configuration.

In practice the most useful hook is the `:post-config' where you can override
lazy-loaded settings."
  (declare (indent 1))
  (let ((name-symbol (if (stringp name) (intern name) name))
        (expanded-forms '()))
    (dolist (keyword qingeditor/use-package-add-hook-keywords)
      (let ((body (qingeditor/mplist-get plist keyword)))
        (when body
          (let ((hook (intern (format "use-package--%S--%s-hook"
                                      name-symbol
                                      (substring (format "%s" keyword) 1)))))
            (push `(add-hook ',hook (lambda nil ,@body)) expanded-forms)))))
    `(progn ,@expanded-forms)))

(provide 'qingeditor-macros)
