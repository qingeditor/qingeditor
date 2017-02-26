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

(provide 'qingeditor-macros)
