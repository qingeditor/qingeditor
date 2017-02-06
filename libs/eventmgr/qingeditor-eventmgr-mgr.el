;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; Define The event manager class

(require 'qingeditor-eventmgr-shared-mgr)
(require 'qingeditor-eventmgr-event)
(require 'qingeditor-eventmgr-response-collection)

(defclass qingeditor/eventmgr/mgr ()
  ((events
    :initarg :events
    :initform (qingeditor/hash-table/init)
    :type qingeditor/hash-table
    :documentation "The event type hash table, save all specified event listeners.")

   (identifiers
    :initarg :identifiers
    :initform nil
    :type list
    :reader qingeditor/cls/get-identifiers
    :documentation "This property store identifiers, identifiers used when current mgr has a
shared event manager, event manager use identifier in this list to get listeners from
shared event manager.")

   (shared-mgr
    :initarg :shared-mgr
    :initform nil
    :type (satisfies (lambda (mgr)
                       (or (null mgr) (object-of-class-p mgr qingeditor/eventmgr/shared-mgr))))
    :reader qingeditor/cls/get-shared-mgr
    :writer qingeditor/cls/set-shared-mgr
    :documentation "The shared event manager object."))
  :documentation "The event manager class
This class can provide event dispatch ability to the object that use it.")

(defun qingeditor/eventmgr/mgr/init (&optional shared-mgr identifiers)
  "create a new event manager object."
  (let ((mgr (make-instance 'qingeditor/eventmgr/mgr)))
    (when (and (not (stringp identifiers))
               (not (listp identifiers)))
      (error "Identifier names passed to `%s' must be a string or list, but %s given."
             "qingeditor/eventmgr/init" (type-of identifiers)))
    (when shared-mgr
      (oset mgr :shared-mgr shared-mgr)
      (qingeditor/cls/set-identifiers mgr identifiers))
    mgr))

(defmethod qingeditor/cls/attach
  ((this qingeditor/eventmgr/mgr) event-name listener &optional priority)
  "Add a event listener for event `event-name'."
  (when (null priority)
    (setq priority 1))
  (when (or (not (stringp event-name))
            (eq (length event-name) 0))
    (error "event name passed to `%s' must be a string and can be empty, but %s given."
           "qingeditor/eventmgr/mgr/attach" (type-of event-name)))
  (let (listener-table
        listener-list)
    (if (qingeditor/cls/has-key (oref this :events) event-name)
        (setq listener-table (qingeditor/cls/get (oref this :events) event-name))
      (setq listener-table (qingeditor/hash-table/init))
      (qingeditor/cls/set (oref this :events) event-name listener-table))
    (when (qingeditor/cls/has-key listener-table priority)
      (setq listener-list (qingeditor/cls/get listener-table priority)))
    (push listener listener-list)
    (qingeditor/cls/set listener-table priority listener-list)
    listener))

(defmethod qingeditor/cls/detach
  ((this qingeditor/eventmgr/mgr) listener &optional event-name force)
  "Delete a listener of event `event-name'."
  (catch 'qingeditor-eventmgr-mgr-detach
    (let (listener-table)
      (when (or (not event-name)
                (and (stringp event-name) (string= event-name "*") (not force)))
        (qingeditor/cls/iterate-items
         (oref this :events)
         (qingeditor/cls/detach this listener key t))
        (throw 'qingeditor-eventmgr-shared-mgr-detach t))
      (when (or (not (stringp event-name))
                (eq (length event-name) 0))
        (error "Invalid event name provided; must be a string, received `%s'" (type-of event-name)))
      (unless (qingeditor/cls/has-key (oref this :events) event-name)
        (throw 'qingeditor-eventmgr-mgr-detach nil))
      (setq listener-table (qingeditor/cls/get (oref this :events) event-name))
      (qingeditor/cls/iterate-items
       listener-table
       (progn
         (setq listener-list value)
         (dolist (evaluated-listener listener-list)
           (when (eq evaluated-listener listener)
             ;; found the target event listener
             (setq listener-list (delete evaluated-listener listener-list))))
         (if (eq (length listener-list) 0)
             (qingeditor/cls/remove listener-table key)
           (qingeditor/cls/set listener-table key listener-list))))
      (when (qingeditor/cls/empty listener-table)
        (qingeditor/cls/remove (oref this :events) event-name)))))

(defmethod qingeditor/cls/set-identifiers
  ((this qingeditor/eventmgr/mgr) identifiers)
  "Get the `identifiers' list, used to retrieve listeners from
shared event manager."
  (delete-dups identifiers)
  (oset this :identifiers identifiers)
  this)

(defmethod qingeditor/cls/add-identifiers
  ((this qingeditor/eventmgr/mgr) identifiers)
  "Add identifiers."
  (oset this :identifiers
        (delete-dups (append identifiers (oref this :identifiers))))
  this)

(defmethod qingeditor/cls/trigger
  ((this qingeditor/eventmgr/mgr) event-name &optional target event-params)
  "触发一个事件`event-name'，调用所有相关的回调函数。"
  (let ((event (qingeditor/eventmgr/event/init event-name target event-params)))
    (qingeditor/cls/trigger-listeners this event)))

(defmethod qingeditor/cls/trigger-until
  ((this qingeditor/eventmgr/mgr) callback event-name &optional target event-params)
  "执行事件的处理器，直到`callback'返回`t'值。"
  (let ((event (qingeditor/eventmgr/event/init event-name target event-params)))
    (qingeditor/cls/trigger-listeners this event callback)))

(defmethod qingeditor/cls/trigger-event
  ((this qingeditor/eventmgr/mgr) event)
  "Trigger `event', This will invoke the listeners of the `event'."
  (qingeditor/cls/trigger-listeners this event))

(defmethod qingeditor/cls/trigger-event-until
  ((this qingeditor/eventmgr/mgr) callback event)
  "Trigger `event', when event manager invoke listener handler, it will also
invoke `callback' lambda, if lambda return `t', event manager will not invoke
the reset listeners any more."
  (qingeditor/cls/trigger-listeners this event callback))

(defmethod qingeditor/cls/trigger-listeners
  ((this qingeditor/eventmgr/mgr) event &optional callback)
  "Invoke listeners of target event."
  (let ((name (qingeditor/cls/get-name event))
        (responses (qingeditor/eventmgr/response-collection))
        listener-handlers
        response)
    (when (null name)
      (error "Event is missing a name; cannot trigger!"))
    (setq listener-handlers
          (qingeditor/cls/get-listeners-by-event-name this name))
    (catch 'qingeditor-eventmgr-mgr-trigger-listeners-stopped
      (dolist (handler listener-handlers)
        (when (object-of-class-p handler qingeditor/eventmgr/event-handler)
          (setq response (qingeditor/cls/call handler event))
          (qingeditor/cls/push responses response)
          ;; If the event was asked to stop propagating, do so
          (when (qingeditor/cls/get-stop-propagation event)
            (qingeditor/cls/set-stopped responses t)
            (throw 'qingeditor-eventmgr-mgr-trigger-listeners-stopped t))
          ;; If the result causes our validation callback to return true,
          ;; stop propagation
          (when (and callback
                     (funcall callback response))
            (qingeditor/cls/set-stopped responses t)
            (throw 'qingeditor-eventmgr-mgr-trigger-listeners-stopped t)))))
    responses))

(defmethod qingeditor/cls/get-listeners-by-event-name
  ((this qingeditor/eventmgr/mgr) event-name)
  "Get the listeners of event named `event-name'."
  (let ((merged-listener-table (qingeditor/hash-table/init))
        listener-table
        wildcard-listener-table
        shared-listener-table
        listener-alist
        listeners)
    (if (qingeditor/cls/has-key (oref this :events) event-name)
        (setq listener-table (qingeditor/cls/get (oref this :events) event-name))
      (setq listener-table (qingeditor/hash-table/init)))
    (if (qingeditor/cls/has-key (oref this :events) "*")
        (setq wildcard-listener-table (qingeditor/cls/get (oref this :events) "*"))
      (setq wildcard-listener-table (qingeditor/hash-table/init)))
    (if (not (null (oref this :shared-mgr)))
        (setq shared-listener-table
              (qingeditor/cls/get-listeners
               (oref this :shared-mgr) (oref this :identifiers) event-name))
      (setq shared-listener-table (qingeditor/hash-table/init)))
    (setq merged-listener-table
          (qingeditor/cls/merge
           this merged-listener-table listener-table wildcard-listener-table shared-listener-table))
    (setq listener-alist (qingeditor/cls/to-alist merged-listener-table))
    (setq listener-alist
          (sort listener-alist (lambda (left right)
                                 (< (car left) (car right)))))
    (dolist (cur-list listener-alist listeners)
      (setq listeners (append (reverse (cdr cur-list)) listeners)))))

(defmethod qingeditor/cls/merge
  ((this qingeditor/eventmgr/mgr) &rest tables)
  "merge the listeners of `hash-table's recursively."
  (let ((target (qingeditor/hash-table/init)))
    (dolist (table tables)
      (qingeditor/cls/iterate-items
       table
       (progn
         (if (not (qingeditor/cls/has-key target key))
             (qingeditor/cls/set target key (copy-sequence value))
           (let ((source-listeners (copy-sequence value))
                 (target-listeners (qingeditor/cls/get target key)))
             (setq target-listeners (append target-listeners source-listeners))
             (qingeditor/cls/set target key target-listeners))))))
    target))

(defmethod qingeditor/cls/clear-listeners
  ((this qingeditor/eventmgr/mgr) event-name)
  "Clear the listeners of the event named `event-name'."
  (when (qingeditor/cls/has-key (oref this :events) event-name)
    (qingeditor/cls/remove (oref this :events) event-name)))

(provide 'qingeditor-eventmgr-mgr)
