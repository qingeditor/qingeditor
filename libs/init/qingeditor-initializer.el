;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; define global initializer class
(require 'eieio-base)
(require 'qingeditor-stddir)
(require 'qingeditor-init-event)
(require 'qingeditor-eventmgr-mgr)

(defclass qingeditor/initializer ()
  ((eventmgr
     :initarg :eventmgr
     :initform nil
     :type (satisfies (lambda (obj) (or (null obj) (object-of-class-p obj qingeditor/eventmgr/mgr))))
     :documentation "the event manager of `qingeditor/initializer' object"))
  :documentation "global initializer class")

(defmethod qingeditor/initializer/init ((this qingeditor/initializer))
  "init `qingeditor' in this method, we first find the configuration
file in load-path, if the configuration not exist, `qingeditor' will
first generate it, then load it normally. after load the configuration file,
we finally process `qingeditor' modules."
  (qingeditor/initializer/load-editor-cfg-file this))

(defmethod qingeditor/initializer/load-editor-cfg-file
  ((this qingeditor/initializer))
  "load `qingeditor' configuration file."
  (qingeditor/initializer/detect-init-filename this)
  (unless (file-exists-p qingeditor/config/target-cfg-filename)
    (qingeditor/initializer/generate-new-cfg-filename-from-tpl
     this 'with-wizard))
  (when (load-file qingeditor/config/target-cfg-filename)
    (let ((event (qingeditor/init/event/init
                  qingeditor/init/event/editor-cfg-ready-event
                  this)))
      (qingeditor/init/event/set-initializer event this)
      (qingeditor/eventmgr/mgr/trigger-event (oref this :eventmgr) event)
      )))

(defmethod qingeditor/initializer/generate-new-cfg-filename-from-tpl
  ((this qingeditor/initializer) &optional arg)
  "Generate a new configuration file for `qingeditor'."
  ;; preferences is alist where the key is the text to replace
  ;; the value in the configuration template file
  (let ((preferences
         (when arg
           `(("distribution 'editor-standard"
              ,(format
                "distribution '%S"
                (qingeditor/initializer/ido-completing-read
                 this
                 "What distribution of qingeditor would you like to start with?"
                 `(("The standard distribution, recommended (editor-standard)"
                    editor-standard)
                   (,(concat "A minimalist distribution that you can build on"
                             " (editor-base)")
                    editor-base)))))
             ("helm"
              ,(qingeditor/initializer/ido-completing-read
                this
                "What type of completion framework di you want? "
                '(("A heavy one but full featured (helm)"
                   "helm")
                  ("A lighter one but still powerfull (ivy)"
                   "ivy")
                  ;; for now, None works only if the user selected
                  ;; the `editor-base' distribution
                  ("None (not recommended)" ""))))))))
    (with-current-buffer (find-file-noselect
                          (concat qingeditor/template-dir ".qingeditor.template"))
      (dolist (p preferences)
        (goto-char (point-min))
        (re-search-forward (car p))
        (replace-match (cadr p)))
      (let ((install
             (if (file-exists-p qingeditor/config/target-cfg-filename)
                 (y-or-n-p
                  (format "%s already exists. Do you want to overwrite it ? "
                          qingeditor/config/target-cfg-filename))
               t)))
        (when install
          (write-file qingeditor/config/target-cfg-filename)
          (message "%s has been generate success."
                   qingeditor/config/target-cfg-filename)
          t)))))

(defmethod qingeditor/initializer/ido-completing-read
  ((this qingeditor/initializer) prompt candidates)
  "Call `ido-completing-read' with a CANDIDATES alist where the key is
a display string and the value is the actual to return."
  (let ((ido-max-window-height (1+ (length candidates))))
    (cadr (assoc (ido-completing-read prompt (mapcar 'car candidates)) candidates))))

(defmethod qingeditor/initializer/detect-init-filename
  ((this qingeditor/initializer))
  "figure out the target configuration full filename."
  (let* ((env (getenv "QINGEDITOR_DIR"))
         (env-dir (when env (expand-file-name (concat env "/"))))
         (env-init-filename (and env-dir (expand-file-name "init.el" env-dir)))
         (no-env-dir-default
          (expand-file-name (concat qingeditor/user-home-dir ".qingeditor.d/")))
         (default-init-filename (expand-file-name ".qingeditor" qingeditor/user-home-dir))
         target-cfg-dir
         target-cfg-filename)
    (setq target-cfg-dir
          (cond
           ((and env (file-exists-p env-dir))
            env-dir)
           ((file-exists-p no-env-dir-default)
            no-env-dir-default)
           (t nil)))
    (let ((default-qingeditor-init-filename
            (when target-cfg-dir
              (concat target-cfg-dir "init.el"))))
      (setq target-cfg-filename
            (cond (env-init-filename)
                  ((file-exists-p default-init-filename) default-init-filename)
                  ((and target-cfg-dir (file-exists-p default-qingeditor-init-filename))
                   default-qingeditor-init-filename)
                  (t default-init-filename))))
    (setq-default qingeditor/config/target-cfg-dir target-cfg-dir)
    (setq-default qingeditor/config/target-cfg-filename target-cfg-filename)
    (list target-cfg-dir target-cfg-filename)))

(defmethod qingeditor/initializer/set-eventmgr
  ((this qingeditor/initializer) eventmgr)
  (qingeditor/eventmgr/mgr/set-identifiers eventmgr '("qingeditor/initializer"))
  (oset this :eventmgr eventmgr)
  this)

(provide 'qingeditor-initializer)
