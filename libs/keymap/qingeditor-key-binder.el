;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;

(require 'qingeditor-funcs)

(defvar qingeditor/key-binder/prefix-titles nil
  "alist for mapping command prefixes to long names.")

(defvar qingeditor/default-map (make-sparse-keymap)
  "Base keymap for all spacemacs leader key commands.")

(defun qingeditor/core/key-binder/translate-C-i (_)
  "If `qingeditor/core/user-cfg/distinguish-gui-tab' is non nil, the raw key
sequence does not include <tab> or <kp-tab>, and we are in the
gui, translate to [C-i]. Otherwise, [9] (TAB)."
  (interactive)
  (if (and (not (cl-position 'tab (this-single-command-raw-keys)))
           (not (cl-position 'kp-tab (this-single-command-raw-keys)))
           qingeditor/config/distinguish-gui-tab
           (display-graphic-p))
      [C-i] [?\C-i]))

(define-key key-translation-map [?\C-i] 'qingeditor/key-binder/translate-C-i)

;; (defun qingeditor/key-binder/translate-C-m (_)
;;   "If `qingeditor/config/distinguish-gui-ret' is non nil, the raw key
;; sequence does not include <ret>, and we are in the gui, translate
;; to [C-m]. Otherwise, [9] (TAB)."
;;   (interactive)
;;   (if (and
;;        (not (cl-position 'return (this-single-command-raw-keys)))
;;        (not (cl-position 'kp-enter (this-single-command-raw-keys)))
;;        qingeditor/core/config/distinguish-gui-ret
;;        (display-graphic-p))
;;     [C-m] [?\C-m]))
;; (define-key key-translation-map [?\C-m] 'qingeditor/key-binder/translate-C-m)

(defun qingeditor/key-binder/declare-prefix (prefix name &optional long-name)
  "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command.
LONG-NAME if given is stored in `qingeditor/key-binder/prefix-titles'."
  (let* ((command name)
         (full-prefix (concat qingeditor/config/leader-key " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix))))
    ;; define the prefix command only if it does not already exist
    (unless long-name
      (setq long-name name))
    (which-key-declare-prefixes
      full-prefix (cons name long-name))))

(defun qingeditor/key-binder/declare-prefix-for-mode (mode prefix name &optional long-name)
  "Declare a prefix `prefix'. `mode' is the mode in which this prefix command should
be added. `prefix' is a string describing a key sequence. NAME is a symbol name
used as the prefix command."
  (let ((command (intern (concat (symbol-name mode) name)))
        (full-prefix (concat qingeditor/config/leader-key " " prefix))
        (is-major-mode-prefix (string-prefix-p "m" prefix))
        (major-mode-prefix (concat qingeditor/config/major-mode-leader-key " " (substring prefix 1))))
    (unless long-name (setq long-name name))
    (let ((prefix-name (cons name long-name)))
      (which-key-declare-prefixes-for-mode mode
        full-prefix prefix-name)
      (when (and is-major-mode-prefix qingeditor/config/major-mode-leader-key)
        (which-key-declare-prefixes-for-mode mode major-mode-prefix prefix-name)))))

(defun qingeditor/key-binder/set-leader-keys (key def &rest bindings)
  "Add `key' and `def' as key bindings under `qingeditor/config/leader-key'.
`key' should be a string suitable for passing to `kbd', and it
should not include the leaders. `def' is most likely a quoted
command. See `define-key' for more information about the possible
choices for `def'. This function simply uses `define-key' to add
the bindings.

For convenience, this function will accept additional KEY DEF
pairs. For example,

\(qingeditor/key-binder/set-leader-keys
   \"a\" 'command1
   \"C-c\" 'command2
   \"bb\" 'command3\)"
 
  (while key
    (define-key qingeditor/default-map (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))
(put 'qingeditor/key-binder/set-leader-keys 'lisp-indent-function 'defun)

(defun qingeditor/key-binder/acceptable-leader-p (key)
  "Return t if key is a string and non-empty."
  (and (stringp key) (not (string= key ""))))

(defun qingeditor/key-binder/init-leader-mode-map (mode map &optional minor)
  "Check for MAP-prefix. If it doesn't exist yet, use `bind-map'
  to create it and bind it to `dotspacemacs-major-mode-leader-key'
  and `dotspacemacs-major-mode-emacs-leader-key'. If MODE is a
  minor-mode, the third argument should be non nil."
  (let* ((prefix (intern (format "%s-prefix" map)))
         (leader1 (when (qingeditor/key-binder/acceptable-leader-p
                         qingeditor/config/major-mode-leader-key)
                    qingeditor/config/major-mode-leader-key))
         (leader2 (when (qingeditor/config/key-binder/acceptable-leader-p
                         qingeditor/config/leader-key)
                    (concat qingeditor/config/leader-key
                            (unless minor " m"))))
         (leaders (delq nil (list leader1 leader2))))
    (or (boundp prefix)
        (progn
          (eval
           `(bind-map ,map
              :prefix-cmd ,prefix
              ,(if minor :minor-modes :major-modes) (,mode)
              :keys ,leaders))
          (boundp prefix)))))

(defun qingeditor/key-binder/set-leader-keys-for-major-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`qingeditor/config/major-mode-leader-key'  for the major-mode
`mode'. `mode' should be a quoted symbol corresponding to a valid
major mode. The rest of the arguments are treated exactly like
they are in `qingeditor/key-binder/set-leader-keys'."
  (let* ((map (intern (format "qingeditor/%s-map" mode))))
    (when (qingeditor/key-binder/init-leader-mode-map mode map)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings)
              def (pop bindings))))))
(put 'qingeditor/key-bninder/set-leader-keys-for-major-mode 'lisp-indent-function 'defun)

(defun qingeditor/key-binder/set-leader-keys-for-minor-mode (mode key def &rest bindings)
  "Add `key' and `def' as key bindings under
`qingeditor/config/major-mode-leader-key' and for the minor-mode
`mode'. `mode' should be a quoted symbol corresponding to a valid
minor mode. The rest of the arguments are treated exactly like
they are in `qingeditor/config/set-leader-keys'."
  (let* ((map (intern (format "qingeditor/%s-map" mode))))
    (when (qingeditor/key-binder/init-leader-mode-map mode map t)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))

(put 'qingeditor/key-binder/set-leader-keys-for-minor-mode 'lisp-indent-function 'defun)

(defun qingeditor/key-binder/create-key-binding-form (props func)
  "Helper which returns a form to build `func' to a key according to `props'.

Supported properties:

`:global-key STRING'
    One or several key sequence strings to be set with `global-set-key'.

`:define-key CONS CELL'
    One or several cons cells (MAP . KEY) where MAP is a mode map and KEY is a
    key sequence string to be set with `define-key'.
"
  (let ((global-key (qingeditor/mplist-get props :global-key))
        (def-key (qingeditor/mplist-get props :define-key)))
    (append
     (when global-key
       `((dolist (key ',global-key)
           (global-set-key (kbd key) ',func))))
     (when def-key
       `((dolist (val ',def-key)
           (define-key (eval (car val)) (kbd (cdr val)) ',func)))))))

(provide 'qingeditor-key-binder)
