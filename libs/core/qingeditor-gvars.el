;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; define some global variables and consts

(defgroup qingeditor nil "qingeditor variables group"
  :group 'starter-kit
  :prefix 'qingeditor/)

(defvar qingeditor/editor-ready-hook nil
  "when qineditor load finished, run hook in this list.")

(defvar qingeditor/insecure nil
  "If non-nil force `qingeditor' to operate without secured protocols.")

(defvar qingeditor/post-theme-change-hook nil
  "theme change hooks.")

(defvar qingeditor/window-resize-hook nil
  "When `emacs' frame changed it's size, run this hook.")

(defvar qingeditor/display-system-ready-init-list nil
  "When display system initialized, run this hook.")

(defvar qingeditor/user-config-setup-finished-hook nil
  "After invoke user config setup function, run this hook.")

(defvar qingeditor/user-config-setup-hook-invoked nil
  "`t' if config setup had been runned.")

(defvar qingeditor/initialized nil
  "`t' if the qingeditor finished init.")

(defconst qingeditor/use-package-add-hook-keywords
  '(:pre-init
    :post-init
    :pre-config
    :post-config))

(defvar qingeditor/command-properties nil
  "Specifications made by `qingeditor/define-command'.")

(defvar qingeditor/interactive-alist nil
  "Association list of qingeditor-specific interactive codes.")

(defcustom qingeditor/track-eol t
  "If non-nil line moves after a call to `qingeditor/end-of-line' stay at eol.
This is analogous to `track-eol' but deals with the end-of-line
interpretation of qingeditor."
  :type 'boolean
  :group 'qingeditor)

(defcustom qingeditor/move-beyond-eol nil
  "Whether the cursor is allowed to move past the last character of 
a line."
  :type 'boolean
  :group 'qingeditor)

(defcustom qingeditor/indent-convert-tabs t
  "If non-nil `qingeditor/indent' converts between leading tabs and spaces.
  Whether tabs are converted to spaces or vice versa depends on the
  value of `indent-tabs-mode'."
  :type 'boolean
  :group 'qingeditor)

(qingeditor/define-local-var qingeditor/this-type nil
  "current motion type.")

(qingeditor/define-local-var qingeditor/undo-list-pointer nil
  "Everything up to this mark is united in the undo-list.")

(defvar qingeditor/in-single-undo nil
  "Set to non-nil if the current undo steps are connected.")

(defvar qingeditor/temporary-undo nil
  "When undo is disabled in current buffer.
Certain commands depending on undo use this variable
instead of `buffer-undo-list'.")

(defvar qingeditor/repl-list '()
  "List of all registered REPLs.")

(qingeditor/define-local-var qingeditor/echo-area-message nil
  "Previous value of `current-message'.")

(defvar qingeditor/write-echo-area nil
  "If set to t inside `qingeditor/save-echo-area', then the echo area
is not restored.")

(qingeditor/define-local-var qingeditor/no-display nil
  "If non-nil, various Evil displays are inhibited.
Use the macro `qingeditor/without-display' to set this variable.")

(defgroup qingeditor/cjk nil
  "CJK support"
  :prefix "evilqingeditor/cjk/"
  :group 'qingeditor)

(defcustom qingeditor/cjk/emacs-word-boundary nil
  "Determin word boundary exactly the same way as Emacs does."
  :type 'boolean
  :group 'qingeditor/cjk)

(defcustom qingeditor/cjk/word-separating-categories
  '(;; Kanji
    (?C . ?H) (?C . ?K) (?C . ?k) (?C . ?A) (?C . ?G)
    ;; Hiragana
    (?H . ?C) (?H . ?K) (?H . ?k) (?H . ?A) (?H . ?G)
    ;; Katakana
    (?K . ?C) (?K . ?H) (?K . ?k) (?K . ?A) (?K . ?G)
    ;; half-width Katakana
    (?k . ?C) (?k . ?H) (?k . ?K) ; (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?C) (?A . ?H) (?A . ?K) ; (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?C) (?G . ?H) (?G . ?K) ; (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `qingeditor/cjk/word-boundary-p'. See the documentation of
`word-separating-categories'. Use `describe-categories' to see
the list of categories."
  :type '((character . character))
  :group 'qingeditor/cjk)

(defcustom qingeditor/cjk/word-combining-categories
  '(;; default value in word-combining-categories
    (nil . ?^) (?^ . nil)
    ;; Roman
    (?r . ?k) (?r . ?A) (?r . ?G)
    ;; half-width Katakana
    (?k . ?r) (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?r) (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?r) (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `qingeditor/cjk/word-boundary-p'. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
  :type '((character . character))
  :group 'qingeditor/cjk)

(defvar-local qingeditor/gne-min-line nil
  "The first line in the buffer that is a valid result.")

(defvar-local qingeditor/gne-max-line nil
  "The last line in the buffer that is a valid result.")

(defvar-local qingeditor/gne-cur-line 0
  "The current line in the buffer. (It is problematic to use
point for this.)")

(defvar-local qingeditor/gne-line-func nil
  "The function to call to visit the result on a line.")

(qingeditor/define-local-var qingeditor/scroll-count 0
  "Holds last used prefix for `qing-scroll-up'
and `qing-scroll-down'.
Determine how many lines should be scrolled.
Default value is 0 - scroll half the screen.")

(qingeditor/define-local-var qingeditor/scroll-line-count 1
  "Holds last used prefix for `qing-scroll-line-up'
and `qing-scroll-line-down'.
Determine how many lines should be scrolled.
Default value is 1 line.")

(qingeditor/define-local-var qingeditor/this-register nil
  "Current register.")

(qingeditor/define-local-var qingeditor/this-macro nil
  "Current macro register.")

(qingeditor/define-local-var qingeditor/this-operator nil
  "Current operator.")

(qingeditor/define-local-var qingeditor/this-motion nil
  "Current motion.")

(qingeditor/define-local-var qingeditor/this-motion-count nil
  "Current motion count.")

(defvar qingeditor/type-properties nil
  "Specifications made by `qingeditor/define-type'.
Entries have the form (TYPE . PLIST), where PLIST is a property
list specifying functions for handling the type: expanding it,
describing it, etc.")

(provide 'qingeditor-gvars)
