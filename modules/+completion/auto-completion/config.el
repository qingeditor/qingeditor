;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defvar-local qingeditor/auto-completion/front-end 'company
  "Which auto-completion front end to use.")

(defvar qingeditor/auto-completion/return-key-behavior 'complete
  "What the RET key should do when auto-completion menu is active.
  Possible values are `complete' or `nil'.")

(defvar qingeditor/auto-completion/tab-key-behavior 'cycle
  "What the TAB key should do when auto-completion menu is active.
Possible values are `complete', `cycle' or `nil'.")

(defvar qingeditor/auto-completion/complete-with-key-sequence nil
  "Provide a key sequence (string) to complete the current
selection.")

(defvar qingeditor/auto-completion/complete-with-key-sequence-delay 0.1
  "Timeout (seconds) when waiting for the second key of
`auto-completion-complete-with-key-sequence'.")

(defvar qingeditor/auto-completion/enable-snippets-in-popup nil
  "If non nil show snippets in the auto-completion popup.")

(defvar qingeditor/auto-completion/enable-sort-by-usage nil
  "If non nil suggestions are sorted by how often they are used.")

(defvar qingeditor/auto-completion/enable-help-tooltip nil
  "If non nil the docstring appears in a tooltip.
If set to `manual', help tooltip appears only when invoked
manually.")

(defvar qingeditor/auto-completion/company-mode-completion-cancel-keywords
  '("do"
    "then"
    "begin"
    "case")
  "Keywords on which to cancel completion so that you can use RET
to complet without blocking common line endings.")

(defvar qingeditor/auto-completion/private-snippets-directory nil
  "Configurable private snippets directory.")
