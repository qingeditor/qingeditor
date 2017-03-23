;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qingeditor/find-thing (forward thing)
  "Return `thing' near point as a string.
`thing' should be a symbol understood by `thing-at-point',
e.g. 'symbol or 'word. If `forward' is nil, search backward,
otherwise forward. Return nil if nothing is found."
  (let ((move (if forward #'forward-char #'backward-char))
        (end (if forward #'eobp #'bobp))
        string)
    (save-excursion
      (setq string (thing-at-point thing))
      ;; if there's nothing under point, go forwards
      ;; (or backwards) to find it
      (while (and (null string) (not (funcall end)))
        (funcall move)
        (setq string (thing-at-point thing)))
      (when (stringp string)
        (set-text-properties 0 (length string) nil string))
      (when (> (length string) 0)
        string))))

(defun qingeditor/find-word (forward)
  "Return word near point as a string.
If FORWARD is nil, search backward, otherwise forward.  Returns
nil if nothing is found."
  (qingeditor/find-thing 'word))

(defun qingeditor/find-symbol (forward)
  "Return word near point as a string.
If FORWARD is nil, search backward, otherwise forward.  Returns
nil if nothing is found."
  (qingeditor/find-thing 'symbol))

(defun qingeditor/search-prompt (forward)
  "Return the search prompt for the given direcion."
  (if forward "/" "?"))

(defun qingeditor/search-message (string forward)
  "Prefix `string' with the search prompt."
  (format "%s%s" (qingeditor/search-prompt forward) string))

(provide 'qingeditor-search)
