;;; page-break-lines.el --- Display ugly ^L page breaks as tidy horizontal lines

;; Copyright (C) 2012-2015 Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/page-break-lines
;; Package-Version: 20160109.1813
;; Package-X-Original-Version: DEV
;; Keywords: convenience, faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides a global mode which displays form feed
;; characters as horizontal rules.

;; Install from Melpa or Marmalade, or add to `load-path' and use
;; (require 'qingeditor-page-break-lines).

;; Use `qingeditor/page-break-lines-mode' to enable the mode in specific buffers,
;; or customize `qingeditor/page-break-lines-modes' and enable the mode globally with
;; `qingeditor/global-page-break-lines-mode'.

;; Issues and limitations:

;; If `qingeditor/mode/page-break-lines-char' is displayed at a different width to
;; regular characters, the rule may be either too short or too long:
;; rules may then wrap if `truncate-lines' is nil. On some systems,
;; Emacs may erroneously choose a different font for the page break
;; symbol, which choice can be overridden using code such as:

;; (set-fontset-font "fontset-default"
;;                   (cons page-break-lines-char page-break-lines-char)
;;                   (face-attribute 'default :family))

;; Use `describe-char' on a page break char to determine whether this
;; is the case.

;; Additionally, the use of `text-scale-increase' or
;; `text-scale-decrease' will cause the rule width to be incorrect,
;; because the reported window width (in characters) will continue to
;; be the width in the frame's default font, not the scaled font used to
;; display the rule.

;; Adapted from code http://www.emacswiki.org/emacs/PageBreaks
;;; Code:

(defgroup qingeditor/page-break-lines nil
  "Display ugly ^L page breaks as tidy horizontal lines."
  :prefix "qingeditor/page-break-lines-"
  :group 'faces)

(defcustom qingeditor/page-break-lines/display-char ?─
  "Character used to render page break lines."
  :type 'character
  :group 'qingeditor/page-break-lines)

(defcustom qingeditor/page-break-lines/lighter " PgLn"
  "Mode-line indicator for `qingeditor/page-break-lines-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :group 'qingeditor/page-break-lines)

(defcustom qingeditor/page-break-lines/modes
  '(emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode)
  "Modes the which to enable `qingeditor/page-break-lines-mode'."
  :group 'qingeditor/page-break-lines)

(defface qingeditor/page-break-lines/face
  '((t :inherit font-lock-comment-face :bold nil :italic nil))
  "Face used to colorize page break lines.
If using :bold or :italic, please ensure `qingeditor/mode/page-break-lines-char'
is available in that variant of your font, otherwise it may be displayed
as junk character."
  :group 'qingeditor/page-break-lines)



;;;###autoload
(define-minor-mode qingeditor/page-break-lines-mode
  "Toggle Page Break Lines mode.

In page break lines mode, page breaks (^L characters) are displayed as a
horizontal line of `qingeditor/page-break-lines/display-char' characters."
  :lighter qingeditor/page-break-lines/lighter
  :group 'qingeditor/page-break-lines
  (qingeditor/page-break-lines/update-display-tables))

;;;###autoload
(defun qingeditor/page-break-lines/turn-on ()
  "Enable `qingeditor/page-break-lines-mode' in this buffer."
  (qingeditor/page-break-lines-mode 1))

;;;###autoload
(defun qingeditor/page-break-lines-mode/turn-off ()
  "Disable `qingeditor/page-break-lines-mode' in this buffer."
  (qingeditor/page-break-lines-mode -1))

(dolist (hook '(window-configuration-change-hook
                after-setting-font-hook))
  (add-hook hook 'qingeditor/page-break-lines/update-display-tables))



(defun qingeditor/page-break-lines/update-display-tables ()
  "Function called for updating display table."
  (mapc 'qingeditor/page-break-lines/update-display-table (window-list nil 'no-minibuffer)))

(defun qingeditor/page-break-lines/update-display-table (window)
  "Modify a display-table that displays page-breaks prettily.
If the buffer inside `window' has `qingeditor/page-break-lines-mode' enabled,
its display table will be modified as necessary."
  (with-current-buffer (window-buffer window)
    (if qingeditor/page-break-lines-mode
        (progn
          (unless buffer-display-table
            (setq buffer-display-table (make-display-table)))
          (let* ((width (- (window-width window) 1))
                 (glyph (make-glyph-code
                         qingeditor/page-break-lines/display-char 'qingeditor/page-break-lines/face))
                 (new-display-entry (vconcat (make-list width glyph))))
            (unless (equal new-display-entry (elt buffer-display-table ?\^L))
              (aset buffer-display-table ?\^L new-display-entry))))
      (when buffer-display-table
        (aset buffer-display-table ?\^L nil)))))



;;;###autoload
(defun qingeditor/page-break-lines/maybe ()
  "Enable `qingeditor/page-break-lines-mode' in current buffer if desired.
When `major-mode' is listed in `qingeditor/page-break-lines/modes', then
`qingeditor/page-break-lines-mode' will be enabled."
  (if (and (not (minibufferp))
           (apply 'derived-mode-p qingeditor/page-break-lines/modes))
      (qingeditor/page-break-lines-mode 1)))

;;;###autoload
(define-global-minor-mode qingeditor/global-page-break-lines-mode
  qingeditor/page-break-lines-mode qingeditor/page-break-lines/maybe
  :group 'qingeditor/page-break-lines)

(provide 'qingeditor-page-break-lines)
