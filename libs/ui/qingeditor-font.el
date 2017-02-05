;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: MIT
;;
;; Define some functions for font handle.

(defvar qingeditor/font/diminished-minor-modes nil
  "。List of diminished moodes to unicode or ascii values.")

(defun qingeditor/font/set-default-font (plists)
  "Set the font given the passed `plist'.

`plist' has either the form (\"fontname\" :prop1 val1 :prop2 val2 ...)
or is a list of such. The first font that can be found will be used.

The return value is nil if no font was found, truthy otherwise."
  (unless (listp (car plists))
    (setq plists (list plists)))
  (catch 'break
    (dolist (plist plists)
      (when (find-font (font-spec :name (car plist)))
        (let* ((font (car plist))
               (props (cdr plist))
               (scale (plist-get props :powerline-scale))
               (font-props (qingeditor/mplist-remove (qingeditor/mplist-remove props :powerline-scale)
                                                   :powerline-offset))
               (fontspec (apply 'font-spec :name font font-props)))
          (qingeditor/message "Setting font  \"%s\"..." font)
          (set-frame-font fontspec nil t)
          (push `(font . ,(frame-parameter nil 'font)) default-frame-alist)
          (setq-default powerline-scale scale)
          (setq-default powerline-height (qingeditor/font/compute-powerline-height))
          ;; fallback font for unicode characters used in `qingeditor'
          (pcase system-type
            (`gnu/linux
             (setq fallback-font-name "NanumGothic")
             (setq fallback-font-name2 "NanumGothic"))
            (`darwin
             (setq fallback-font-name "Arial Unicode MS")
             (setq fallback-font-name2 "Arial Unicode MS"))
            (`windows-nt
             (setq fallback-font-name "MS Gothic")
             (setq fallback-font-name2 "Lucida Sans Unicode"))
            (`cygwin
             (setq fallback-font-name "MS Gothic")
             (setq fallback-font-name2 "Lucida Sans Unicode"))
            (other
             (setq fallback-font-name nil)
             (setq fallback-font-name2 nil)))
          (when (and fallback-font-name fallback-font-name2)
            ;; remove any size or height properties in order to be able to
            ;; scale the fallback fonts with the default one (for zoom-/out
            ;; for instance)
            (let* ((fallback-props (qingeditor/core/mplist-remove
                                    (qingeditor/core/mplist-remove font-props :size)
                                    :height))
                   (fallback-spec (apply 'font-spec :name fallback-font-name fallback-props))
                   (fallback-spec2 (apply 'font-spec :name fallback-font-name2 fallback-props)))
              ;; window numbers
              (set-fontset-font "fontset-default"
                                '(#x2776 . #x2793) fallback-spec nil 'prepend)
              ;; mode line circle letters
              (set-fontset-font "fontset-default"
                                '(#x24b6 . #x24fe) fallback-spec nil 'prepend)
              ;; mode line additional characters
              (set-fontset-font "fontset-default"
                                '(#x2295 . #x22a1) fallback-spec nil 'prepend)
              ;;  new version lighter
              (set-fontset-font "fontset-default"
                                '(#x2190 . #x2200) fallback-spec2 nil 'prepend))))
        (throw 'break t)))
    nil))

(defun qingeditor/ui/editor-font/compute-powerline-height ()
  "获取一个调整过得powerline的高度值。"
  (let ((scale (if (and (boundp 'powerline-scale) powerline-scale)
                   powerline-scale 1)))
    (truncate (* scale (frame-char-height)))))

(defmacro qingeditor/ui/editor-font/diminish (mode &optional unicode ascii)
  "根据`qingeditor/core/user-cfg/mode-line-unicode-symbols'来决定是否在`mode-line’上显示
一个`mode'的简称，如果`ascii'没有提供就显示`unicode'，如果两个都没有提供的话，就什么也不显示。"
  `(let* ((cell (assq ',mode qingeditor/ui/editor-font/diminished-minor-modes)))
     (if cell
         (setcdr cell '(,unicode ,ascii))
       (push '(,mode ,unicode ,ascii) qingeditor/ui/editor-font/diminished-minor-modes))))

(defmacro qingeditor/ui/editor-font/hide-lighter (mode)
  "是否显示简约版的`mode'的名字。"
  `(eval-after-load 'diminish '(diminish ',mode)))

(provide 'qingeditor-editor-font)
