;; 这个类只要负责`qingeditor'的字体设置

(require 'qingeditor-io)

(defvar qingeditor/ui/editor-font/diminished-minor-modes nil
  "显示在`mode-line'上面的mode的代表字符。")

(defun qingeditor/ui/editor-font/set-default-font (plists)
  "设置plist里面的字体。

`PLIST'格式要是`(\"fontname\" :prop1 val1 :prop2 val2 ...)'，要么是这种格式的
列表，系统将这个列表中的第一个存在的字体设置城编辑器的字体。

如果列表里面的字体都不存在返回`nil'，否则返回t。"
  (unless (listp (car plists))
    (setq plists (list plists)))
  (catch 'break
    (dolist (plist plists)
      (when (find-font (font-spec :name (car plist)))
        (let* ((font (car plist))
               (props (cdr plist))
               (scale (plist-get props :powerline-scale))
               (font-props (qingeditor/core/mplist-remove (qingeditor/core/mplist-remove props :powerline-scale)
                                                   :powerline-offset))
               (fontspec (apply 'font-spec :name font font-props)))
          (qingeditor/core/io/message "正在设置字体 \"%s\"..." font)
          (set-frame-font fontspec nil t)
          (push `(font . ,(frame-parameter nil 'font)) default-frame-alist)
          (setq-default powerline-scale scale)
          (setq-default powerline-height (qingeditor/ui/editor-font/compute-powerline-height))
          ;; qingeditor最终的容错字体
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
            ;; 移除字体的高度属性，然后使用默认的大小(主要是用于缩放)
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
