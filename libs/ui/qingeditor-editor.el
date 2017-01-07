;; qingeditor界面渲染相关函数
(require 'qingeditor-runtime)
(require 'qingeditor-editor-drawer)

(defvar qingeditor/ui/editor/ui-version-info "0.1" "`qingeditor'UI的独立的版本号。")

(defvar qingeditor/ui/editor/buffer-name "*qingeditor*" "`qingeditor'buffer的名称。")

(defvar qingeditor/ui/editor/buttons-startup-lists-offset 25
  "Relative position in characters of the home buffer buttons and the home
 buffer startup lists.")

(defvar qingeditor/ui/editor/buffer-logo-title "[ Q I N G E D I T O R ]" "`qingeditor'logo标题。")

(defvar qingeditor/ui/editor/startup-lists-length 20 "设置startup列表的长度。设置成`nil'则不限制长度。")

(defvar qingeditor/ui/editor/mode-line-format mode-line-format "`qingeditor'默认的`mode-line-format'。")

(defvar qingeditor/ui/editor/official-banner-png
  (expand-file-name (concat qingeditor/banner-dir "img/qingeditor3.png"))
  
  "`qingeditor'官方banner文件名。")
(defvar qingeditor/ui/editor/purple-heart-png
  (expand-file-name (concat qingeditor/banner-dir "img/heart.png"))
  "`qingeditor'官方心跳图标的文件名")

(defvar qingeditor/ui/editor/badge-official-png
  (expand-file-name (concat qingeditor/banner-dir "img/qingeditor-badge.png"))
  "`qingeditor'官方徽章文件名")

(defvar qingeditor/ui/editor/init-redisplay-count 0 "`qingeditor'初始化刷新计数器。")

(defvar qingeditor/ui/editor/after-display-system-init-list-hooks '()
  "系统`frame'初始化完成的时候调用的函数，有些操作必须要等到frame初始化完成之后才能调用。")

(defvar qingeditor/ui/editor/loading-char ?█ "用户现在进度条的填充字符。")

(defvar qingeditor/ui/editor/loading-string "" "当前的进度的显示字符串。")

(defvar qingeditor/ui/editor/loading-counter 0 "当前加载的周期计数器。")

(defvar qingeditor/ui/editor/loading-value 0 "当前已经加载的数量。")

(defvar qingeditor/ui/editor/loading-dots-chunk-count 3 "一个加载单元的点数。")

(defvar qingeditor/ui/editor/loading-dots-count (window-total-width nil 'width) "加载进度条的总长度。")

(defvar qingeditor/ui/editor/loading-dots-chunk-size
  (/ qingeditor/ui/editor/loading-dots-count
     qingeditor/ui/editor/loading-dots-chunk-count) "加载进度条的点的总数。")

(defvar qingeditor/ui/editor/loading-dots-chunk-threshold 0 "每增加一个进度格子计数的阈值。")

(defvar qingeditor/ui/editor/startup-time nil  "`qingeditor'的启动时间。")

(defun qingeditor/ui/editor/refresh-loading-bar ()
  "刷新加载进度条，模拟加载的动画效果。"
  (when (and (not noninteractive) qingeditor/core/user-cfg/loading-progress-bar)
    (setq qingeditor/ui/editor/loading-counter (1+ qingeditor/ui/editor/loading-counter))
    (setq qingeditor/ui/editor/loading-value (1+ qingeditor/ui/editor/loading-value))
    (when (>= qingeditor/ui/editor/loading-counter qingeditor/ui/editor/loading-dots-chunk-threshold)
      (let* ((suffix (format ">%s/%s" qingeditor/ui/editor/loading-value
                             (length qingeditor/layer/layer/used-packages-private)))
             (suffix-length (if (display-graphic-p) (+ (length suffix) 3) (length suffix))))
        (setq qingeditor/ui/editor/loading-counter 0)
        (setq qingeditor/ui/editor/loading-string
              (make-string
               (max 0 (- (* qingeditor/ui/editor/loading-dots-chunk-size
                            (floor (/ qingeditor/ui/editor/loading-value
                                      qingeditor/ui/editor/loading-dots-chunk-threshold)))
                         suffix-length))
               qingeditor/ui/editor/loading-char))
        (qingeditor/ui/editor/set-mode-line (concat qingeditor/ui/editor/loading-string suffix)))
      (qingeditor/ui/editor/redisplay))))

(defun qingeditor/ui/editor/buffer ()
  "获取`qingeditor' buffer, 如果不存在就创建一个新的buffer。"
  (get-buffer-create qingeditor/ui/editor/buffer-name))

(defun qingeditor/ui/editor/draw (&optional refresh)
  "如果qingeditor buffer不存在绘制一个新的qingeditor buffer给`qingeditor-buffer-mode'，
如果存在的话系统切换到存在的buffer上面。"
  (qingeditor/ui/editor-drawer/do-draw refresh))

(defun qingeditor/ui/editor/redisplay ()
  "window刷新皮包函数。"
  (setq qingeditor/ui/editor/init-redisplay-count
        (1+ qingeditor/ui/editor/init-redisplay-count))
  (redisplay))

(defun qingeditor/ui/editor/display-info-box ()
  "显示信息框。"
  (qingeditor/ui/editor-drawer/display-info-box))

(defun qingeditor/ui/editor/setup-after-emacs-startup ()
  (qingeditor/ui/editor-drawer/setup-after-emacs-startup))

(defun qingeditor/ui/editor/remove-unused-gui-elements ()
  "移除`qingeditor'中不适用的界面元素,包含`menu bar', `tool bar'和`scroll bar'."
  (unless (qingeditor/runtime/window-system-is-mac)
    ;; `macos'下面的`Emacs'没有独立的`menu bar'。
    (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
      (menu-bar-mode -1)))
  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1)))

;; hide mode line
;; from http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "在当前的buffer隐藏`mode-line'的minor mode。"
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; 有时间为了刷新mode-line光调用`(force-mode-line-update)'还不够
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "隐藏`mode line mode'已经开启，您可以用`M-x hidden-mode-line-mode'重新"
             "显示`mode-line'。"))))

(defun qingeditor/ui/editor/set-mode-line (format)
  "设置qingeditor buffer的mode-line显示格式。"
  (with-current-buffer (qingeditor/ui/editor/buffer)
    (setq mode-line-format format)))

(defun qingeditor/ui/editor/clear-mode-line ()
  "清空`mode-line'里面的内容。"
  (qingeditor/ui/editor/set-mode-line ""))

(defun qingeditor/ui/editor/refresh-editor (&optional refresh)
  "刷新`qingeditor-buffer-mode'的欢迎buffer。"
  (interactive)
  (setq qingeditor/ui/editor-drawer/last-width-private nil)
  (qingeditor/ui/editor/draw refresh))

(defun qingeditor/ui/editor/buffer-append (msg &optional messagebuf)
  "往`qingeditor' buffer里面附加信息，如果`messagebuf'不为`nil'那么信息同时写入`*Message*' buffer。"
  (with-current-buffer (qingeditor/ui/editor/buffer)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (if messagebuf (message "(QingEditor) %s" msg)))
    (qingeditor/ui/editor/clear-mode-line)))

(defun qingeditor/ui/editor/replace-last-line (msg &optional messagebuf)
  "替换`qingeditor buffer'的最后一行，如果`messagebuf'不为`nil'那么这条信息同时插入`*Message*'buffer
里面。"
  (with-current-buffer (qingeditor/ui/editor/buffer)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (if messagebuf (message "(QingEditor) %s" msg)))
    (qingeditor/ui/editor/clear-mode-line)))

(defun qingeditor/ui/editor/insert-page-break ()
  "在qingeditor buffer插入一个水平风格线。"
  (qingeditor/ui/editor/buffer-append "\n\n"))

(defun qingeditor/ui/editor/register-window-setup-hooks ()
  "注册window设置相关的钩子函数。"
  (qingeditor/ui/editor/register-window-resize-hooks))

(defun qingeditor/ui/editor/register-window-resize-hooks ()
  "注册window大小变动钩子函数。"
  (add-hook 'window-setup-hook
            (lambda ()
              (add-hook 'window-configuration-change-hook
                        (lambda ()
                          (qingeditor/ui/editor/resize-editor)))
              (qingeditor/ui/editor/resize-editor))))

(defun qingeditor/ui/editor/resize-editor ()
  "窗口大小变动处理函数，主要用于居中显示`QingEditor' buffer里面的内容。"
  (let ((space-win (get-buffer-window (qingeditor/ui/editor/buffer)))
        (frame-win (frame-selected-window)))
    (when (and qingeditor/core/user-cfg/startup-buffer-responsive
               space-win
               (not (window-minibuffer-p frame-win)))
      (with-selected-window space-win
        (qingeditor/ui/editor/draw)))))

(defadvice server-create-window-system-frame
    (after qingeditor/ui/init-display activate)
  "当Emacs创建一个frame的时候，运行在`qingeditor/ui/editor/after-display-system-init-list-hooks'里面的
钩子函数，我们可以在钩子函数中进行一些必须在图形系统加载之后进行的一些操作。"
  (progn
    (dolist (fn (reverse qingeditor/ui/editor/after-display-system-init-list-hooks))
      (funcall fn))
    (ad-disable-advice 'server-create-window-system-frame
                       'after
                       'qingeditor/ui/init-display)
    (ad-activate 'server-create-window-system-frame)))

(defmacro qingeditor/ui/do-after-display-system-init (&rest body)
  "如果系统的`display-system'已经初始化，直接运行`BODY'，否则系统会在一个列表中注册相应的函数
然后等到图形系统第一个frame初始化完成的时候进行调用。"
  `(let ((init (cond ((boundp 'ns-initialized) ns-initialized)
                     ;; w32-initialized会过早的调用，所以我们在windows平台使用，我们
                     ;; 通过探测字体列表的方式来探测，因为字体列表知道图形系统初始化完成
                     ;; 才不为空。
                     ((boundp 'w32-initialized) (font-family-list))
                     ((boundp 'x-initialized) x-initialized)
                     ;; 回退到最后的默认方式
                     (t (display-graphic-p)))))
     (if init
         (progn
           ,@body)
       (push (lambda () ,@body) qingeditor/ui/editor/after-display-system-init-list-hooks))))

(defvar qingeditor-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "J") 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "K") 'widget-backward)
    (define-key map (kbd "C-r") 'spacemacs-buffer/refresh)
    (define-key map "q" 'quit-window)
    map)
  "qingeditor buffer的keymap。")

(with-eval-after-load 'evil
  (evil-make-overriding-map qingeditor-buffer-mode-map 'motion))

(define-derived-mode qingeditor-buffer-mode fundamental-mode "qingeditor buffer"
  "开始欢迎页面的buffer mode定义。

\\<qingeditor-buffer-mode-map>"
  :group 'qingeditor
  :syntax-table nil
  :abbrev-table nil
  (page-break-lines-mode)
  (setq buffer-read-only t
        truncate-lines t)
  ;; needed to make tab work correctly in terminal
  (evil-define-key 'motion qingeditor-buffer-mode-map
    (kbd "C-i") 'widget-forward)
  ;; motion state since this is a special mode
  (evil-set-initial-state 'qingeditor-buffer-mode 'motion))

(defun qingeditor/ui/editor/display-summary (start-time)
  "显示一个加载统计信息。"
  (qingeditor/ui/editor-drawer/display-summary start-time))

(defun qingeditor/ui/editor/render-loading-text ()
  "显示一个加载提示,因为我们加载中文字体的时候有点慢。"
  (insert "LOADING ... ")
  (mark-paragraph)
  (let* ((body-height (window-body-height))
        (newlines (- (/ body-height 2) 4)))
    (newline newlines))
  (qingeditor/ui/editor-drawer/center-current-line)
  (qingeditor/ui/editor/redisplay))

(provide 'qingeditor-editor)
