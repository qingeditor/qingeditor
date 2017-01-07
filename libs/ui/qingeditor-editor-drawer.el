(require 'qingeditor-editor-info)
(require 'qingeditor-user-cfg)
(require 'qingeditor-runtime)

(defvar qingeditor/ui/editor-drawer/var-export/release-note-version nil
  "这个变量主要用于保存导出的`release-note-version'数据，内部使用。")

(defvar qingeditor/ui/editor-drawer/last-width-private nil
  "上次计算出的`qingeditor'buffer的宽度。")

(defvar qingeditor/ui/editor-drawer/banner-length-private 75
  "banner的宽度。")

(defvar qingeditor/ui/editor-drawer/note-widgets-private nil
  "release note显示列表。")

(defvar qingeditor/ui/editor-drawer/random-banner-private nil
  "当前随机选择的Ascii类型的banner文件, 记录下来，保证一个编辑session里面banner文件
保持不变。")

(defvar qingeditor/ui/editor-drawer/cache-file-private (expand-file-name (concat qingeditor/cache-dir "qingeditor-buffer.el"))
  "qingeditor buffer的缓存文件，存放qingeditor buffer的一些持久化信息。")

(defvar qingeditor/ui/editor-drawer/previous-insert-type-private nil
  "上一个插入的note的类型。")

(defvar qingeditor/ui/editor-drawer/buttons-position-private 0
  "frame边缘和home按钮之间的字符数目。")

(defvar qingeditor/ui/editor-drawer/note-widgets-private nil
  "当前插入的note小组件的对象列表。")

(defvar qingeditor/ui/editor-drawer/release-note-version-private nil
  "如果发行说明框销毁了这个值是`nil'。如果不为`nil'他包含一个版本号， 这个版本号小于当前版本号发行说明信息框显示。")

(defun qingeditor/ui/editor-drawer/do-draw (&optional refresh)
  (let ((buffer-exists (buffer-live-p (get-buffer qingeditor/ui/editor/buffer-name)))
        (save-line nil))
    (when (or (not (eq qingeditor/ui/editor-drawer/last-width-private (window-width)))
              (not buffer-exists)
              refresh)
      (setq qingeditor/ui/editor-drawer/banner-length-private (window-width))
      (setq qingeditor/ui/editor-drawer/last-width-private  qingeditor/ui/editor-drawer/banner-length-private)
      (with-current-buffer (get-buffer-create qingeditor/ui/editor/buffer-name)
        (page-break-lines-mode)
        (save-excursion
          (when (> (buffer-size) 0)
            (set 'save-line (line-number-at-pos))
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (qingeditor/ui/editor/clear-mode-line)
          ;; 当我们删除buffer再创建的时候需要清空变量里面的数据
          (setq qingeditor/ui/editor-drawer/note-widgets-private nil)
          (qingeditor/ui/editor-drawer/render-banner-and-buttons)
          ;; 如果emacs-startup-hook运行了这个不为`nil'
          (if (bound-and-true-p qingeditor/core/runtime/initialized)
              (progn
                (qingeditor/ui/editor-drawer/display-summary qingeditor/start-time)
                (when qingeditor/core/user-cfg/startup-lists
                  (qingeditor/ui/editor-drawer/render-startupify-lists))
                (qingeditor/ui/editor-drawer/render-footer)
                (qingeditor/ui/editor/set-mode-line qingeditor/ui/editor/mode-line-format)
                (force-mode-line-update)
                (qingeditor-buffer-mode))
            (add-hook 'emacs-startup-hook 'qingeditor/ui/editor/setup-after-emacs-startup t))))
      (if save-line
          (progn (goto-char (point-min))
                 (forward-line (1- save-line))
                 (forward-to-indentation 0))
        (qingeditor/ui/editor-drawer/goto-link-line))
      (switch-to-buffer qingeditor/ui/editor/buffer-name)
      (qingeditor/ui/editor/redisplay))))

(defun qingeditor/ui/editor-drawer/goto-link-line ()
  "将光标定位到QingEditor的连接地址的第一行。"
  (with-current-buffer qingeditor/ui/editor/buffer-name
    (goto-char (point-min))
    (with-demoted-errors "qingeditor buffer错误: %s"
      (widget-forward 1))))

(defun qingeditor/ui/editor-drawer/setup-after-emacs-startup ()
  "一些ui元素需要Emacs已经启动了才能进行设置。"
  (with-current-buffer (get-buffer qingeditor/ui/editor/buffer-name)
    (when qingeditor/core/user-cfg/startup-lists
      (qingeditor/ui/editor-drawer/render-startupify-lists))
    (qingeditor/ui/editor-drawer/render-footer)
    (if qingeditor/core/runtime/error-count
        (progn
          (qingeditor-buffer-mode)
          (qingeditor/ui/editor/set-mode-line
           (format "在初始化过程中出现%s个错误！QingEditor功能可能出现异常。"
                   qingeditor/core/runtime/error-count))
          (face-remap-add-relative 'mode-line
                                   '((:background "red") mode-line)))
      (qingeditor/ui/editor/set-mode-line qingeditor/ui/editor/mode-line-format)
      (qingeditor-buffer-mode))
    (force-mode-line-update)
    (qingeditor/ui/editor-drawer/goto-link-line)))

(defun qingeditor/ui/editor-drawer/render-footer ()
  "渲染`QingEditor'的底部一些界面。"
  (save-excursion
    (let* ((maxcol qingeditor/ui/editor-drawer/banner-length-private)
           (heart-path qingeditor/ui/editor/purple-heart-png)
           (heart (when (and (display-graphic-p)
                             (image-type-available-p
                              (intern (file-name-extension heart-path))))
                    (create-image heart-path)))
           (heart-size (when heart (car (image-size heart))))
           (build-lhs "Made with ")
           (build-rhs " by the community")
           (buffer-read-only nil))
      (when heart
        (goto-char (point-max))
        (qingeditor/ui/editor/insert-page-break)
        (insert "\n")
        (when heart
          (insert (make-string (floor (/ (- maxcol
                                            (length build-lhs)
                                            heart-size
                                            (length build-rhs)) 2)) ?\ ))
          (insert build-lhs)
          (insert-image heart)
          (insert build-rhs)
          (insert "\n"))))))

(defun qingeditor/ui/editor-drawer/render-startupify-lists ()
  "绘制两个初始化列表。"
  (with-current-buffer qingeditor/ui/editor/buffer-name
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (qingeditor/ui/editor/insert-page-break)
      (insert "\n")
      (save-restriction
        (narrow-to-region (point) (point))
        (qingeditor/ui/editor-drawer/do-render-startupify-lists)
        (qingeditor/ui/editor-drawer/center-startupify-lists)))))

(defun qingeditor/ui/editor-drawer/do-render-startupify-lists ()
  ;; TODO
  (let ((list-separator "\n\n"))
    (mapc (lambda (els)
            (let ((el (or (car-safe els) els))
                  (list-size (or (cdr-safe els) qingeditor/ui/editor/startup-lists-length)))
              (cond
               ((eq el 'warnings)))))
          (append '(warnings)
                  qingeditor/core/user-cfg/startup-lists))))

(defun qingeditor/ui/editor-drawer/center-startupify-lists ()
  ;; TODO
  )

(defun qingeditor/ui/editor-drawer/register-window-setup-hook ()
  "在window设置钩子注册我们针对窗口大小变动以及配置变动的钩子函数。"
  )

(defun qingeditor/ui/editor-drawer/render-banner-and-buttons ()
  "在QingEditor buffer里面绘制`qingeditor/core/user-cfg/startup-banner'指定的图片，在banner的下面再绘制
一些快捷按钮。

系统支持Ascii Art:
Doge special text banner can be reachable via `999', `doge' or `random*'.
Cate special text banner can de reachable via `998', `cat' or `random*'.
`random' ignore special banners whereas `random*' does not."
  (let ((banner (qingeditor/ui/editor-drawer/choose-banner))
        (buffer-read-only nil))
    (progn
      (when banner
        (qingeditor/core/io/message (format "Banner: %s" banner))
        (if (image-type-available-p (intern (file-name-extension banner)))
            (qingeditor/ui/editor-drawer/render-image-banner banner)
          (qingeditor/ui/editor-drawer/render-ascii-banner-centered banner))
        (qingeditor/ui/editor-drawer/inject-version)
        (qingeditor/ui/editor-drawer/render-buttons)))))

(defun qingeditor/ui/editor-drawer/render-buttons ()
  "在logo下面插入一些快捷按钮。"
  (goto-char (point-max))
  (insert "\n")
  (qingeditor/ui/editor-drawer/insert-shortcut "m" "[?]" t)
  (widget-create
   'url-link
   :tag (propertize "?" 'face 'font-lock-doc-face)
   :help-echo "打开快速帮助。"
   :action (lambda (&rest ignore)
             (qingeditor/ui/editor-drawer/toggle-note
              (concat qingeditor/info-dir "quickhelp.txt")
              ;; if nil is returned,
              ;; jsut delete the current note widgets
              (qingeditor/ui/editor-drawer/insert-note-p 'quickhelp)))
   :mouse-face 'highlight
   :follow-link "\C-m")

  (insert " ")

  (widget-create
   'url-link
   :tag (propertize "项目主页" 'face 'font-lock-keyword-face)
   :help-echo "在浏览器中打开QingEditor的GitHub官方主页。"
   :mouse-face 'highlight
   :follow-link "\C-m"
   "https://github.com/qcoreteam/qingeditor")

  (insert " ")

  (widget-create
   'url-link
   :tag (propertize "文档" 'face 'font-lock-keyword-face)
   :help-echo "在浏览器中打开QingEditor的文档。"
   :mouse-face 'highlight
   :follow-link "\C-m"
   "http://qingeditor.org/doc/DOCUMENTATION.html")

  (insert " ")

  (widget-create
   'url-link
   :tag (propertize "官方社区" 'face 'font-lock-keyword-face)
   :help-echo "在浏览器打开我们QingEditor官方论坛。"
   :mouse-face 'highlight
   :follow-link "\C-m"
   "http://bbs.qingeditor.org")

  (insert " ")
  (widget-create
   'push-button
   :tag (propertize "升级QingEditor" 'face 'font-lock-keyword-face)
   :help-echo "升级QingEditor核心库和相应的配置层。"
   :action (lambda (&rest ignore) ())
   :follow-link "\C-m")

  (let ((len (- (line-end-position)
                (line-beginning-position))))
    (qingeditor/ui/editor-drawer/center-current-line)
    (setq qingeditor/ui/editor-drawer/buttons-position-private (- (line-end-position)
                                           (line-beginning-position)
                                           len)))
  (insert "\n")

  (widget-create
   'push-button
   :tag (propertize "更新ELPA程序包" 'face 'font-lock-keyword-face)
   :help-echo "更新所有的ELPA包到最新版本。"
   :action (lambda (&rest ignore) ())
   :mouse-face 'highlight
   :follow-link "\C-m")

  (insert " ")
  (widget-create
   'push-button
   :tag (propertize "回滚ELPA程序包" 'face 'font-lock-keyword-face)
   :help-echo "当升级遇到问题的时候，我们可以回滚升级的ELPA程序包到以前的版本"
   :action (lambda (&rest ignore) ())
   :mouse-face 'highlight
   :follow-link "\C-m")

  (qingeditor/ui/editor-drawer/center-current-line)
  (insert "\n")

  (widget-create
   'push-button
   :tag (propertize "升级日志" 'face 'font-lock-preprocessor-face)
   :help-echo "打开或者关闭升级日志。"
   :action (lambda (&rest ignore) ())
   :mouse-face 'highlight
   :follow-link "\C-m")

  (insert " ")
  (widget-create
   'url-link
   :tag (propertize "搜索QingEditor内置文档" 'face 'font-lock-function-name-face)
   :help-echo "搜索QingEditor的内置文档"
   :action (lambda (&rest ignore) ())
   :mouse-face 'highlight
   :follow-link "\C-m")

  (qingeditor/ui/editor-drawer/center-current-line)
  (insert "\n\n"))

(defun qingeditor/ui/editor-drawer/center-current-line ()
  "将光标定位当当前行的居中位置。"
  (let* ((width (current-column))
         (margin (max 0 (floor (/ (- qingeditor/ui/editor-drawer/banner-length-private  width) 2)))))
    (beginning-of-line)
    (insert (make-string margin ?\ ))
    (end-of-line)))

(defun qingeditor/ui/editor-drawer/insert-shortcut (shortcut-char search-label
                                &optional no-next-line)
  `(define-key qingeditor-buffer-mode-map
     ,shortcut-char
     (lambda ()
       (interactive)
       (unless (search-forward ,search-label (point-label) t)
         (search-backward ,search-label (point-min) t))
       ,@(unless no-next-line
           '((forward-line 1)))
       (back-to-indentation))))

(defun qingeditor/ui/editor-drawer/inject-version ()
  "在qingeditor buffer的顶行的最右边插入当前的版本号。"
  (with-current-buffer (get-buffer-create qingeditor/ui/editor/buffer-name)
    (save-excursion
      (let ((maxcol qingeditor/ui/editor-drawer/banner-length-private)
            (version (format "%s@%s (%s)"
                             qingeditor/version
                             emacs-version
                             qingeditor/core/user-cfg/distribution))
            (buffer-read-only nil))
        (unless (display-graphic-p)
          (setq maxcol (1- maxcol)))
        (goto-char (point-min))
        ;; 删除首行里面的内容
        (delete-region (point) (progn (end-of-line) (point)))
        ;; 前两个%是为了转义形成一个结果%第三个%d输出一个整数
        (insert (format (format "%%%ds" maxcol) version))))))

(defun qingeditor/ui/editor-drawer/render-image-banner (banner)
  "在QingEditor buffer中插入一张图片。在这个时候不用按照按照窗口宽度进行计算，到时候咱们统一的进行计算。"
  (when (file-exists-p banner)
    (let* ((title qingeditor/ui/editor/buffer-logo-title)
           (spec (create-image banner))
           (size (image-size spec))
           (width (car size))
           (left-margin (max 0 (floor (- qingeditor/ui/editor-drawer/banner-length-private width) 2))))
      (goto-char (point-min))
      (insert "\n")
      (insert (make-string left-margin ?\ ))
      (insert-image spec)
      (insert "\n\n")
      (insert (make-string (max 0 (floor (/ (- qingeditor/ui/editor-drawer/banner-length-private
                                               (length title)) 2))) ?\ ))
      (insert (format "%s\n\n" title)))))

(defun qingeditor/ui/editor-drawer/render-ascii-banner-centered (file)
  "居中插入一个Ascii类型的banner。"
  (insert-string
   (with-temp-buffer
     (insert-file-contents file)
     ;; 找出最长的行
     (let ((banner-width 0))
       (while (not (eobp))
         (let ((line-length (- (line-end-position) (line-beginning-position))))
           (if (< banner-width line-length)
               (setq banner-width line-length)))
         (forward-line 1))
       (goto-char 0)
       (let ((margin (max 0 (floor (/ (- qingeditor/ui/editor-drawer/banner-length-private banner-width) 2)))))
         (while (not (eobp))
           (insert (make-string margin ?\ ))
           (forward-line 1))))
     (buffer-string))))

(defun qingeditor/ui/editor-drawer/choose-banner ()
  "根据`qingeditor/core/user-cfg/startup-banner'的配置信息选择banner图片。"
  (let ((startup-banner qingeditor/core/user-cfg/startup-banner))
    (when startup-banner
      (cond ((eq 'official startup-banner)
             (if (and (display-graphic-p) (image-type-available-p 'png))
                qingeditor/ui/editor/official-banner-png
               (qingeditor/ui/editor-drawer/get-text-banner-path 5)))
            ((eq 'random startup-banner)
             (qingeditor/ui/editor-drawer/choose-random-text-banner))
            ((eq 'random startup-banner)
             (qingeditor/ui/editor-drawer/choose-random-text-banner t))
            ((eq 'doge startup-banner)
             (qingeditor/ui/editor-drawer/get-text-banner-path 999))
            ((eq 'cat startup-banner)
             (qingeditor/ui/editor-drawer/get-text-banner-path 998))
            ((integerp startup-banner)
             (qingeditor/ui/editor-drawer/get-text-banner-path startup-banner))
            ((and startup-banner
                  (image-type-available-p (intern (file-name-extension startup-banner)))
                  (display-graphic-p))
             (if (file-exists-p startup-banner)
                 startup-banner
               (qingeditor/core/io/warning (format "banner file %s is not exist。" startup-banner))
               (qingeditor/ui/editor-drawer/get-text-banner-path 5)))
            (t (qingeditor/ui/editor-drawer/get-text-banner-path 5))))))

(defun qingeditor/ui/editor-drawer/get-text-banner-path (index)
  "获取Ascii类型的banner文件。"
  (concat qingeditor/banner-dir (format "%03d-banner.txt" index)))

(defun qingeditor/ui/editor-drawer/choose-random-text-banner (&optional all)
  "随机选择Ascii类型的banner文件。如果`all'不为`nil'从所有的Ascii文件中进行选择，否则去掉最后两个。"
  (let ((random-banner qingeditor/ui/editor-drawer/random-banner-private))
    (or random-banner
        (let* ((files (directory-files qingeditor/banner-dir t ".*\.txt"))
               (count (length files))
               ;; -2 删除最后两个文件 (easter eggs)
               (choice (random (- count (if all 0 2)))))
          (nth choice files)))))

(defun qingeditor/ui/editor-drawer/display-info-box ()
  "在启动界面绘制一个显示信息的框。"
  (when (file-exists-p qingeditor/ui/editor-drawer/cache-file-private)
    (load qingeditor/ui/editor-drawer/cache-file-private)
    (when (boundp 'qingeditor/ui/editor-drawer/var-export/release-note-version)
      (setq qingeditor/ui/editor-drawer/release-note-version-private
            qingeditor/ui/editor-drawer/var-export/release-note-version)))
  (cond
   (qingeditor/core/runtime/fresh-install
    ;; 首先假设用户是全新安装,我们默认打开信息显示框
    (qingeditor/ui/editor-drawer/toggle-note (concat qingeditor/info-dir "quickhelp.txt")
                     (qingeditor/ui/editor-drawer/insert-note-p 'quickhelp)))
   ((or (not qingeditor/ui/editor-drawer/release-note-version-private)
        (version< qingeditor/ui/editor-drawer/release-note-version-private
                  qingeditor-version))
    ;; 通过检查当前的`relase-note-version'决定是否显示发行说明。
    (qingeditor/ui/editor-drawer/toggle-note
     (concat qingeditor/release-notes-dir
             qingeditor/ui/editor/ui-version-info ".txt") 'release-note)))
  (qingeditor/ui/editor/redisplay))

(defun qingeditor/ui/editor-drawer/insert-release-note-widget (file)
  "将保存在文件里面的发型说明插入到信息框中。"
  (qingeditor/ui/editor-drawer/remove-existing-widget-if-exist)
  (let ((widget-func
         (lambda ()
           (add-to-list
            'qingeditor/ui/editor-drawer/note-widgets-private
            (widget-create 'push-button
                           :tag (propertize "点击查看详细更改历史" 'face 'font-lock-warning-face)
                           :help-echo "打开详细更改历史"
                           :action (lambda (&rest ignore)
                                     (funcall 'qingeditor/core/view-org-file
                                              (concat qingeditor/start-dir
                                                      "CHANGELOG.org")
                                              (format "Release %s.x" qingeditor/ui/editor/ui-version-info)
                                              'subtree))
                           :mouse-face 'highlight
                           :follow-link "\C-m")))))
    (qingeditor/ui/editor-drawer/insert-note file
                     (format " 重要的升级记录 (版本：%s.x) "
                             qingeditor/ui/editor/ui-version-info)
                     widget-func))
  (setq qingeditor/ui/editor-drawer/release-note-version-private nil)
  (setq qingeditor/ui/editor-drawer/var-export/release-note-version nil)
  (qingeditor/core/dump-vars-to-file '(qingeditor/ui/editor-drawer/var-export/release-note-version)
                              qingeditor/ui/editor-drawer/cache-file-private)
  (setq qingeditor/ui/editor-drawer/previous-insert-type-private 'release-note))

(defun qingeditor/ui/editor-drawer/insert-note-p (type)
  "根据传入的`type'来决定是否需要插入note小组件。

如果note的类型是`quickhelp'或者`release-note'并且类型与`qingeditor/ui/editor-drawer::previous-insert-type'相同
我们在这里简单的删除小组件而不去插入到`qingeditor buffer'里面。否则我们先删除老的组件然后插入
新的组件。"
  (if (not (eq qingeditor/ui/editor-drawer/previous-insert-type-private type))
      type
    (setq qingeditor/ui/editor-drawer/previous-insert-type-private nil)))

(defun qingeditor/ui/editor-drawer/toggle-note (file type)
  "根据`type'来显示在文件`file'里面的信息。

如果`type'为`nil'，函数将删除显示小组件。"
  (qingeditor/ui/editor-drawer/remove-existing-widget-if-exist)
  (cond
   ((eq type 'quickhelp)
    (qingeditor/ui/editor-drawer/insert-quickhelp-widget file))
   ((eq type 'release-note)
    (qingeditor/ui/editor-drawer/insert-release-note-widget file))
   (t)))

(defun qingeditor/ui/editor-drawer/remove-existing-widget-if-exist ()
  "删除存在的note小组件。"
  (when qingeditor/ui/editor-drawer/note-widgets-private
    (qingeditor/ui/editor-drawer/remove-note-widgets)))

(defun qingeditor/ui/editor-drawer/remove-note-widgets ()
  (mapc 'widget-delete qingeditor/ui/editor-drawer/note-widgets-private)
  (setq qingeditor/ui/editor-drawer/note-widgets-private nil)
  (setq qingeditor/ui/editor-drawer/release-note-version-private qingeditor-version)
  (setq qingeditor/ui/editor-drawer/var-export/release-note-version qingeditor-version)
  (qingeditor/core/dump-vars-to-file '(qingeditor/ui/editor-drawer/var-export/release-note-version)
                              qingeditor/ui/editor-drawer/cache-file-private))

(defun qingeditor/ui/editor-drawer/insert-quickhelp-widget (file)
  "将指定的文件`file'加入的快速帮助信息框中。"
  (qingeditor/ui/editor-drawer/remove-existing-widget-if-exist)
  (let ((widget-func
         (lambda ()
           (add-to-list
            'qingeditor/ui/editor-drawer/note-widgets-private
            (widget-create 'push-button
                           :tag (propertize "Emacs简易教程" 'face 'font-lock-keyword-face)
                           :help-echo "教您如何使用Emacs。"
                           :action (lambda (&rest ignore)
                                     (call-interactively #'help-with-tutorial))
                           :mouse-face 'highlight
                           :follow-link "\C-m"))
           (widget-insert " ")
           (add-to-list
            'qingeditor/ui/editor-drawer/note-widgets-private
            (widget-create 'push-button
                           :tag (propertize "Evil简易教程"
                                            'face 'font-lock-keyword-face)
                           :help-echo "教您如何使用Vim。"
                           :action (lambda (&rest ignore)
                                     (call-interactively #'evil-tutor-start))
                           :mouse-face 'highlight
                           :follow-link "\C-m"))
           (widget-insert " ")
           (add-to-list
            'qingeditor/ui/editor-drawer/note-widgets-private
            (widget-create 'push-button
                           :tag (propertize "Vim升级指南" 'face 'font-lock-keyword-face)
                           :help-echo "送给Vim转Emacs的用户的差异文档"
                           :action (lambda (&rest ignore)
                                     (qingeditor/core/view-org-file
                                      (concat qingeditor/docs-dir "VIMUSERS.org") "^" 'all))
                           :mouse-face 'highlight
                           :follow-link "\C-m")))))
    (qingeditor/ui/editor-drawer/insert-note file "快速入门" widget-func))
  (setq qingeditor/ui/editor-drawer/previous-insert-type-private 'quickhelp))

(defun qingeditor/ui/editor-drawer/insert-note (file caption &optional additional-widgets)
  "在banner下面插入发行说明。

`file'的内容是将要插入的内容，`title'是信息框的标题。"
  (save-excursion
    (goto-char (point-min))
    (search-forward "搜索`QingEditor'内置文档\]")
    (forward-line)
    (let* ((note (concat "\n" (qingeditor/ui/editor-drawer/render-framed-text
                               file qingeditor/ui/editor-drawer/banner-length-private caption))))
      (add-to-list 'qingeditor/ui/editor-drawer/note-widgets-private (widget-create 'text note))
      (save-excursion
        (while (re-search-backward "\\[\\[\\(.*\\)\\]\\]" nil t)
          (let ((buffer-read-only nil))
            (make-text-button
             (match-beginning 1)
             (match-end 1)
             'type 'help-url
             'help-args (list (match-string 1))))))
      (funcall additional-widgets))))

(defun qingeditor/ui/editor-drawer/render-framed-text (content &optional width caption hpadding)
  "将`content'指定的字符串周围绘制一个包裹的边框。

`content'可以是字符串，也可以是一个文本文件的路径，`width'的值将会设置`fill-column'变量。
如果`caption'不为`nil'的话，将显示在边框的顶部如果`caption'的长度大于`fill-column'的
长度减去5是异常情况，函数返回`nil'。"
  (with-temp-buffer
    (if (not (file-exists-p content))
        (insert content)
      (insert-file-contents content)
      ;; 删除文件末尾多余的换行符号
      (goto-char (point-max))
      (delete-char -1))
    (let* ((hpadding (or hpadding 1))
           (fill-column (if width
                            (- width (+ 3 (* 2 hpadding)))
                          fill-column))
           (sentence-end-double-space nil)
           (caption-len (if (display-graphic-p) (+ (length caption) 1) (+ (length caption) 4))))
      (fill-region (point-min) (point-max) 'justify 'nosqueeze)
      (concat
       ;; 顶部
       "─"
       (if caption
           (concat caption
                   (make-string 5 ?─))
         (make-string fill-column ?─))
       (make-string hpadding ?─) "\n"
       ;; 内容添加
       (qingeditor/ui/editor-drawer/render-framed-line "" hpadding)
       (mapconcat (lambda (line-text)4
                    (qingeditor/ui/editor-drawer/render-framed-line line-text hpadding))
                  (split-string (buffer-string) "\n" nil) "")
       (qingeditor/ui/editor-drawer/render-framed-line "" hpadding)
       ;; 底部
       "╰" (make-string hpadding ?─)
       (make-string fill-column ?─)
       (make-string hpadding ?─)
       "╯"))))

(defun qingeditor/ui/editor-drawer/render-framed-line (line hpadding)
  "返回一个两个带边框的字符串，`hpadding'代表字符串与边框之间的距离，如果`line'的
长度大于`fill-column'的宽度函数返回`nil'。"
  (let* ((len (length line)))
    (concat (make-string hpadding ?\s)
            line
            (make-string hpadding ?\s) "\n")))

(defun qingeditor/ui/editor-drawer/display-summary (start-time)
  "显示一个加载统计信息。"
  (unless qingeditor/ui/editor/startup-time
    (setq qingeditor/ui/editor/startup-time
          (float-time (time-subtract (current-time) qingeditor/start-time))))
  (let ((stats (qingeditor/layer/layer/configured-packages-stats
                qingeditor/layer/layer/used-packages-private)))
    (qingeditor/ui/editor/insert-page-break)
    (qingeditor/ui/editor/buffer-append
     (format "\n共加载%s个软件包，耗时：%.3s秒 (e:%s r:%s l:%s b:%s)"
             (cadr (assq 'total stats))
             qingeditor/ui/editor/startup-time
             (cadr (assq 'elpa stats))
             (cadr (assq 'recipe stats))
             (cadr (assq 'local stats))
             (cadr (assq 'built-in stats))))
    (with-current-buffer (get-buffer-create (qingeditor/ui/editor/buffer))
      (let ((buffer-read-only nil))
        (qingeditor/ui/editor-drawer/center-current-line)
        (insert "\n")))))

(provide 'qingeditor-editor-drawer)
