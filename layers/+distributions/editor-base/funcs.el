;; `qingeditor'基本配置层layer中使用的函数定义
(require 'cl-lib)

(defun qingeditor/editor-base/run-local-vars-mode-hook ()
  "当`major mode'的局部变量处理完成，运行hook钩子函数。"
  (run-hooks (intern (format "qingeditor/editor-base/%S-local-vars-hook" major-mode))))

(defun qingeditor/editor-base/split-and-new-line ()
  "分割一个`quoted'字符串或者`s-expression'并且加入一个换行符并且自动缩进。"
  (interactive)
  (sp-splice-sexp 1)
  (sp-newline))

(defun qingeditor/editor-base/push-mark-and-goto-beginning-of-line ()
  "保存当前的位置然后将光标移动到行首。"
  (interactive)
  (push-mark (point))
  (evil-beginning-of-line))

(defun qingeditor/editor-base/push-mark-and-goto-end-of-line ()
  "保存房钱的位置然后将光标移动到行尾。"
  (interactive)
  (push-mark (point))
  (evil-end-of-line))

(defun qingeditor/editor-base/evil-insert-line-above (count)
  "在当前点的行之上插入一行或者多行并且不改变当前的状态和点的位置。"
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun qingeditor/editor-base/evil-insert-line-below (count)
  "在当前点的行之下插入一行或者多行并且不改变当前的状态和点的位置"
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(defun qingeditor/editor-base/new-empty-buffer ()
  "创建一个全新的`buffer'，默认的名称是`untitled<n>'。"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

(defun qingeditor/editor-base/evil-goto-next-line-and-indent (&optional count)
  (interactive "p")
  (let ((counter (or count 1)))
    (while (> counter 0)
      (join-line 1)
      (newline-and-indent)
      (setq counter (1- counter)))))

;; from Prelude
;; TODO: dispatch these in the layers

(defvar qingeditor/editor-base/indent-sensitive-modes
  '(coffee-mode
    elm-mode
    haml-mode
    haskell-mode
    slim-mode
    makefile-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    python-mode
    yaml-mode)
  "禁止自动缩进的`mode'名称。")

(defcustom qingeditor/editor-base/yank-indent-threshold 1000
  "自动缩进不自动发生的阈值。"
  :type 'number
  :group 'qingeditor)

(defcustom qingeditor/editor-base/large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                 doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                 pdf-view-mode)
  "大文件阈值不检查的符号列表。"
  :group 'qingeditor
  :type '(list symbol))

(defun qingeditor/editor-base/indent-region-or-buffer ()
  "缩进选中区域如果没有选中就缩进整个`buffer'。"
  (interactive)
  (unless (member major-mode qingeditor/editor-base/indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (qingeditor/core/message "缩进选中的区域。"))
        (progn
          (evil-indent (point-min) (point-max))
          (qingeditor/core/message "缩进整个`buffer'。")))
      (whitespace-cleanup))))

;; from https://gist.github.com/3402786
(defun qingeditor/editor-base/toggle-maximize-buffer ()
  "最大化`buffer'。"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

;; https://tsdh.wordpress.com/2007/03/28/deleting-windows-vertically-or-horizontally/
(defun qingeditor/editor-base/maximize-horizontally ()
  "删除当前窗口左边或者右边的窗口。"
  (interactive)
  (require 'windows)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun qingeditor/editor-base/toggle-centered-buffer-mode ()
  "切换`qingeditor/editor-base/centered-buffer-mode'。"
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (call-interactively 'qingeditor/editor-base/centered-buffer-mode)))

(defun qingeditor/editor-base/centered-buffer-mode-full-width ()
  "在一个`frame'中居中一个`buffer'。"
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (qingeditor/editor-base/maximize-horizontally)
    (call-interactively 'qingeditor/editor-base/centered-buffer-mode)))

(defun qingeditor/editor-base/useful-buffer-p (buffer)
  "判断一个`buffer'是否有用。"
  (let ((buf-name (buffer-name buffer)))
    (or (with-current-buffer buffer
          (derived-mode-p 'comint-mode))
        (cl-loop for useful-regexp in qingeditor/editor-base/useful-buffers-regexp
                 thereis (string-match-p useful-regexp buf-name))
        (cl-loop for useless-regexp in qingeditor/editor-base/useless-buffers-regexp
                 never (string-match-p useless-regexp buf-name)))))

(defun qingeditor/editor-base/useless-buffer-p (buffer)
  "判断一个`buffer'是否无用。"
  (not (qingeditor/editor-base/usefull-buffer-p buffer)))

(defun qingeditor/editor-base/rotate-windows (count)
  "以此的旋转窗口，锁定的窗口不进行选装。"
  (interactive "p")
  (let* ((non-dedicated-windows (remove-if 'window-dedicated-p (window-list)))
         (num-windows (length non-dedicated-windows))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (qingeditor/core/message "您不能旋转单个窗口!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))
                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))
                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))
                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))

(defun qingeditor/editor-base/rotate-windows-backward (count)
  "依次向下旋转窗口，不看锁定的窗口。"
  (interactive "p")
  (qingeditor/editor-base/rotate-windows (* -1 count)))

(defun qingeditor/editor-base/rename-file (filename &optional new-filename)
  "重命名一个文件名称。

如果没有指定`new-filename'，会要求用户进行输入。如果存在`buffer'管理这个文件，那么我们同时会
改变`buffer'的名称，然后让`projectile'缓存失效然后更新`recentf'列表。"
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let* ((buffer (find-buffer-visiting filename))
           (short-name (file-name-nondirectory filename))
           (new-name (if new-filename new-filename
                       (read-file-name
                        (format "重命名%s到: " short-name)))))
      (cond ((get-buffer new-name)
             (error "`buffer': %s已经存在！" new-name))
            (t
             (let ((dir (file-name-directory new-name)))
               (when (and (not (file-exists-p dir))
                          (yes-or-no-p (format "创建文件夹:%s ?" dir)))
                 (make-directory dir t)))
             (rename-file filename new-name 1)
             (when buffer
               (kill-buffer)
               (find-file new-name))
             (when (fboundp 'recentf-add-file)
               (recentf-add-file new-name)
               (recentf-remove-if-non-kept filename))
             (when (and (qingeditor/layer/layer/package-usedp 'projectile)
                        (projectile-project-p))
               (call-interactively #'projectile-invalidate-cache))
             (qingeditor/core/message "文件'%s'成功重命名为'%s'" short-name (file-name-nondirectory new-name)))))))

(defun qingeditor/editor-base/rename-current-buffer-file ()
  "重命名当前的`buffer'和他关联的文件的名称。"
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir (file-name-directory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "当前的`buffer':%s没有关联文件！" name)
      (let ((new-name (read-file-name "新的文件名: " dir)))
        (cond ((get-buffer new-name)
               (error "新的`buffer'名称: %s已经存在！" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "创建文件夹：%s?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (when (and (qingeditor/layer/layer/package-usedp 'projectile)
                          (projectile-project-p))
                 (call-interactively #'projectile-invalidate-cache))
               (qingeditor/core/message "`buffer': %s成功重命名为%s" name (file-name-nondirectory new-name))))))))

(defun qingeditor/editor-base/delete-file (filename &optional ask-user)
  "删除指定的文件或者文件夹。

同时会删除文件关联的文件并且让`projectile'缓存失效。如果`ask-user'不为`nil'
将询问用户是否进行删除。"
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "您确定要删除这个文件？"))
      (delete-file filename)
      (when (and (qingeditor/layer/layer/package-usedp 'projectile)
                 (projectile-project-p))
        (call-interactively #'projectile-invalidate-cache))
      (qingeditor/core/message "文件: %s成功删除!" filename))))

(defun qingeditor/editor-base/delete-current-buffer-file ()
  "删除当前`buffer'关联的文件和`buffer'本身。"
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "您确定要删除这个文件？")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (and (qingeditor/layer/layer/package-usedp 'projectile)
                   (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))
        (qingeditor/core/message "文件: %s成功删除成功！" filename)))))

(defun qingeditor/editor-base/sudo-edit (&optional arg)
  (interactive "p")
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "文件：")
                 buffer-file-name)))
    (find-file
     (cond ((string-match-p "^/ssh:" fname)
            (with-temp-buffer
              (insert fname)
              (search-backward ":")
              (let ((last-match-end nil)
                    (last-ssh-hostname nil))
                (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                  (setq last-ssh-hostname (or (match-string 1 fname)
                                              last-ssh-hostname))
                  (setq last-match-end (match-end 0)))
                (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
              (buffer-string)))
           (t (concat "/sudo:root@localhost:" fname))))))

(defun qingeditor/editor-base/check-large-file ()
  "检查文件的大小，如果太大我们使用`fundamental'的方式进行打开。"
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and
           (not (memq major-mode qingeditor/editor-base/large-file-modes-list))
           size (> size (* 1024 1024 qingeditor/core/user-cfg/large-file-size))
           (y-or-n-p (format (concat "文件：%s太大，将不使用`literally'方式打开"
                                     "避免性能问题？")
                             filename)))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(defun qingeditor/editor-base/delete-window (&optional arg)
  "删除当前的窗口。
如果通用的前缀参数指定了那么我们同时删除`buffer'。"
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

(defun qingeditor/editor-base/ace-delete-window (&optional arg)
  "删除Ace窗口。

如果通用的前缀参数指定了那么我们同时删除`buffer'。"
  (interactive "P")
  (require 'ace-window)
  (aw-select
   " Ace - 删除窗口"
   (lambda (window)
     (when (equal '(4) arg)
       (with-selected-window window
         (qingeditor/editor-base/kill-this-buffer arg)))
     (aw-delete-window window))))

(defun qingeditor/editor-base/kill-this-buffer (&optional arg)
  "删除当前的`buffer'。
如果通用的前缀参数指定了那么我们同时删除`buffer'关联的`window'。"
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun qingeditor/editor-base/ace-kill-this-buffer (&optional arg)
  (interactive "P")
  (require 'ace-window)
  (let (golden-ratio-mode)
    (aw-select
     " Ace - 删除窗口中的`buffer'。"
     (lambda (window)
       (with-selected-window window
         (qingeditor/editor-base/kill-this-buffer arg))))))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun qingeditor/editor-base/kill-other-buffers (&optional arg)
  "删除其他的`buffer'。
如果通用的前缀参数指定了那么我们同时删除`buffer'关联的`window'。"
  (interactive "P")
  (when (yes-or-no-p (format "您确定要删除除了`buffer':\"%s\"之外的所有的`buffer'吗？"
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (qingeditor/core/message "`buffer'成功删除！")))

;; 借鉴http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun qingeditor/editor-base/toggle-current-window-dedication ()
  "翻转窗口的`dedication'状态。"
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (qingeditor/core/message "窗口%s`dedicated'到%s"
                      (if dedicated "不再" "")
                      (buffer-name))))

;; 借鉴 http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun qingeditor/editor-base/show-and-copy-buffer-filename ()
  "复制当前`buffer'文件的路径并且在`minibuffer'中进行显示。"
  (interactive)
  ;; `list-buffers-directory'在`dired buffer'进行了设置
  (let ((filename (or (buffer-file-name) list-buffers-directory)))
    (if filename
        (qingeditor/core/message (kill-new filename))
      (error "当前的`buffer'没有访问任何文件。"))))

;; http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun qingeditor/editor-base/find-user-init-file ()
  "在当前的窗口，编辑`user-init-file'变量指定的文件"
  (interactive)
  (find-file-existing user-init-file))

(defun qingeditor/editor-base/find-dotfile ()
  "在当前的窗口编辑`dotfile'。"
  (interactive)
  (find-file-existing (qingeditor/core/user-cfg/user-cfg-filename)))

(defun qingeditor/editor-base/ediff-dotfile-and-template ()
  "ediff比价`dotfile'和模板配置之间的差别。"
  (interactive)
  (ediff-files (qingeditor/core/user-cfg/user-cfg-filename)
               (concat qingeditor/template-dir ".qingeditor.template")))

;; 借鉴 https://gist.github.com/timcharper/493269
(defun qingeditor/editor-base/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun qingeditor/editor-base/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun qingeditor/editor-base/layout-triple-columns ()
  "将布局变成3列。"
  (interactive)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

(defun qingeditor/editor-base/layout-double-columns ()
  "将布局编程2列。"
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun qingeditor/editor-base/insert-line-above-no-indent (count)
  "在当前行的上面插入一行，但是不进行缩进。"
  (interactive "p")
  (let ((p (+ (point) count)))
    (save-excursion
      (if (eq (line-number-at-pos) 1)
          (evil-move-beginning-of-line)
        (progn
          (evil-previous-line)
          (evil-move-end-of-line)))
      (while (> count 0)
        (insert "\n")
        (setq count (1- count))))
    (goto-char p)))

(defun qingeditor/editor-base/insert-line-below-no-indent (count)
  "在当前行的下面插入一行，但是不进行缩进。"
  (interactive "p")
  (save-excursion
    (evil-move-end-of-line)
    (while (> count 0)
      (insert "\n")
      (setq count (1- count)))))

;; from https://github.com/gempesaw/dotemacs/blob/emacs/dg-defun.el
(defun qingeditor/editor-base/kill-matching-buffer-rudely (regexp &optional internal-too)
  "删除名字能够匹配的`regexp'正则表达式的`buffer'。这个函数不像内置的`kill-matching-buffers'，删除
`buffer'的时候不进行相关的询问。如果`internal-too'不为`nil'那么我们将同时删除内部的`buffer'。"
  (interactive "s删除匹配指定正则表达式的buffers: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))

(defvar qingeditor/editor-base/really-kill-emacs nil
  "阻止窗口管理器因为实例关闭而关闭。")

(defun qingeditor/editor-base/persistent-server-running-p ()
  "需要`qingeditor/editor-base/really-kill-emacs'处于打开状态并且`qingeditor/editor-base'设置成`t'"
  (and (fboundp 'server-running-p)
       (server-running-p)
       qingeditor/core/user-cfg/persistent-server))

(defadvice kill-emacs (around qingeditor/editor-base/really-exit activate)
  "只有在`prefix'设置了才去关闭emacs。"
  (if (and (not qingeditor/editor-base/really-kill-emacs)
           (qingeditor/editor-base/persistent-server-running-p))
      (qingeditor/editor-base/frame-killer)
    ad-do-it))

(defadvice save-buffers-kill-emacs (around qingeditor/editor-base-really-exit activate)
  "只有在`prefix'设置了才去关闭emacs。"
  (if (or qingeditor/editor-base/really-kill-emacs (not qingeditor/core/user-cfg/persistent-server))
      ad-do-it
    (qingeditor/editor-base/frame-killer)))

(defun qingeditor/editor-base/kill-emacs ()
  "丢弃所有的改变然后退出`emacs'。"
  (interactive)
  (setq qingeditor/editor-base/really-kill-emacs t)
  (save-buffers-kill-emacs))

(defun qingeditor/editor-base/prompt-kill-emacs ()
  "显示一个提示框，询问用户是否对`buffer'进行保存然后退出。"
  (interactive)
  (setq qingeditor/editor-base/really-kill-emacs t)
  (save-some-buffers)
  (kill-emacs))

(defun qingeditor/editor-base/frame-killer ()
  "删除`server buffer'然后隐藏主要的`Emacs'窗口。"
  (interactive)
  (condition-case-unless-debug nil
      (delete-frame nil 1)
    (error
     (make-frame-invisible nil 1))))

(defun qingeditor/editor-base/toggle-frame-fullscreen ()
  "在切换是否全屏的时候考虑`qingeditor/core/user-cfg/fullscreen-use-non-active'变量的值。"
  (interactive)
  (if qingeditor/core/user-cfg/fullscreen-use-non-native
      (qingeditor/editor-base/toggle-frame-fullscreen-non-native)
    (toggle-frame-fullscreen)))

(defun qingeditor/editor-base/toggle-fullscreen ()
  "在`X11'和`Carbon'切换全屏模式。"
  (interactive)
  (cond
   ((eq window-system 'x)
    (set-frame-parameter nil 'fullscreen
                         (when (not (frame-parameter nil 'fullscreen))
                           'fullboth)))
   ((eq window-system 'mac)
    (set-frame-parameter nil 'fullscreen
                         (when (not (frame-parameter nil 'fullscreen))
                           'fullscreen)))))

(defun qingeditor/editor-base/toggle-frame-fullscreen-non-native ()
  "使用非原生接口切换窗口最大化状态，使用`fullboth'的`frame'参数替代`fullscreen'。
在OSX w/o动画很有用。"
  (interactive)
  (modify-frame-parameters
   nil
   `((maximized
      . ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
           (frame-parameter nil 'fullscreen)))
     (fullscreen
      . ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
             (if (eq (frame-parameter nil 'maximized) 'maximized)
                 'maximized)
           'fullboth)))))

;; taken from Prelude: https://github.com/bbatsov/prelude
(defmacro qingeditor/editor-base/advise-commands (advice-name commands class &rest body)
  "批量生成`defadvice'，应用`advice-name'到多个`commands'
`advide'的表达式体是`body'。"
  `(porgn
    ,@(mapcar (lambda (command)
                `(defadvice ,command
                     (,class ,(intern (format "%S-%s" command advice-name))
                             activate)
                   ,@body))
              commands)))


(defun qingeditor/editor-base/safe-revert-buffer ()
  "在恢复文件之前对用户进行询问。"
  (interactive)
  (revert-buffer nil nil))

(defun qingeditor/editor-base/safe-erase-buffer ()
  "当删除`buffer'内容时候询问用户。"
  (interactive)
  (if (y-or-n-p (format "是否删除`buffer' %s的内容？" (current-buffer)))
      (erase-buffer)))

(defun qingeditor/editor-base/find-ert-test-buffer (ert-test)
  "返回定义`ERT-TEST'的`buffer'。"
  (car (find-definition-noselect (crt-test-name ert-test) 'ert-deftest)))

(defun qingeditor/editor-base/ert-run-tests-buffer ()
  "运行当前`buffer'里面所有的测试。"
  (interactive)
  (save-buffer)
  (load-file (buffer-file-name))
  (let ((cbuf (current-buffer)))
    (ert '(satisfies
           (lambda (test)
             (eq cbuf (qingeditor/editor-base/find-ert-test-buffer test)))))))

(defun qingeditor/editor-base/open-in-external-app (file-path)
  "打开`file-path'变量指定的外部程序。"
  (cond
   ((qingeditor/runtime/system-is-mswindows)
    (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
   ((qingeditor/runtime/system-is-mac) (shell-command (format "open \"%s\"" file-path)))
   ((qingeditor/runtime/system-is-linux)
    (let ((process-connection-type nil))
      (start-process "" nil "xdg-open" file-path)))))

(defun qingeditor/editor-base/open-file-or-directory-in-external-app (arg)
  "在外部程序中打开当前文件，如果前缀参数存在，那么在文件管理器中打开当前文件所在的文件夹。"
  (if arg
      (qingeditor/editor-base/open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (qingeditor/editor-base/open-in-external-app file-path)
        (qingeditor/core/message "当前的`buffer'没有关联任何文件。")))))

(defun qingeditor/editor-base/switch-to-minibuffer-window ()
  "跳转到`minibuffer'窗口。"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;; http://stackoverflow.com/a/10216338/4869
(defun qingeditor/editor-base/copy-whole-buffer-to-clipboard ()
  "复制整个`buffer'的内容到剪切板。"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun qingeditor/editor-base/copy-clipboard-to-whole-buffer ()
  "复制整个剪切板的内容到整个`buffer'。"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;; 对齐函数开始

;; 借鉴 http://emacswiki.org/emacs/AlignCommands
(defun qingeditor/editor-base/align-repeat (start end regexp &optional justify-right after)
  "参照给定的`regexp'进行对齐，如果`justify-right'不为空的话，向右对齐，如果`after'不为`nil'，将
在左边插入空格。"
  (interactive "r\ns对齐正则：")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))
    (qingeditor/core/message "%S" complete-regexp)
    (align-regexp start end complete-regexp group 1 t)))

;; 借鉴 http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun qingeditor/editor-base/align-repeat-decimal (start end)
  "对齐一个表格。"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro qingeditor/editor-base/create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "qingeditor/editor-base/align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (qingeditor/editor-base/aligin-repeat start end ,regexp ,justify-right after)))))

(qingeditor/editor-base/create-align-repeat-x "comma" "," nil t)
(qingeditor/editor-base/create-align-repeat-x "semicolon" ";" nil t)
(qingeditor/editor-base/create-align-repeat-x "colon" ":" nil t)
(qingeditor/editor-base/create-align-repeat-x "equal" "=")
(qingeditor/editor-base/create-align-repeat-x "math-oper" "[+\\-*/]")
(qingeditor/editor-base/create-align-repeat-x "ampersand" "&")
(qingeditor/editor-base/create-align-repeat-x "bar" "|")
(qingeditor/editor-base/create-align-repeat-x "left-paren" "(")
(qingeditor/editor-base/create-align-repeat-x "right-paren" ")" t)
(qingeditor/editor-base/create-align-repeat-x "backslash" "\\\\")

;; 对齐函数结束

(defun qingeditor/editor-base/dos2unix ()
  "转换当前的`buffer'格式从`dos'转换到`unix'文件格式。"
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun qingeditor/editor-base/unix2dos ()
  "转换当前的`buffer'格式从`unix'转换到`dos'文件格式。"
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun qingeditor/editor-base/copy-file ()
  "将当前的`buffer'保存到另外指定的文件。"
  (interactive)
  (call-interactively 'write-file))

(defun qingeditor/editor-base/uniquify-lines ()
  "移除`region'或者当前的`buffer'里面相邻的重复的行。"
  (interactive)
  (save-excursion
    (save-restriction
      (let ((beg (if (region-active-p) (region-beginning) (point-min)))
            (end (if (region-active-p) (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)1+" end t)
          (replace-match "\\1"))))))

(defun qingeditor/editor-base/sort-lines ()
  "给激活的`region'或者当前的`buffer'进行排序。"
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (sort-lines nil beg end)))

(defvar qingeditor/editor-base/linum-mdown-line nil
  "保存`linum'选区的持久化变量。")

(defun qingeditor/editor-base/line-at-click ()
  "获取鼠标点击的可视行的行号。"
  (save-excursion
    (let ((click-y (cddr (mouse-position)))
          (debug-on-error t)
          (line-move-visual t))
      (goto-char (window-start))
      (next-line (1- click-y))
      (1+ (line-number-at-pos)))))

(defun qingeditor/editor-base/md-select-linum (event)
  "将鼠标点击的点数据保存到`qingeditor/editor-base/linum-mdown-line'。"
  (interactive "e")
  (mouse-select-window event)
  (goto-line (qingeditor/editor-base/line-at-click))
  (set-mark (point))
  (setq qingeditor/editor-base/linum-mdown-line
        (line-number-at-pos)))

(defun qingeditor/editor-base/mu-select-linum ()
  "选择`point'和`qingeditor/editor-base/linum-mdown-line'之间的代码块。"
  (interactive)
  (when qingeditor/editor-base/linum-mdown-line
    (let (mul-line)
      (setq mul-line (qingeditor/editor-base/line-at-click))
      (goto-line (max qingeditor/editor-base/linum-mdown-line mul-line))
      (set-mark (line-end-position))
      (goto-line (min qingeditor/editor-base/linum-mdown-line mul-line))
      (setq qingeditor/editor-base/linum-mdown-line nil))))

(defun qingeditor/editor-base/select-current-block ()
  "选中两个各行之间的内容。"
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn
            (re-search-forward "\n[ \t]*\n" nil "move")
            (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn
            (re-search-backward "\n[ \t]*\n")
            (setq p2 (point)))
        (setq p2 (point))))
    (set-mark p1)))

;; 借鉴 http://xugx2007.blogspot.ca/2007/06/benjamin-rutts-emacs-c-development-tips.html
(setq compilation-finish-function
      (lambda (buf str)
        (if (or (string-match "exited abnormally" str)
                (string-match "FAILED" (buffer-name)))
            ;; 说明有错误产生
            (message "有错误发生。使用SPC-e-n进行查看。")
          (unless (or (string-match "Grep finished" (buffer-string))
                      (string-match "Ag finished" (buffer-string))
                      (string-match "nosetests" (buffer-name)))
            (message "编译完成。")))))

;; 借鉴 http://www.emacswiki.org/emacs/WordCount
(defun qingeditor/editor-base/count-word-analysis (start end)
  "统计单词在选中区域的出现的次数，忽略标点符号。"
  (interactive "r")
  (let (words alist-words-compare (formated ""))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (+1 (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (defun alist-words-compare (a b)
      "在`words'列表中比较两个元素，先按计数排序再按字母升序排序。"
      (let ((a-key (car a))
            (a-value (cdr a))
            (b-key (car b))
            (b-value (cdr b)))
        (if (eq a-value b-value)
            (string-lessp a-key b-key)
          (> a-value b-value))))

    (setq words (cl-sort words 'alist-words-compare))
    (while words
      (let* ((word (pop words))
             (name (car word))
             (count (cdr word)))
        (setq formated (concat (formated (format "[%s: %d], " name count))))))
    (when (interactive-p)
      (if (> (length formated) 2)
          (qingeditor/core/message (substring formated 0 -2))
        (message "没有找到单词。")))
    words))


