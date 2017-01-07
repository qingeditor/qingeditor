;; `qingeditor'的ELPA包管理器

(require 'package)
(require 'qingeditor-editor)
(require 'qingeditor-gvars)

(defvar qingeditor/pkg/installer/package-timeout nil
  "软件包元信息更新的间隔时间。")

(defvar qingeditor/pkg/installer/rollback-dir (concat qingeditor/cache-dir ".rollback/")
  "软件包回滚文件夹。")

(defvar qingeditor/pkg/installer/elpa-archives
  '(("popkit" . "elpa.popkit.org/packages/")
    ("org"   . "http://orgmode.org/elpa/")
    ("marmalade" . "marmalade-repo.org/packages/")
    ("gnu"   . "elpa.gnu.org/packages/")
    ("melpa" . "http://melpa.org/packages/"))
  "几个官方的`qingeditor'使用的ELPA软件包来源。")

(defvar qingeditor/pkg/installer/package-archives-refreshed-private nil
  "在本session里面判断ELPA源是否已经刷新的标志变量。")

(defvar qingeditor/pkg/installer/protected-packages nil
  "系统受保护的ELPA软件包，防止无意中删除列表中的软件包。")

(defun qingeditor/pkg/installer/get-package-installed-dir (pkg)
  "获取指定的`pkg'包的目标安装目录"
  (let ((elpa-dir (file-name-as-directory package-user-dir)))
    (when (file-exists-p elpa-dir)
      (let* ((pkg-match (concat "\\`" (symbol-name pkg) "-[0-9]+"))
             (dir (car (directory-files elpa-dir 'full pkg-match))))
        (when dir (file-name-as-directory dir))))))

(defun qingeditor/pkg/installer/load-or-install-package  (pkg &optional log file-to-load)
  "加载指定的ELPA软件包`pkg',如果指定的软件包不存在，那么系统将自动安装。如果`log'不为`nil'将在
qingeditoreditor buffer进行log的输出，如果`file-to-load'不为`nil'函数将在安装ELPA软件包成功之后自动加载
相应的软件包。"
  (let ((warning-minimum-level :error))
    (unless (require pkg nil :noerror)
      ;; 软件包不存在, 如果require失败，但是`pkg'存在于`elpa'的安装目录，那么我们在这里
      ;; 将其加入到`load-path'里面。
      (let ((pkg-elpa-dir (qingeditor/pkg/installer/get-package-installed-dir pkg)))
        (if pkg-elpa-dir
            (add-to-list 'load-path pkg-elpa-dir)
          (when log
            (qingeditor/ui/editor/append
             (format "(QingEditor 初始引导) 正在安装 %s ...\n" pkg))
            (qingeditor/ui/editor/redisplay))
	  (qingeditor/pkg/installer/retrieve-package-archives 'quiet)
          (package-install pkg)
          (setq pkg-elpa-dir (qingeditor/pkg/installer/get-package-installed-dir pkg)))
        (require pkg nil 'noerror)
        (when file-to-load
          (load-file (concat pkg-elpa-dir file-to-load)))
        pkg-elpa-dir))))

(defun qingeditor/pkg/installer/load-or-install-protected-package
  (pkg &optional log file-to-load)
  "安装或则加载`pkg'然后把这个package加入到保护列表，避免被删除。"
  (push pkg qingeditor/pkg/installer/protected-packages)
  (qingeditor/pkg/installer/load-or-install-package pkg log file-to-load))

(defun qingeditor/pkg/installer/retrieve-package-archives (&optional quiet force)
  "本函数会首先将在`package-archives'里面定义的ELPA源里面的元数据拉回本地缓存。我们首先会使用一个`GET'
请求，然后看在规定的时间里面是否有结果，如果有结果这个源就可用，避免的比较耗时的源刷新。
这个地方有个问题，`GET'请求只是一种启发式的探测，他不能避免服务器是好的，但是没有想要的ELPA包。

如果`quiet'不为`nil'那么将在`qingeditor buffer'里面输出一条信息

如果`force'不为空，那么函数会进行强制ELPA源刷新。"
  (unless (and qingeditor/pkg/installer/package-archives-refreshed-private
               (not force))
    (setq qingeditor/pkg/installer/package-archives-refreshed-private t)
    (let ((count (length package-archives))
          (i 1))
      (dolist (archive package-archives)
        (unless quiet
          (qingeditor/ui/editor/replace-last-line
           (format "--> 正在刷新ELPA源: %s... [%s/%s]"
                   (car archive) i count)))
        (qingeditor/ui/editor/redisplay)
        (setq i (1+ i))
        (unless
            (eq 'error (with-timeout
                           (qingeditor/core/user-cfg/elpa-timeout
                            (progn
                              (display-warning
                               'qingeditor
                               (format
                                "\n连接ELPA源：%s超时！" (car archive)) :warning)
                              'error))
                         ;; 开始探测
                         (condition-case err
                             (url-retrieve-synchronously (cdr archive))
                           ('error
                            (display-warning 'qingeditor
                                             (format
                                              "\n连接到ELPA软件源：%s的时候发生错误" (car archive)) :warning)
                            'error))))
          ;; 刷新ELPA软件源成功,这个地方我们暂时将package-archives
          (let ((package-archives (list archive)))
            (package-refresh-contents))))
      (package-read-all-archive-contents)
      (unless quiet (qingeditor/ui/editor/buffer-append "\n")))))

(defun qingeditor/pkg/installer/initialize ()
  "初始化 `package.el'。"
  (setq qingeditor/pkg/package-timeout qingeditor/core/user-cfg/elpa-timeout)
  (unless package--initialized
    (setq qingeditor/pkg/installer/rollback-dir
          (qingeditor/pkg/installer/get-elpa-dir qingeditor/pkg/installer/rollback-dir))
    (setq package-user-dir
          (qingeditor/pkg/installer/get-elpa-dir package-user-dir))
    (setq package-archives
          (qingeditor/pkg/installer/resolve-package-archives qingeditor/pkg/installer/elpa-archives))
    ;; 优化，不需要这么早获取所有的包
    (setq package-enable-at-startup nil)
    (package-initialize 'noactivate)))

(defun qingeditor/pkg/installer/get-elpa-dir (root)
  "根据`qingeditor/core/user-cfg::elpa-subdir'标志变量来确定elpa软件包的安装目录。"
  (let ((elpa-dir qingeditor/core/user-cfg/elpa-subdir))
    (if (not elpa-dir)
        root
      (let ((subdir (if (eq 'emacs-version elpa-dir)
                      (format "%d%s%d"
                              emacs-major-version
                              version-separator
                              emacs-minor-version)
                      (eval (elpa-dir)))))
        (file-name-as-directory (expand-file-name subdir root))))))

(defun qingeditor/pkg/installer/resolve-package-archives (archives)
  "探测所有的ELPA源是否可以访问，按照`package-archives'的格式返回所有可以访问的源列表，
如果在源的地址中包含协议，我们在探测的时候不操作地址。"
  (mapcar
   (lambda (x)
     (cons (car x)
           (if (or (string-match-p "http:" (cdr x))
                   (string-prefix-p "/" (cdr x)))
               (cdr x)
             (concat
              (if (and qingeditor/core/user-cfg/elpa-https
                       (not qingeditor/gvars/insecure))
                  "https://"
                "http://")
              (cdr x)))))
   archives))

(defun qingeditor/pkg/installer/install-packages (packages)
  "安装所有不是迟延加载的elpa软件包。"
  (let ((display-buffer-alist
         '(("\\(\\*Compile-Log\\*\\)\\|\\(\\*Warnings\\*\\)"
            (display-buffer-in-side-window)
            (inhibit-same-window . t)
            (side . bottom)
            (window-height . 0.2)))))
    ;; 保证quelpa可用
    (qingeditor/pkg/installer/install-quelpa)
    (let* ((upkg-names (qingeditor/pkg/installer/get-uninstalled-packages packages))
           (not-inst-count (length upkg-names))
           installed-count)
      ;; 开始安装相关的elpa软件包
      (when upkg-names
        (qingeditor/ui/editor/buffer-append
         (format "发现有%s个软件包需要安装...\n"
                 not-inst-count))
        (qingeditor/pkg/installer/retrieve-package-archives)
        (setq installed-count 0)
        (qingeditor/ui/editor/redisplay)
        (dolist (pkg-name upkg-names)
          (setq installed-count (1+ installed-count))
          (qingeditor/pkg/installer/install-package (qingeditor/layer/layer/get-package pkg-name)))
        (qingeditor/ui/editor/buffer-append "\n")))))

(defun qingeditor/pkg/installer/install-package (pkg-obj)
  "无条件安装指定的`pkg-obj'软件包。"
  (let* ((layer (when pkg-obj (car (oref pkg-obj :owners))))
         (location (when pkg-obj (oref pkg-obj :location)))
         (pkg-name (oref pkg-obj name))
         (min-version (when pkg-obj (oref pkg-obj :min-version))))
    (qingeditor/ui/editor/replace-last-line
     (format "--> 正在安装 %s: %s%s ... [%s/%s]"
             (if layer "软件包" "依赖软件包")
             pkg-name (if layer (format "@%S" layer) "")
             installed-count not-inst-count) t)
    (qingeditor/ui/editor/redisplay)
    (unless (package-installed-p pkg-name min-version)
      (condition-case-unless-debug err
          (cond
           ((or (null pkg-obj) (eq 'elpa location))
            (qingeditor/pkg/installer/install-from-elpa pkg-name)
            (when pkg-obj (qingeditor-package-set-property pkg-obj :lazy-install nil)))
           ((and (listp location) (eq 'recipe (car location)))
            (qingeditor/pkg/installer/install-from-recipe pkg-obj)
            (qingeditor/pkg/installer/package-set-property pkg-obj :lazy-install nil))
           (t (qingeditor/core/io/warning "安装软件包%S失败。" pkg-name)))
        ('error
         (qingeditor/core/runtime/increment-error-count)
         (qingeditor/ui/editor/buffer-append
          (format "\n安装软件包%s出现错误，错误信息：(%S)\n" pkg-name err))
         (qingeditor/ui/editor/redisplay))))))

(defun qingeditor/pkg/installer/install-from-elpa (pkg-name)
  "从ELPA源安装`pkg-name'软件包。"
  (if (not (assq pkg-name package-archive-contents))
      (qingeditor/ui/editor/buffer-append 
       (format "\n软件包%s当前不可用, 您可以检查一下是否将软件包的名字写错了"
               pkg-name))
    (let ((pkg-desc (assq pkg-name package-archive-contents)))
      (dolist (dep (qingeditor/pkg/installer/get-package-deps-from-archive pkg-name))
        (if (package-installed-p (car dep) (cadr dep))
            (qingeditor/pkg/installer/activate-package (car dep))
          (qingeditor/pkg/installer/install-from-elpa (car dep))))
      (if pkg-desc
          (package-install (cadr pkg-desc))
        (package-install pkg-name)))))

(defun qingeditor/pkg/installer/activate-package (pkg-name)
  "激活软件包`pkg-name'。"
  (unless (memq pkg-name package-activated-list)
    (package-activate pkg-name)))

(defun qingeditor/pkg/installer/get-uninstalled-packages (pkg-names)
  "返回传入的`pkg-names'列表的pkg中还没有安装的package列表。"
  (qingeditor/pkg/installer/filter-packages-with-deps
   pkg-names (lambda (pkg-name)
               (let* ((pkg-obj (qingeditor/layer/layer/get-package pkg-name))
                      (min-version (when pkg-obj (oref pkg-obj :min-version))))
                 (not (package-installed-p pkg-name min-version))))))

(defun qingeditor/pkg/installer/filter-packages-with-deps
    (pkg-names filter &optional use-archive)
  "返回`pkg-names'中符合`filter'条件的配置层列表。"
  (when pkg-names
    (let (result)
      (dolist (pkg-name pkg-names)
        ;; 递归检查依赖性
        (let* ((deps
                (if use-archive
                    (qingeditor/pkg/installer/get-package-deps-from-archive pkg-name)
                  (qingeditor/pkg/installer/get-package-deps-from-alist pkg-name)))
               (install-deps
                (when deps
                  (qingeditor/pkg/installer/filter-packages-with-deps (mapcar 'car deps) filter))))
          (when install-deps
            (setq result (append install-deps result))))
        (when (funcall filter pkg-name)
          (add-to-list 'result pkg-name t)))
      (delete-dups result))))

(defun qingeditor/pkg/installer/delete-orphan-packages (packages)
  "删除`packages'参数指定的软件包。"
  (let* ((dependencies (qingeditor/pkg/installer/generate-all-package-dependencies-map))
         (implicit-packages (qingeditor/pkg/installer/get-implicit-packages packages))
         (orphans (qingeditor/pkg/installer/get-orphan-packages packages implicit-packages dependencies))
         (orphans-count (length orphans))
         deleted-count)
    ;; (message "依赖软件包: %s" dependencies)
    ;; (message "隐形软件包: %s" implicit-packages)
    ;; (message "悬空软件包: %s" orphans)
    (if orphans
        (progn
          (qingeditor/ui/editor/buffer-append 
           (format "发现%s个悬空软件包，系统将删除他们...\n"
                   orphans-count))
          (setq deleted-count 0)
          (dolist (orphan orphans)
            (setq deleted-count (1+ deleted-count))
            (qingeditor/pkg/installer/replace-last-line
             (format "-> 正在删除软件包:%s... [%s/%s]"
                     orphan
                     deleted-count
                     orphans-count) t)
            (qingeditor/pkg/installer/package-delete orphan)
            (qingeditor/ui/editor/redisplay))
          (qingeditor/ui/editor/buffer-append "\n"))
      (qingeditor/core/io/message "没有悬空软件包需要删除\n"))))

(defun qingeditor/pkg/installer/get-orphan-packages (dist-pkgs implicit-pkgs dependencies)
  "获取悬空的`packages'，悬空的`package'可以理解为`qingeditoreditor'不再使用了，但是还存在在我们的elpa文件夹里面。"
  (let (result)
    (dolist (imp-pkg implicit-pkgs)
      (when (qingeditor/pkg/installer/is-package-orphan imp-pkg dist-pkgs dependencies)
        (add-to-list 'result imp-pkg)))))

(defun qingeditor/pkg/installer/is-package-orphan (pkg-name dist-pkgs dependencies)
  "判断指定的`pkg-name'是否是个悬空的`package'软件包。"
  (unless (or (memq pkg-name dist-pkgs)
              (memq pkg-name qingeditor/pkg/installer/protected-packages))
    (if (ht-contains? dependencies pkg-name)
        ;; 只要有一个依赖的包不是悬空的，当前的软件包就不是悬空的
        (let ((parents (ht-get dependencies pkg-name)))
          (cl-reduce (lambda (x y) (and x y))
                     (mapcar (lambda (p) (qingeditor/pkg/installer/is-package-orphan p dist-pkgs dependencies))
                             parents)
                     :initial-value t)))
    ;; 否则只要判断这个软件包是否在`dist-pkgs'列表里面
    (not (memq pkg-name dist-pkgs))))

(defun qingeditor/pkg/installer/get-package-deps-from-alist (pkg-name)
  "返回指定的`pkg-name'的依赖包关联列表。"
  (let ((pkg-desc (assq pkg-name package-alist)))
    (when pkg-desc (package-desc-reqs (cadr pkg-desc)))))

(defun qingeditor/pkg/installer/generate-all-package-dependencies-map ()
  "返回的在`package-alist'里面所有的软件包的依赖map。"
  (let ((result (make-hash-table :size 512)))
    (dolist (pkg package-alist)
      (let* ((pkg-sym (car pkg))
             (deps (qingeditor/pkg/installer/get-package-deps-from-alist pkg-sym)))
        (dolist (dep deps)
          ;; 探测当前的package是否存在在`hash'，如果存在直接在上面累加。
          (let* ((dep-sym (car dep))
                 (value (ht-get result dep-sym)))
            (puthash dep-sym
                     (if value (add-to-list 'value pkg-sym) (list pkg-sym))
                     result)))))
    result))

(defun qingeditor/pkg/installer/get-implicit-packages (packages)
  "获取在`packages-alist'里面但是不在`packages'参数里面的elpa软件包。"
  (let (imp-pkg-syms)
    (dolist (pkg-specs package-alist)
      (let ((pkg-sym (car pkg-specs)))
        (unless (memq pkg-sym packages)
          (add-to-list 'imp-pkg-syms pkg-sym))))
    imp-pkg-syms))

(defun qingeditor/pkg/installer/get-package-deps-from-archive (pkg-name)
  "通过archive内容获取指定的`pkg-name'的依赖包关联列表。"
  (let* ((pkg-arch (assq pkg-name package-archive-contents))
         (reqs (when pkg-arch (package-desc-reqs (cadr pkg-arch)))))
    ;; 递归的获取依赖列表
    (dolist (req reqs)
      (let* ((pkg-name2 (car req))
             (reqs2 (qingeditor/pkg/installer/get-package-deps-from-archive pkg-name2)))
        (when reqs2 (setq reqs (append reqs2 reqs)))))
    reqs))

(defun qingeditor/pkg/installer/get-package-dir (pkg-name)
  "获取指定`pkg-name'软件包的文件夹路径。"
  (let ((pkg-desc (assq pkg-name package-alist)))
    (package-desc-dir (cadr pkg-desc))))

(defun qingeditor/pkg/installer/install-quelpa ()
  "安装`quelpa'。"
  (setq quelpa-verbose init-file-debug
        quelpa-dir (concat qingeditor/cache-dir "quelpa/")
        quelpa-build-dir (expand-file-name "build" quelpa-dir)
        quelpa-persistent-cache-file (expand-file-name "cache" quelpa-dir))
  (qingeditor/pkg/installer/load-or-install-protected-package 'package-build)
  (qingeditor/pkg/installer/load-or-install-protected-package 'quelpa))

(provide 'qingeditor-installer)
