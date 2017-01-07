;; 配置层相关函数

(require 'qingeditor-layer-meta)
(require 'qingeditor-pkg-meta)

(defvar qingeditor/layer/layer/exclude-all-layers nil
  "如果不为`nil'的话，系统将不加载除了`distributions'之外的所有layer配置层。")

(defvar qingeditor/layer/layer/force-distribution nil
  "强制安装默认的发行的layer配置层，忽略用户的选择`qingeditor/core/user-cfg/distribution'选项。")

(defvar qingeditor/layer/layer/layer-categories '()
  "layer配置层的分类名称列表，分类列表是一个以`+'开头的文件夹。")

(defvar qingeditor/layer/layer/packages-alist '()
  "用于记录在cache文件夹下面的关于回滚软件包的信息。")

(defvar qingeditor/layer/layer/template-dir qingeditor/template-dir
  "layer配置层的模板文件夹。")

(defvar qingeditor/layer/layer/layer-dir  (expand-file-name (concat qingeditor/start-dir "layers/"))
  "layer配置层的根文件夹。")

(defvar qingeditor/layer/layer/private-dir (expand-file-name (concat qingeditor/start-dir "private/"))
  "`qingeditoreditor'私有配置层根目录。")

(defvar qingeditor/layer/layer/private-layer-dir
  (let ((user-cfg-layer-dir
         (when qingeditor/core/user-cfg/target-cfg-dir
           (expand-file-name (concat qingeditor/core/user-cfg/target-cfg-dir "layers/")))))
    (if (and qingeditor/core/user-cfg/target-cfg-dir
             (file-exists-p user-cfg-layer-dir))
        user-cfg-layer-dir
      (expand-file-name (concat qingeditor/start-dir "private/"))))
  "`qingeditoreditor'私有配置层默认文件夹。如果存在`~/.qingeditor.d/layers'返回这个文件夹
否则的话，返回`~/.emacs.d/private'文件夹。")

(defvar qingeditor/layer/layer/load-packages-files-private nil
  "当此字段不为`nil'，那么当创建配置层对象的时候强制加载`packages.el'文件。")

(defvar qingeditor/layer/layer/used-layers-private '()
  "当前已经使用的配置层名字列表。")

(defvar qingeditor/layer/layer/indexed-layers-private (make-hash-table :size 1024)
  "保存了layer配置层的名字到配置层类`qingeditor/layer/layer-meta'对象的映射。")

(defvar qingeditor/layer/layer/used-packages-private '()
  "按照字母表顺序排列的使用的ELPA软件包的名字。")

(defvar qingeditor/layer/layer/indexed-packages-private (make-hash-table :size 2048)
  "保存了ELPA软件包名称到软件包元信息类`qingeditor/layer/pkg-meta'对象的映射。")

(defvar qingeditor/layer/layer/package-archives-refreshed-private nil
  "此字段不为`nil'代表我们已经刷新过ELPA软件包了。")

(defvar qingeditor/layer/layer/used-distant-packages-private '()
  "A list of all distant packages that are effectively used.")

(defvar qingeditor/layer/layer/check-new-version-error-packages-private nil
  "在系统更新的时候跳过更新的package的列表。")

(defvar qingeditor/layer/layer/lazy-mode-alist-private nil
  "迟延加载的模型列表, 键是mode的名称，值是用来匹配的正则表达式。")

(defvar qingeditor/layer/layer/inhibit-warnings-private nil
  "如果值不为`nil'，那么由配置层系统抛出的异常信息都会被拦截。")

(defvar qingeditor/layer/layer/package-properties-read-onlyp-private nil
  "如果不为`nil'那么package对象的属性是只读的，不能在`(qingeditor-make-pkg qingeditor/layer/layer)'函数中进行修改。")

(defvar qingeditor/layer/layer/declared-layers-usedp-private nil
  "如果不为`nil'，说明定义的配置层已经被使用了。")

(defun qingeditor/layer/layer/warning (msg &rest args)
  "正常的时候输出错误信息到`*Message*'里面, 如果
`qingeditor/layer/layer/inhibit-warnings-private'不为`nil'的时候这个函数什么都不输出。"
  (unless qingeditor/layer/layer/inhibit-warnings-private
    (apply 'qingeditor/core/io/warning msg args)))

(defun qingeditor/layer/layer/sync (&optional no-install)
  "同步我们在`qingeditor/core/user-cfg'里面配置的层，如果`no-install'不为`nil'函数将跳过
安装相应的layer的过程。"
  (qingeditor/core/call-func qingeditor/core/layer-cfg-setup "正在调用配置脚本的layer配置初始化函数...")
  (setq qingeditor/core/user-cfg/cfg-layers-saved-private
        qingeditor/core/user-cfg/cfg-layers)
  (when (qingeditor/ui/editor-drawer/choose-banner)
    (qingeditor/ui/editor-drawer/inject-version))
  ;; 定义layer配置层对象，让package能尽早的探测使用情况和依赖关系
  (qingeditor/layer/layer/discover-layers)
  (qingeditor/layer/layer/declare-used-layers qingeditor/core/user-cfg/cfg-layers)
  (qingeditor/layer/layer/declare-used-packages qingeditor/layer/layer/used-layers-private)
  ;; 加载配置层相关的文件，然后配置相关信息
  (qingeditor/layer/layer/load-layers-files qingeditor/layer/layer/used-layers-private
					    '("funcs.el"))
  (qingeditor/layer/layer/configure-layers qingeditor/layer/layer/used-layers-private)
  ;; 去掉一些elpa软件包，加快qingeditoreditor的加载速度
  (setq qingeditor/layer/layer/used-distant-packages-private
	(qingeditor/layer/layer/get-distant-packages qingeditor/layer/layer/used-packages-private t))
  (qingeditor/layer/layer/load-auto-layer-file)
  ;; 在这里我们开始安装需要的package或者删除不再需要的packages
  (unless no-install
    (let ((packages
           (append
            ;; 安装使用的elpa软件包
            (qingeditor/layer/layer/filter-objects
             qingeditor/layer/layer/used-distant-packages-private
             (lambda (pkg-name)
               (let ((pkg (qingeditor/layer/layer/get-package pkg-name)))
                 (not (oref pkg :lazy-install)))))
            ;; 同样去安装其他的elpa软件包
            (when (eq 'all qingeditor/core/user-cfg/install-packages)
              (let (all-other-packages)
                (dolist (layer-name (qingeditor/layer/layer/get-layer-names-list))
                  ;; 这个跟局部变量有区别，可能是bug的来源，先放在这里观察观察
                  (setq qingeditor/layer/layer/declared-layers-usedp-private nil)
                  (setq qingeditor/layer/layer/load-packages-files-private t)
                  (qingeditor/layer/layer/declare-layer layer-name)
                  (let* ((layer-obj (qingeditor-get-layer self layer-name))
                         (layer-pkgs-specs (when layer-obj (oref layer-obj :packages))))
                    (qingeditor/layer/layer/make-packages-from-layers (list layer-name))
                    (dolist (pkg-specs layer-pkgs-specs)
                      (let ((pkg-name (if (listp pkg-specs) (car pkg-specs) pkg-specs)))
                        (add-to-list 'all-other-packages pkg-name)))))
                (qingeditor/layer/layer/get-distant-packages all-other-packages nil))))))
      (qingeditor/pkg/installer/install-packages packages)
      (when (and (or (eq 'used qingeditor/core/user-cfg/install-packages)
                     (eq 'used-only qingeditor/core/user-cfg/install-packages))
                 (not qingeditor/layer/layer/force-distribution)
                 (not qingeditor/layer/layer/exclude-all-layers))
        (qingeditor/pkg/installer/delete-orphan-packages packages))))
  ;; 开始配置使用的package软件包
  (qingeditor/layer/layer/configure-packages qingeditor/layer/layer/used-packages-private)
  (qingeditor/layer/layer/load-layers-files qingeditor/layer/layer/used-layers-private '("keybindings.el")))

(defun qingeditor/layer/layer/configure-packages (packages)
  "配置所有传入的package软件包。"
  (setq qingeditor/ui/editor/loading-dots-chunk-threshold
	(/ (length qingeditor/layer/layer/used-packages-private)
	   qingeditor/ui/editor/loading-dots-chunk-count))
  (qingeditor/core/io/message "+ 正在配置bootstrap软件包...")
  (qingeditor/layer/layer/do-configure-packages
   (qingeditor/layer/layer/filter-objects
    packages (lambda (pkg-name)
               (let ((pkg-obj (qingeditor/layer/layer/get-package pkg-name)))
                 (eq 'bootstrap (oref pkg-obj :step))))))
  (qingeditor/core/io/message "+ 正在初始化pre软件包...")
  (qingeditor/layer/layer/do-configure-packages
   (qingeditor/layer/layer/filter-objects
    packages (lambda (pkg-name)
               (let ((pkg-obj (qingeditor/layer/layer/get-package pkg-name)))
                 (eq 'pre (oref pkg-obj :step))))))
  (qingeditor/core/io/message "+ 正在初始化软件包...")
  (qingeditor/layer/layer/do-configure-packages
   (qingeditor/layer/layer/filter-objects
    packages (lambda (pkg-name)
               (let ((pkg-obj (qingeditor/layer/layer/get-package pkg-name)))
                 (null (oref pkg-obj :step)))))))

(defun qingeditor/layer/layer/do-configure-packages (packages)
  "配置所有传入的packages软件包，我们在这里主要决定要配置哪些软件包
然后将需要配置的软件包的文件夹路径加入到`load-path'里面。"
  (dolist (pkg-name packages)
    (qingeditor/ui/editor/refresh-loading-bar)
    (let ((pkg-obj (qingeditor/layer/layer/get-package pkg-name)))
      (cond
       ((oref pkg-obj :lazy-install)
        (qingeditor/core/io/message
         (format "软件包%S是迟延安装，跳过。" pkg-name)))
       ((and (oref pkg-obj :excluded)
             (not (oref pkg-obj :protected)))
        (qingeditor/core/io/message
         (format "软件包%S在排除列表里面，跳过。" pkg-name)))
       ((null (oref pkg-obj :owners))
        (qingeditor/core/io/message 
         (format "软件包%S没有所有的配置层对象，跳过。" pkg-name)))
       ((not (qingeditor-package-enabledp pkg-obj t))
        (qingeditor/core/io/message 
         (fotmat "软件包%S没有启用，跳过。" pkg-name)))
       (t
        ;; 将当前的软件包加入到加载目录`load-path'里面
        (let ((location (oref pkg-obj :location)))
          (cond
           ((stringp location)
            (if (file-directory-p location)
                (push (file-name-as-directory location) load-path)
              (qingeditor/core/io/warning "软件包%S的加载目录(%s)不存在。" pkg-name location)))
           ((and (eq 'local location)
                 (eq 'dotfile (car (oref pkg-obj :owners))))
            ;; 这种情况的软件包是在`.dotfile'里面进行的手动的添加
            (push (file-name-as-directory
                   (concat qingeditor/layer/layer/private-dir "local/"
                           (symbol-name (oref pkg-obj :name))))
                  load-path))
           ((eq 'local location)
            ;; 本地软件包，但是不是在`.dotfile'里面配置的软件包
            ;; 这里的软件包是在layer配置层本地的软件包
            (let* ((owner (qingeditor/layer/layer/get-layer
                           (car (oref pkg-obj :owners))))
                   (layer-dir (when owner (oref owner :dir))))
              (push (format "%slocal/%S/" layer-dir pkg-name) load-path)))))
        (unless (memq (oref pkg-obj :location) '(local site built-in))
          (qingeditor/layer/layer/activate-package pkg-name))
        (cond
         ((eq 'dotfile (car (oref pkg-obj :owners)))
          (qingeditor/core/io/message (format "软件包%S已经在`.qingeditoreditor'文件中进行了配置。" pkg-name)))
         (t
          (qingeditor/layer/layer/configure-package pkg-obj))))))))

(defun qingeditor/layer/layer/activate-package  (pkg-name)
  "激活指定的package软件包。"
  (unless (memq pkg-name package-activated-list)
    (package-activate pkg-name)))

(defun qingeditor/layer/layer/configure-package (pkg-obj)
  "配置指定的`pkg-obj'软件包，主要是调用我们在layer配置层的`packages.el'文件中定义的
相应软件包的初始化函数。"
  (let* ((pkg-name (oref pkg-obj :name))
         (owner (car (oref pkg-obj :owners))))
    (qingeditor/core/io/message 
     (format "正在配置软件包%S..." pkg-name))
    ;; 调用pre init钩子函数
    (mapc
     (lambda (layer-name)
       (when (qingeditor/layer/layer/layer-usedp layer-obj)
         (if (not (qingeditor/layer/layer/package-enabled-p pkg-obj layer-name))
             (qingeditor/core/io/message 
              (format "软件没有使用，跳过软件包的qingeditor/%S/pre-init-%S钩子函数..." layer-name pkg-name))
           (qingeditor/core/io/message 
            (format "正在调用软件包qingeditor/%S/pre-init-%S钩子函数..." layer-name pkg-name))
           (condition-case-unless-debug err
               (funcall (intern (format "qingeditor/%S/pre-init-%S" layer-name pkg-name)))
             ('error
              (qingeditor/core/runtime/increment-error-count)
              (qingeditor/ui/editor/buffer-append
               (format "\n在调用qingeditor/%S/pre-init-%S钩子函数时候出现错误，错误信息(%s)"
                       layer-name
                       pkg-name
                       err)))))))
     (oref pkg-obj :pre-layers))
    ;; 调用init钩子函数
    (qingeditor/core/io/message (format " -> 正在调用钩子函数qingeditor/%S/init-%S"
                                        owner
                                        pkg-name))
    (funcall (intern (format "qingeditor/%S/init-%S" owner pkg-name)))
    ;; 调用post init钩子函数
    (mapc
     (lambda (layer-name)
       (when (qingeditor-layer-usedp self layer-name)
         (if (not (qingeditor/layer/layer/package-enabled-p pkg-obj layer-name))
             (qingeditor/core/io/message
              (format "软件没有使用，跳过软件包的qingeditor/%S/post-init-%S钩子函数..." layer-name pkg-name))
           (qingeditor/core/io/message
            (format "正在调用软件包qingeditor/%S/post-init-%S钩子函数..." layer-name pkg-name))
           (condition-case-unless-debug err
               (funcall (intern (format "qingeditor/%S/post-init-%S" layer-name pkg-name)))
             ('error
              (qingeditor/core/runtime/increment-error-count)
              (qingeditor/ui/editor/buffer-append
               (format "\n在调用qingeditor/%S/post-init-%S钩子函数时候出现错误，错误信息(%s)"
                       layer-name
                       pkg-name
                       err)))))))
     (oref-default pkg-obj :post-layers))))

(defun qingeditor/layer/layer/get-layer-names-list ()
  "获取当前系统中所有的启用的layer配置层的名字列表。"
  (ht-keys qingeditor/layer/layer/indexed-layers-private))

(defun qingeditor/layer/layer/load-auto-layer-file ()
  "加载`auto-layer'文件。"
  (let ((file (concat qingeditor/layer/layer/layer-dir "auto-layer.el")))
    (when (file-exists-p file)
      (qingeditor/core/io/message "正在加载auto-layer文件...")
      (load-file file))))

(defun qingeditor/layer/layer/get-distant-packages (packages usedp)
  "返回将要被安装的elpa软件包，如果`usedp'不为`nil'函数只返回标记为`used'的软件包，如果
为`nil'函数将返回`used'和`unused'软件包。"
  (qingeditor/layer/layer/filter-objects
   packages
   (lambda (x)
     (let ((pkg-obj (qingeditor/layer/layer/get-package x)))
       (and (not (memq (oref pkg-obj :location) '(built-in site local)))
            (not (stringp (oref pkg-obj :location)))
            (or (null usedp)
                (and (not (null (oref pkg-obj :owners)))
                     (not (oref pkg-obj :excluded))
                     (qingeditor-package-enabledp pkg-obj))))))))

(defun qingeditor/layer/layer/filter-objects (objects ffunc)
  "留下符合`ffunc'函数的对象，返回列表。"
  (reverse (cl-reduce (lambda (acc x) (if (funcall ffunc x) (push x acc) acc))
                      objects
                      :initial-value nil)))

(defun qingeditor/layer/layer/configure-layers (layer-names)
  "配置指定在`layer-names'的配置层对象相关信息"
  (let ((warning-minimum-level :error))
    (dolist (layer-name layer-names)
      (qingeditor/layer/layer/load-layer-files layer-name '("config.el")))))

(defun qingeditor/layer/layer/declare-used-packages (layers)
  "定义在layer配置层中设置的packages的元信息对象。"
  (setq qingeditor/layer/layer/used-packages-private nil)
  (let* ((warning-minimum-level :error))
    (qingeditor/layer/layer/make-packages-from-layers layers t)
    (qingeditor/layer/layer/make-packages-from-user-cfg-file t)
    (setq qingeditor/layer/layer/used-packages-private
          (qingeditor/layer/layer/sort-packages qingeditor/layer/layer/used-packages-private))))

(defun qingeditor/layer/layer/sort-packages (packages)
  "按照字母表的顺序进行排序。"
  (sort packages (lambda (x y) (string< (symbol-name x) (symbol-name y)))))

(defun qingeditor/layer/layer/make-packages-from-user-cfg-file (&optional usedp)
  "根据用户配置脚本里面创建elpa软件包对象, `usedp'不为`nil'的话当前添加的elpa软件包是使用状态。"
  (dolist (pkg-specs qingeditor/core/user-cfg/additional-packages)
    (let* ((pkg-name (if (listp pkg-specs) (car pkg-specs) pkg-specs))
           (pkg-obj (qingeditor/layer/layer/get-package pkg-name)))
      (if pkg-obj
          (setq pkg-obj (qingeditor/layer/layer/make-pkg pkg-specs 'dotfile pkg-obj))
        (setq pkg-obj (qingeditor/layer/layer/make-pkg pkg-specs 'dotfile)))
      (qingeditor/layer/layer/add-pkg pkg-obj t)))
  (dolist (exclude-pkg-specs qingeditor/core/user-cfg/excluded-packages)
    (let ((pkg-obj (qingeditor/layer/layer/get-package exclude-pkg-specs)))
      (unless pkg-obj
        (setq pkg-obj (qingeditor/layer/layer/make-pkg exclude-pkg-specs 'dotfile)))
      (qingeditor/layer/layer/add-pkg pkg-obj usedp)
      (qingeditor-package-set-property pkg-obj :exclude t))))

(defun qingeditor/layer/layer/make-packages-from-layers (layer-names &optional usedp)
  "根据`layer-names'参数生成package元信息，如果`usedp'不为`nil'表示生成的packages元信息对象属性已经使用的
状态。如果`qingeditor/core/user-cfg/additional-packages'字段不为空，在这里我们也会进行处理。"
  (dolist (layer-name layer-names)
    (let ((layer-obj (qingeditor/layer/layer/get-layer layer-name)))
      (dolist (pkg-specs (qingeditor-layer-get-packages layer-obj))
        (let* ((pkg-name (if (listp pkg-specs) (car pkg-specs) pkg-specs))
               (pkg-obj (qingeditor/layer/layer/get-package pkg-name)))
          (setq pkg-obj (qingeditor/layer/layer/make-pkg pkg-specs layer-name pkg-obj))
          (qingeditor/layer/layer/add-pkg pkg-obj (and (qingeditor-package-get-safe-owner pkg-obj) usedp)))))))

(defmethod qingeditor/layer/layer/make-pkg (pkg-specs layer-name &optional pkg-obj)
  "根据`pkg-specs'创建一个`qingeditor/layer/pkg-meta'类的实例，如果
`obj'不为`nil'那么我们将`pkg-specs'相关的属性复制进`obj'对象中。能复制的属性有：
`:location', `:step'和`:excluded'。如果`TOGGLEP'为`nil',那么`:toggle'属性忽略。"
  (let* ((pkg-name (if (listp pkg-specs) (car pkg-specs) pkg-specs))
         (pkg-name-str (symbol-name pkg-name))
         (layer (unless (eq 'dotfile layer-name)
                  (qingeditor/layer/layer/get-layer layer-name)))
         (min-version (when (listp pkg-specs) (plist-get (cdr pkg-specs) :min-version)))
         (step (when (listp pkg-specs) (plist-get (cdr pkg-specs) :step)))
         (toggle (when (listp pkg-specs) (plist-get (cdr pkg-specs) :toggle)))
         (excluded (when (listp pkg-specs) (plist-get (cdr pkg-specs) :excluded)))
         (location (when (listp pkg-specs) (plist-get (cdr pkg-specs) :location)))
         (protected (when (listp pkg-specs) (plist-get (cdr pkg-specs) :protected)))
         (init-func (intern (format "qingeditor/%S/init-%S" layer-name pkg-name)))
         (pre-init-func (intern (format "qingeditor/%S/pre-init-%S" layer-name pkg-name)))
         (post-init-func (intern (format "qingeditor/%S/post-init-%S" layer-name pkg-name)))
         (copyp (not (null pkg-obj)))
         (pkg-obj (if pkg-obj pkg-obj (qingeditor/layer/pkg-meta pkg-name-str :name pkg-name)))
         (ownerp (or (and (eq 'dotfile layer-name)
                          (null (oref pkg-obj owners)))
                     (fboundp init-func))))
    (when min-version
      (qingeditor-package-set-property pkg-obj :min-version (version-to-list min-version)))
    (when step
      (qingeditor-package-set-property pkg-obj :step step))
    (when toggle
      (qingeditor-package-set-property pkg-obj :toggle toggle))
    (qingeditor-package-set-property pkg-obj :excluded
				     (and (qingeditor/layer/layer/layer-usedp layer-name)
					  (or excluded (oref pkg-obj :excluded))))
    (when location
      (if (and (listp location)
               (eq (car location) 'recipe)
               (eq (plist-get (cdr location) :fetcher) 'local))
          (cond
           (layer (let ((path (expand-file-name
                               (format "%s%s/%s.el" (qingeditor/layer/layer/get-layer-load-dir layer-name)
                                       pkg-name-str pkg-name-str))))
                    (qingeditor-package-set-property
                     pkg-obj :location `(recipe :fetcher file :path ,path))))
           ((eq 'dotfile layer-name)
            ;; TODO 一个dofile类型的pkg他的本地路径到底是什么？暂时没想好。
            nil))
        (qingeditor-package-set-property pkg-obj :location location)))
    ;; 不能覆盖受保护的ELPA软件包
    (unless copyp
      ;; 初始化过程的ELAP软件包拾受保护的
      (qingeditor-package-set-property pkg-obj :protected (or protected (eq 'bootstrap step)))
      (when protected
        (push pkg-name qingeditor/pkg/installer/protected-packages)))
    (when ownerp
      ;; 当有多个所有者的时候输出警告信息
      (when (and (oref pkg-obj owners)
                 (not (memq layer-name (oref pkg-obj owners))))
        (qingeditor/core/io/warning 
         (format (concat "发现当前的elpa软件包%S有多个init函数，"
                         "前一个owner是%S，将会被替换成配置层layer %S。")
                 pkg-name (car (oref pkg-obj owners)) layer-name)))
      ;; 最后一个owner优先级最高
      (object-add-to-list pkg-obj :owners layer-name))
    ;; 检查`qingeditor/layer/pkg-meta'对象和其定义的初始化函数之间的一致性
    (unless (or ownerp
                (eq 'dotfile layer-name)
                (fboundp pre-init-func)
                (fboundp post-init-func)
                (oref pkg-obj :excluded))
      (qingeditor/core/io/warning 
       (format (concat
                "package软件包%s没有在配置层%s中初始化， "
                "您可以将这个package从列表中移除，或者使用"
                "`:toggle'关键字取代`when'表达式。")
               pkg-name layer-name)))
    ;; 判断这个elpa软件包是否可以应用toggle操作
    (when (and (not ownerp)
               (and (not (eq 'unspecified toggle))
                    toggle))
      (qingeditor/core/io/warning 
       (format (concat "package软件包%s的`:toggle'被禁用，因为"
                       "当前指定的配置层layer %s不拥有这个package。") pkg-name layer-name)))
    (when (fboundp pre-init-func)
      (object-add-to-list pkg-obj pre-layers layer-name))
    (when (fboundp post-init-func)
      (object-add-to-list pkg-obj post-layers layer-name))
    pkg-obj))

(defun qingeditor/layer/layer/get-package (pkg-name)
  "根据`pkg-name'获取获取package对象。不存在的话返回`nil'。"
  (when (ht-contains? qingeditor/layer/layer/indexed-packages-private pkg-name)
    (ht-get qingeditor/layer/layer/indexed-packages-private pkg-name)))

(defun qingeditor/layer/layer/get-packages-list ()
  "获取所有的ELPA软件包名称列表。"
  (ht-keys qingeditor/layer/layer/indexed-packages-private))

(defun qingeditor/layer/layer/declare-used-layers (layers-specs)
  "根据`layer-specs'参数，声明已使用的layer配置层。"
  (setq qingeditor/layer/layer/used-layers-private nil)
  (let ((qingeditor/layer/layer/declared-layers-usedp-private t))
    (unless qingeditor/layer/layer/exclude-all-layers
      (dolist (layer-specs layers-specs)
        (let* ((layer-name (if (listp layer-specs)
                               (car layer-specs)
                             layer-specs))
               (layer (qingeditor/layer/layer/get-layer layer-name)))
          (if layer
              (let ((layer-dir (oref layer dir)))
                (unless (string-match-p "+distributions" layer-dir)
                  (qingeditor/layer/layer/declare-layer layer-specs)))
            (qingeditor/core/io/warning "您在初始化脚本中指定的layer配置层：%s不支持。" layer-name))))
      (setq qingeditor/layer/layer/used-layers-private
            (reverse qingeditor/layer/layer/used-layers-private)))
    ;; distribution和bootstrap配置层必须在最前面
    (let* ((force-dist qingeditor/layer/layer/force-distribution)
           (distribution (if force-dist force-dist
                           qingeditor/core/user-cfg/distribution)))
      (unless (eq 'qingeditoreditor-bootstrap distribution)
        (qingeditor/layer/layer/declare-layer distribution)))
    (qingeditor/layer/layer/declare-layer 'qingeditoreditor-bootstrap)))

(defun qingeditor/layer/layer/declare-layers (layers-specs)
  "定义配置层列表中指定的layer配置层。"
  (mapc (lambda (layer-specs)
          (qingeditor/layer/layer/declare-layer layer-specs))
        layers-specs))

(defun qingeditor/layer/layer/declare-layer (layer-specs)
  "根据`layer-specs'来创建的一个layer配置层对象。
暂时这个函数通过`qingeditor/layer/layer/declared-layers-usedp-private'属性来判断
定义的layer配置层对象是否是一个已经使用的layer配置层。"
  (let* ((layer-name (if (listp layer-specs) (car layer-specs) layer-specs))
         (layer (qingeditor/layer/layer/get-layer layer-name))
         (usedp qingeditor/layer/layer/declared-layers-usedp-private))
    (if layer
        ;; 如果这里的layer存在的话就直接设置相关的对象属性，而不去重新创建一个新的
        ;; 配置层layer对象
        (let ((layer-obj (qingeditor/layer/layer/make-layer layer-specs layer usedp)))
          (qingeditor/layer/layer/add-layer layer-obj usedp)
          (qingeditor/layer/layer/set-layer-variables layer-obj)
          (when (or usedp qingeditor/layer/layer/load-packages-files-private)
            (qingeditor/layer/layer/load-layer-files layer-name '("layers.el"))))
      (qingeditor/core/io/warning "您在配置信息脚本配置的layer: %s不支持。" layer-name))))

(defun qingeditor/layer/layer/load-layers-files (layer-names files)
  "批量加载`layer-names'列表里面的所有layer配置层的关联文件。"
  (dolist (layer-name layer-names)
    (qingeditor/layer/layer/load-layer-files layer-name files)))

(defun qingeditor/layer/layer/load-layer-files (layer-name files)
  "加载配置层对象的相应的结构文件。"
  (let ((layer-obj (qingeditor/layer/layer/get-layer layer-name)))
    (when layer-obj
      (let ((layer-dir (oref layer-obj dir)))
        (dolist (filename files)
          (let ((filename (concat layer-dir filename)))
            (if (file-exists-p filename)
                (load filename))))))))

(defun qingeditor/layer/layer/set-layers-variables (layers)
  "给传入的layer配置层列表设置变量。"
  (mapc (lambda (layer)
          (qingeditor/layer/layer/set-layer-variables layer)) layers))

(defun qingeditor/layer/layer/set-layer-variables (layer)
  "给指定的layer配置层对象设置变量。"
  (let ((variables (oref layer variables)))
    (while variables
      (let ((var (pop variables)))
        (if (consp variables)
            (condition-case-unless-debug err
                (set-default var (eval (pop variables)))
              ('error
               (qingeditor/core/runtime/increment-error-count)
               (qingeditor/ui/editor/buffer-append 
                (format (concat "\n设置layer配置层变量%s出现错误，错误信息(%s)"
                                "请检查您是否转义必要的变量名称。\n") var err))))
          (qingeditor/core/io/warning "指定layer配置层变量%s的时候，缺少变量值。"))))))

(defun qingeditor/layer/layer/discover-layers ()
  "通过探测layer配置层的文件夹内容来初始化`qingeditor/layer/layer/indexed-layers'字段"
  ;; 我们在最后才去探测private layer文件夹，原因是用户才是上帝，他们的决定才是最终的决定
  ;; 如果`qingeditor/core/user-cfg/directory'存在的话，那么覆盖private配置层的文件夹。
  (let ((search-paths (append (list qingeditor/layer/layer/layer-dir)
                              qingeditor/core/user-cfg/cfg-layer-dir
                              (list qingeditor/layer/layer/private-layer-dir)
                              (when qingeditor/core/user-cfg/target-cfg-dir
                                (list qingeditor/core/user-cfg/target-cfg-dir))))
        (discovered '()))
    ;; 深度优先搜索所有的子目录
    (while search-paths
      (let ((current-path (car search-paths)))
        (setq search-paths (cdr search-paths))
        (dolist (sub-dir (directory-files current-path t nil 'nosort))
          ;; 忽略掉".", ".."和普通文件
          (unless (or (string-equal ".." (substring sub-dir -2))
                      (string-equal "." (substring sub-dir -1))
                      (not (file-directory-p sub-dir)))
            (let ((type (qingeditor/layer/layer/dir-type sub-dir)))
              (cond
               ((eq 'category type)
                (let ((category (qingeditor/layer/layer/get-category-from-dir sub-dir)))
                  (qingeditor/core/io/message "-> 发现配置层分类：%S" category)
                  (push category qingeditor/layer/layer/layer-categories)
                  ;; 这个推送的搜索列表最前面
                  (setq search-paths (cons sub-dir search-paths))
                  ))
               ((eq 'layer type)
                (let* ((layer-name-str (file-name-nondirectory sub-dir))
                       (layer-name (intern layer-name-str))
                       (indexed-layer (qingeditor/layer/layer/get-layer layer-name)))
                  (if indexed-layer
                      ;; 出现这种情况，代表这个layer配置层被发现了两次
                      ;; 我们在这里进行容错处理
                      (unless (string-equal (oref indexed-layer dir) sub-dir)
                        (qingeditor/layer/layer/warning (concat
							 "在文件路径\"%s\"发现重复的配置层%s"
							 "请将旧得配置层文件夹名称\"%s\"重命名成新的名称。")
							layer-name-str sub-dir (oref indexed-layer dir)))
                    (qingeditor/core/io/message "-> 发现配置层：%S" layer-name-str)
                    (qingeditor/layer/layer/add-layer (qingeditor/layer/layer/make-layer layer-name nil nil sub-dir)))
                  )
                )
               (t
                ;; 当前的文件夹没有发现配置层对象，把当前的文件夹添加到搜索列表继续进行搜索
                (setq search-paths (cons sub-dir search-paths)))))))))))

(defun qingeditor/layer/layer/make-layer (layer-specs &optional obj usedp dir)
  "根据`layer-specs'创建一个`qingeditor/layer/layer-meta'对象，如果`LOAD-PKGS'不为`nil'那么加载配置层的`packages.el'
`dir'是配置层的所在文件夹，如果为`nil'那么搜索indexed layers里面的数据。"
  (let* ((layer-name (if (listp layer-specs) (car layer-specs) layer-specs))
         (layer-name-str (symbol-name layer-name))
         (layer-obj (if obj obj (qingeditor/layer/layer-meta layer-name-str :name layer-name)))
         (dir (or dir (oref obj dir))))
    (if (or (null dir)
            (and dir (not (file-exists-p dir))))
        (qingeditor/core/io/warning
         "常见layer配置层对象%S失败，指定的文件夹不合法。" layer-name)
      (let* ((dir (file-name-as-directory dir))
             ;; 难道`:disabled-for'一定存在
             (disabled (when (listp layer-specs)
                         (qingeditor/core/mplist-get layer-specs :disabled-for)))
             (enabled (if (and (listp layer-specs)
                               (memq :enabled-for layer-specs))
                          (qingeditor/core/mplist-get layer-specs :enabled-for)
                        'unspecified))
             (variables (when (listp layer-specs)
                          (qingeditor/core/mplist-get layer-specs :variables)))
             (packages-file (concat dir "packages.el"))
             (packages
              (if (and (or usedp qingeditor/layer/layer/load-packages-files-private)
                       (file-exists-p packages-file))
                  (progn
                    (load packages-file)
                    (symbol-value (intern (format "qingeditor-%S-packages" layer-name))))
                (oref layer-obj packages)))
             (selected-packages (if packages
                                    (qingeditor/layer/layer/select-packages layer-specs packages)
                                  'all)))
        (oset layer-obj dir dir)
        (when usedp
          (oset layer-obj disabled disabled)
          (oset layer-obj enabled enabled)
          (oset layer-obj variables variables))
        (when packages
          (oset layer-obj packages packages)
          (oset layer-obj selected-packages selected-packages))
        layer-obj))))

(defun qingeditor/layer/layer/select-packages (layer-specs packages)
  "获取跟layer配置层的相关联的package列表。"
  (let* ((value (when (listp layer-specs)
                  (qingeditor/core/mplist-get layer-specs :packages)))
         (selected-packages (if (and (not (null (car value)))
                                     (listp (car value)))
                                (car value)
                              value)))
    (cond
     ;; 选中的package
     ((and selected-packages
           (not (memq (car selected-packages) '(all not))))
      selected-packages)
     ;; 除了列表中都选中
     ((and selected-packages
           (eq 'not (car selected-packages)))
      (delq nil (mapcar (lambda (x)
                          (let ((pkg-name (if (listp x) (car x) x)))
                            (unless (memq pkg-name selected-packages)
                              pkg-name)))
                        packages)))
     (t 'all))))

(defun qingeditor/layer/layer/get-category-from-dir (dir)
  "通过文件夹路径获取配置层分类的符号，配置层的名字是以`+'开头的字符串。函数返回`nil'。"
  (when (file-directory-p dir)
    (let ((dirname (file-name-nondirectory dir)))
      (when (string-match "^+" dirname)
        (intern (substring dirname 1))))))

(defun qingeditor/layer/layer/dir-type (dir)
  "返回传入的文件夹的类型, `dir'必须为绝对路径
可能的类型有如下几种：
layer       --   当前文件夹是一个配置层
category    --   当前文件夹是一个分类
nil         --   当前文件夹是一个普通文件夹"
  (when (file-directory-p dir)
    (if (string-match
         "^+" (file-name-nondirectory (directory-file-name dir)))
        'category
      (let ((files (directory-files dir)))
        ;; 根据文件夹里面的文件进行类型判断，最常见的文件先判断
        (when (or (member "packages.el" files)
                  (member "layers.el" files)
                  (member "config.el" files)
                  (member "keybindings.el" files)
                  (member "funcs.el" files))
          'layer)))))

(defun qingeditor/layer/layer/get-layer (layer-name)
  "通过配置层的名字获取配置层对象。"
  (let ((layers qingeditor/layer/layer/indexed-layers-private))
    (when (ht-contains? layers layer-name)
      (ht-get layers layer-name))))

(defun qingeditor/layer/layer/add-layer (layer &optional usedp)
  "增加一个layer配置层到系统，如果`usedp'不为`nil',说明当前的layer是一个已经使用的
配置层对象。"
  (let ((layer-name (oref layer name))
        (layer-map qingeditor/layer/layer/indexed-layers-private))
    (puthash layer-name layer layer-map)
    (setq qingeditor/layer/layer/indexed-layers-private layer-map)
    (when usedp
      (let ((used-layers qingeditor/layer/layer/used-layers-private))
        (add-to-list 'used-layers layer-name)
        (setq qingeditor/layer/layer/used-layers-private used-layers)))))

(defun qingeditor/layer/layer/add-pkg (pkg &optional usedp)
  "增加一个`PKG'添加当系统， `usedp'不为`nil'代表当前这个软件包是一个被使用的package。"
  (let ((pkg-name (oref pkg name)))
    (puthash pkg-name pkg qingeditor/layer/layer/indexed-packages-private)
    (when usedp
      (add-to-list 'qingeditor/layer/layer/used-packages-private pkg-name))))

(defun qingeditor/layer/layer/layer-usedp (layer-name)
  "判断相应的layer-name是否已经使用。"
  (let ((layer-obj (qingeditor/layer/layer/get-layer layer-name)))
    (when layer-obj
      (memq layer-name qingeditor/layer/layer/used-layers-private))))

(defun qingeditor/layer/layer/package-usedp (name)
  "判断一个指定的`package'是否使用。"
  (let ((pkg-obj (qingeditor/layer/layer/get-package name)))
    (and pkg-obj (qingeditor-package-get-safe-owner pkg-obj)
         (not (oref pkg-obj :excluded)))))

(defun qingeditor/layer/layer/configured-packages-stats  (packages)
  "获取传入的`packages'的状态信息，返回格式`alist'。"
  `((total ,(length packages))
    (elpa ,(length (qingeditor/layer/layer/filter-objects
                    packages
                    (lambda (pkg-name)
                      (let ((pkg-obj (qingeditor/layer/layer/get-package pkg-name)))
                        (eq 'elpa (oref pkg-obj :location)))))))
    (recipe ,(length (qingeditor/layer/layer/filter-objects
                      packages
                      (lambda (pkg-name)
                        (let* ((pkg-obj (qingeditor/layer/layer/get-package pkg-name))
                               (location (oref pkg-obj :location)))
                          (and (listp location)
                               (eq 'recipe (car location))))))))
    (local ,(length (qingeditor/layer/layer/filter-objects
		     packages
		     (lambda (pkg-name)
		       (let ((pkg-obj (qingeditor/layer/layer/get-package pkg-name)))
			 (memq (oref pkg-obj :location) '(local site)))))))
    (built-in ,(length (qingeditor/layer/layer/filter-objects
                        packages
                        (lambda (pkg-name)
                          (let ((pkg-obj (qingeditor/layer/layer/get-package pkg-name)))
                            (eq 'built-in (oref pkg-obj :location)))))))))

(provide 'qingeditor-layer)
