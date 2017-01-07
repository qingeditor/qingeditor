;; qingeditor-pkg-meta 类定义
;; 类属性定义

(defclass qingeditor-pkg-meta ()
  ((name
    :initarg :name
    :type symbol
    :documentation "`package'的名称。")

   (min-version
    :initarg :min-version
    :initform nil
    :type list
    :documentation "Minimum version to install as a version list.")

   (owners
    :initarg :owners
    :initform nil
    :type list
    :documentation "定义有`init'函数的配置层对象列表。")

   (pre-layers
    :initarg :pre-layers
    :initform '()
    :type list
    :documentation "定义有`pre-init'函数的配置层对象列表。")

   (post-layers
    :initarg :post-layers
    :initform '()
    :type list
    :documentation "定义有`post-init'函数的配置层对象列表。")

   (location
    :initarg :location
    :initform elpa
    :type (satisfies (lambda (x)
                       (or (stringp x)
                           (memq x '(built-in local site elpa))
                           (and (listp x) (eq 'recipe (car x))))))
    :documentation "`ELAP'软件包的位置。")

   (toggle
    :initarg :toggle
    :initform t
    :type (satisfies (lambda (x) (or (symbolp x) (listp x))))
    :documentation "当前`ELPA'软件包是否使用的标志变量。")

   (step
    :initarg :step
    :initform nil
    :type (satisfies (lambda (x) (member x '(nil bootstrap pre))))
    :documentation "Initialization step.")

   (lazy-install
    :initarg :lazy-install
    :initform nil
    :type boolean
    :documentation "`ELPA'软件包迟延安装的标志变量。")

   (protected
    :initarg :protected
    :initform nil
    :type boolean
    :documentation "`ELPA'软件包是否被保护的标志变量，如果这个属性不为`nil'，当前软件包
不能加入到`excluded'列表中。")

   (excluded
    :initarg :excluded
    :initform nil
    :type boolean
    :documentation "If non-nil this package is excluded from all layers.")
   ))

;; 类方法定义

(defmethod qingeditor-package-enabledp ((this qingeditor/layer/package-meta) &optional inhibit-message)
  "对当前的ELPA包的`toggle'属性进行求值并且返回结果。"
  (let ((message-log-max (unless inhibit-message message-log-max))
        (toggle (oref this toggle)))
    (eval toggle)))

(defmethod qingeditor-package-get-safe-owner ((this qingeditor/layer/package-meta))
  "获取所有的拥有当前ELPA软件包的配置层列表。"
  ;; The owner of a package is the first *used* layer in `:owners' slot.
  ;; Note: for packages in `qingeditor/layer/layer/used-packages' the owner is
  ;; always the car of the `:owners' slot.
  (let ((layers (oref this owners)))
    (while (and (consp layers)
                (not (qingeditor/layer/layer/layer-usedp (car layers))))
      (pop layers))
    (when (qingeditor/layer/layer/layer-usedp (car layers))
      (car layers))))

(defmethod qingeditor-package-set-property ((this qingeditor/layer/package-meta) slot value)
  "给当前的ELPA软件包对象设置键值对，如果`qingeditor/layer/layer/package-properties-read-onlyp-private'不为
`nil', 那么久忽略此次设置请求。"
  (unless qingeditor/layer/layer/package-properties-read-onlyp-private
    (eval `(oset this ,slot value))))

(provide 'qingeditor-pkg-meta)
