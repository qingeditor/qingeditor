;; 配置层原信息的类定义

(defclass qingeditor/layer/layer-meta ()
  ((name
    :initarg :name
    :type symbol
    :documentation "`layer'配置层的名称。")

   (dir
    :initarg :dir
    :initform nil
    :type (satisfies (lambda (x) (or (null x) (stringp x))))
    :documentation "`layer'配置层的绝对路径。")

   (packages
    :initarg :packages
    :initform nil
    :type list
    :documentation "")

   (selected-packages
    :initarg :selected-packages
    :initform 'all
    :type (satisfies (lambda (x)
                       (or (and (symbolp x) (eq 'all x))
                           (listp x))))
    :documentation "`layer'配置层中选中的package列表。")

   (variables
    :initarg :variables
    :initform nil
    :type list
    :documentation "`layer'配置层中关联的变量列表。")

   (lazy-install
    :initarg :lazy-install
    :initform nil
    :type boolean
    :documentation "如果不为`nil'那么这个层暂时不安装。")

   (disabled
    :initarg :disabled-for
    :initform nil
    :type list
    :documentation "指定当前这个`layer'是`disable'的`layer'列表")

   (enabled
    :initarg :enabled-for
    :initform 'unspecified
    :type (satisfies (lambda (x) (or (listp x) (eq 'unspecified x))))
    :documentation (concat "当前`layer'层相关联的所有`layer'层列表"
                           "(优先级高于 `:disabled-for'.)")))
  "配置层原信息。")

(defmethod qingeditor-layer-owned-packages ((layer qingeditor/layer/layer-meta))
  "返回指定的`layer'层下属的所有`packages'的列表，这个函数只有当配置已经安装了
才能正常工作。"
  (delq nil (mapcar
             (lambda (x)
               (let ((pkg (qingeditor-get-package qingeditor/layer/layer x)))
                 (when (and pkg (eq (oref layer :name)
                                    (car (oref pkg :owners))))
                   pkg)))
             (oref layer :packages))))

(defmethod qingeditor-layer-owned-packages ((layer nil))
  "当无对象关联时候，函数派发返回`nil'。"
  nil)

(defmethod qingeditor-layer-get-packages ((layer qingeditor/layer/layer-meta))
  "获取这个配置层的指定packages名称列表"
  (if (eq 'all (oref layer selected-packages))
      (oref layer packages)
    (delq nil (mapcar
               (lambda (x)
                 (let ((pkg-name (if (listp x) (car x) x)))
                   (when (memq pkg-name (oref layer selected-packages)) x)))
               (oref layer packages)))))

(provide 'qingeditor-layer-meta)
