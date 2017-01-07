;; 定义全局的核心变量

(defgroup qingeditor nil "`QingEditor'自定义分组"
  :group 'starter-kit
  :prefix 'qingeditor-)

(defvar qingeditor/gvars/post-user-cfg-hook nil  "当用户配置函数调用之后执行的钩子函数。")
(defvar qingeditor/gvars/post-user-cfg-hook-run nil
  "标志变量，判断`qingeditor/gvars/post-user-config-hook'是否已经运行。")

(defvar qingeditor/gvars/insecure nil
  "当`qingeditor'访问网络的时候，是否使用不安全的连接。")

(defvar qingeditor/gvars/post-theme-change-hook nil
  "当风格变化的时候执行的钩子函数。")

(provide 'qingeditor-gvars)
