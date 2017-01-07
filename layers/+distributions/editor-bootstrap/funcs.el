;; 定义一些`editor-bootstrap'配置层提供的函数

(defun qingeditor/editor-bootstrap/state-color-face (state)
  "返货指定`state'的`face'的名称。"
  (intern (format "qingeditor/editor-bootstrap/%s-face" (symbol-name state))))

(defun qingeditor/editor-bootstrap/state-color (state)
  "获取状态`state'关联的颜色值。"
  (face-background (qingeditor/editor-bootstrap/state-color-face state)))

(defun qingeditor/editor-bootstrap/current-state-color ()
  "获取当前的状态的颜色。"
  (face-background (qingeditor/editor-bootstrap/state-color-face evil-state)))

(defun qingeditor/editor-bootstrap/current-state-face ()
  "获取当前状态的`face'符号。"
  (let ((state (if (eq evil-state 'operator)
                   evil-previous-state
                 evil-state)))
    (qingeditor/editor-bootstrap/state-color-face state)))

(defun qingeditor/editor-bootstrap/set-state-faces ()
  (cl-loop for (state color cursor) in qingeditor/editor-bootstrap/evil-cursors
           do
           (set-face-attribute (intern (format "qingeditor/editor-bootstrap/%s-face" state))
                               nil
                               :foreground (face-background 'mode-line))))

(defun qingeditor/editor-bootstrap/evil-insert-state-cursor-hide ()
  (setq evil-insert-state-cursor '((hbar . 0))))

(defun qingeditor/editor-bootstrap/diminish-hook (_)
  "在Emacs的`mode-line'上显示一个减弱的小图标。"
  (let ((unicodep qingeditor/core/user-cfg/show-mode-line-unicode-symbols))
    (cl-loop for (mode uni nouni) in qingeditor/ui/editor-font/diminished-minor-modes
             do (diminish mode (if unicodep uni nouni)))))

(defun qingeditor/editor-bootstrap/set-state-faces ()
  (cl-loop for (state color cursor) in qingeditor/editor-bootstrap/evil-cursors
           do
           (set-face-attribute (intern (format "qingeditor/editor-bootstrap/%s-face" state))
                               nil
                               :foreground (face-background 'mode-line))))

(defun qingeditor/editor-bootstrap/set-evil-shift-width ()
  "根据当前的`major mode'的缩进设置来设置`evil-shift-width'的值。"
  (let ((shift-width
         (catch 'break
           (dolist (test qingeditor/editor-bootstrap/indent-variable-alist)
             (let ((mode (car test))
                   (val (cdr test)))
               (when (or (and (symbolp mode) (derived-mode-p mode))
                         (and (listp mode) (apply 'derived-mode-p mode))
                         (eq 't mode))
                 (when (not (listp val))
                   (setq val (list val)))
                 (dolist (v val)
                   (cond
                    ((integerp v) (throw 'break v))
                    ((and (symbolp v) (boundp v))
                     (throw 'break (symbol-value v))))))))
           ;; 没有找到我们在这里返回全局的默认的缩进值
           (throw 'break (default-value 'evil-shift-width)))))
    (when (and (integerp shift-width)
               (< 0 shift-width))
      (setq-local evil-shift-width shift-width))))

(defmacro qingeditor/editor-bootstrap/define-text-object (key name start end)
  "定义一个文本对象和一个环绕对。
`start'和`end'都是字符串，不是正则表达式，这两个值定义了文本对象的边界。"
  `(progn
     (qingeditor/editor-bootstrap/define-text-object-regexp ,key ,name
                                                        ,(regexp-quote start)
                                                        ,(regexp-quote end))
     (with-eval-after-load 'evil-surround
       (push (cons (string-to-char ,key)
                   (if ,end
                       (cons ,start ,end)
                     ,start))
             evil-surround-pairs-alist))))

(defmacro qingeditor/editor-bootstrap/define-text-object-regexp (key name start-regexp end-regexp)
  "定义一个文本对象
`start-regexp'和`end-regexp'是文本对象的边界。"
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-outer-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regexp ,end-regexp beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regexp ,end-regexp beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;; 这个必须延迟，因为这个宏定义使用了`evil'里面的`keymap'
(with-eval-after-load 'evil
  (defmacro qingeditor/editor-bootstrap/evil-map (state key seq)
    "在特定的状态`state'中定义`key'到`seq'的映射。

当递归定义时候，我们只支持`key'是按键序列的`seq'第一个元素。
例如：(evil-map visual \"<\" \"<gv\")"
    (let ((map (intern (format "evil-%S-state-map" state))))
      `(define-key ,map ,key
         (lambda ()
           (interactive)
           ,(if (string-equal key (substring seq 0 1))
                `(progn
                   (call-interactively ',(lookup-key evil-normal-state-map key))
                   (execute-kbd-macro ,(substring seq 1)))
              (execute-kbd-macro ,seq)))))))

(defun qingeditor/editor-bootstrap/evil-smart-doc-lookup ()
  "使用特定模型的`goto-definition'绑定的版本的`evil-lookup'函数， 例如
使用按键序列`SPC m h h'查询定义，如果没有绑定的话使用`evil-lookup'来查询定义。"
  (interactive)
  (let ((binding (key-binding (kbd (concat qingeditor/core/user-cfg/leader-key " mhh")))))
    (if (commandp binding)
        (call-interactively binding)
      (evil-lookup))))

(defun qingeditor/editor-bootstrap/hydra-key-doc-function (key key-width doc doc-width)
  "为`keys'自定义hint文档格式"
  (format (format "[%%%ds] %%%ds" key-width (- -1 doc-width))
          key doc))
