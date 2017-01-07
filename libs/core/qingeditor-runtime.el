;; `qingeditor'运行时的信息
(defun qingeditor/runtime/system-is-mac ()
  (eq system-type 'darwin))

(defun qingeditor/runtime/system-is-linux ()
  (eq system-type 'gnu/linux))

(defun qingeditor/runtime/system-is-mswindows ()
  (eq system-type 'windows-nt))

(defun qingeditor/runtime/window-system-is-mac ()
  ;; 在Emacs 25+的mac `(window-system)'返回 ns
  (memq (window-system) '(mac ns)))

(provide 'qingeditor-runtime)
