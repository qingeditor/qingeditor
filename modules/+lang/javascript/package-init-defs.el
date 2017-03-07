;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
(defun qingeditor/javascript/init-coffee-mode ()
  (use-package coffee-mode
    :defer t
    :init
    (progn
      (defun qingeditor/javascript/coffee-indent ()
        (if (coffee-line-wants-indent)
            ;; We need to insert an additional tab because the last line was special.
            (coffee-insert-spaces (_ (coffee-previous-indent) coffee-tab-width))
          ;; otherwise keep at the same indentation level
          (coffee-insert-spaces (coffee-previous-indent))))
      (add-hook 'coffee-mode-hook
                '(lambda ()
                   (setq indent-line-function 'qingeditor/javascript/coffee-indent))))))

(defun qingeditor/javascript/init-flycheck ()
  )

(defun qingeditor/javascript/init-flycheck ()
  )

(defun qingeditor/javascript/init-ggtags ()
  )

(defun qingeditor/javascript/init-helm-gtags ()
  )

(defun qingeditor/javascript/init-js-doc ()
  )

(defun qingeditor/javascript/init-js2-mode ()
  )

(defun qingeditor/javascript/init-js2-refactor ()
  )

(defun qingeditor/javascript/init-json-mode ()
  )

(defun qingeditor/javascript/init-json-snatcher ()
  )

(defun qingeditor/javascript/init-tern ()
  )

(defun qingeditor/javascript/init-web-beautify ()
  )

(defun qingeditor/javascript/init-skewer-mode ()
  )

(defun qingeditor/javascript/init-livid-mode ()
  )
