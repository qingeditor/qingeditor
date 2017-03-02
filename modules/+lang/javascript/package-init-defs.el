;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
(defmethod qingeditor/cls/init-coffee-mode
  ((this qingeditor/module/javascript))
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

(defmethod qingeditor/cls/init-flycheck
  ((this qingeditor/module/javascript))
  )

(defmethod qingeditor/cls/init-flycheck
  ((this qingeditor/module/javascript))
  )

(defmethod qingeditor/cls/init-ggtags
  ((this qingeditor/module/javascript))
  )

(defmethod qingeditor/cls/init-helm-gtags
  ((this qingeditor/module/javascript))
  )

(defmethod qingeditor/cls/init-js-doc
  ((this qingeditor/module/javascript))
  )

(defmethod qingeditor/cls/init-js2-mode
  ((this qingeditor/module/javascript))
  )

(defmethod qingeditor/cls/init-js2-refactor
  ((this qingeditor/module/javascript))
  )

(defmethod qingeditor/cls/init-json-mode
  ((this qingeditor/module/javascript))
  )

(defmethod qingeditor/cls/init-json-snatcher
  ((this qingeditor/module/javascript))
  )

(defmethod qingeditor/cls/init-tern
  ((this qingeditor/module/javascript))
  )

(defmethod qingeditor/cls/init-web-beautify
  ((this qingeditor/module/javascript))
  )

(defmethod qingeditor/cls/init-skewer-mode
  ((this qingeditor/module/javascript))
  )

(defmethod qingeditor/cls/init-livid-mode
  ((this qingeditor/module/javascript))
  )
