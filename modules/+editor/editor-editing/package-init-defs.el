;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qingeditor/editor-editing/init-aggressive-indent ()
  (use-package aggressive-indent
    :defer t
    :init
    (progn
      (qingeditor/add-toggle aggressive-indent
        :mode aggressive-indent-mode
        :documentation "Always keep code indented."
        :leader "tI")
      (qingeditor/add-toggle aggressive-indent-globally
        :mode global-aggressive-indent-mode
        :documentation "Always keep code indented globally."
        :leader "t C-I")
      )
    :config
    (progn
      (add-hook 'diff-auto-refine-mode-hook
                #'qing-toggle-aggressive-indent-off)
      (qingeditor/font/diminish aggressive-indent-mode " Ⓘ" " I"))))

(defun qingeditor/editor-editing/init-avy ()
  )

(defun qingeditor/editor-editing/init-bracketed-paste ()
  )

(defmethod qingeditor/editor-editing/init-clean-aindent-mode ()
  )

(defmethod qingeditor/editor-editing/init-eval-sexp-fu ()
  )

(defun qingeditor/editor-editing/init-expand-region ()
  )

(defun qingeditor/editor-editing/init-hexl ()
  )

(defun qingeditor/editor-editing/init-hungry-delete ()
  )

(defun qingeditor/editor-editing/init-link-hint ()
  )

(defun qingeditor/editor-editing/init-lorem-ipsum ()
  )

(defun qingeditor/editor-editing/init-move-text ()
  )

(defun qingeditor/editor-editing/init-origami ()
  )

(defun qingeditor/editor-editing/init-smartparens ()
  )

(defun qingeditor/editor-editing/init-qingeditor/whitespace-cleanup ()
  )

(defun qingeditor/editor-editing/init-undo-tree ()
  )

(defun qingeditor/editor-editing/init-uuidgen ()
  )

(defun qingeditor/editor-editing/init-ws-butler ()
  )
