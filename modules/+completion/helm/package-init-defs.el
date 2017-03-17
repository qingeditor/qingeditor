;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; helm init method defs

(defun qingeditor/helm/init-ace-jump-helm-line ()
  (use-package ace-jump-helm-line
    :defer t
    :init
    (with-eval-after-load 'helm
      (define-key helm-map (kbd "C-q" 'ace-jump-helm-line)))))

(defun qingeditor/helm/init-auto-highlight-symbol ()
  )

(defun qingeditor/helm/post-init-bookmark ()
  )

(defun qingeditor/helm/init-helm ()
  )

(defun qingeditor/helm/init-helm-ag ()
  )

(defun qingeditor/helm/init-helm-descbinds ()
  )

(defun qingeditor/helm/init-helm-flx ()
  )

(defun qingeditor/helm/init-helm-make ()
  )

(defun qingeditor/helm/init-helm-mode-manager ()
  )

(defun qingeditor/helm/init-helm-projectile ()
  )

(defun qingeditor/helm/init-helm-swoop ()
  )

(defun qingeditor/helm/init-helm-themes ()
  )

(defun qingeditor/helm/init-qingeditor/helm-help-mode ()
  )

(defun qingeditor/helm/post-init-imenu ()
  )

(defun qingeditor/helm/init-popwin ()
  )

(defun qingeditor/helm/post-init-projectile ()
  )
