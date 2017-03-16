;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3
;;
;; editor completion module extra config setup

(defface qingeditor/editor-completion/helm-navigation-ms-face
  `((t :background ,(face-attribute 'error :foreground)
       :foreground "black"))
  "Face for helm header when helm transient-state is activated."
  :group 'qingeditor)

;; from https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
(with-eval-after-load 'helm
  (defvar helm-source-header-default-background
    (face-attribute 'helm-source-header :background))
  (defvar helm-source-header-default-foreground
    (face-attribute 'helm-source-header :foreground))
  (defvar helm-source-header-default-box
    (face-attribute 'helm-source-header :box))
  (defvar helm-source-header-default-height
    (face-attribute 'helm-source-header :height)))

