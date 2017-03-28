;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qingeditor/syntax-checking/init-flycheck ()
  (use-package flycheck
    :defer t
    :init
    (progn
      (setq flycheck-standard-error-naivigation nil
            flycheck-golbal-modes nil)

      (qingeditor/add-toggle syntax-checking
        :mode flycheck-mode
        :documentation "Enable error and syntax checking."
        :lader "ts")

      (qingeditor/font/diminish flycheck-mode " ⓢ" " s")

      (when qingeditor/syntax-checking/syntax-checking-enable-by-default
        (global-flycheck-mode 1))

      ;; Custom fringe indicator
      (when (and (fboundp 'define-fringe-bitmap)
                 (not qingeditor/syntax-checking/use-original-bitmaps))
        (define-fringe-bitmap 'my-flycheck-fringe-indicator
          (vector #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00011100
                  #b00111110
                  #b00111110
                  #b00111110
                  #b00011100
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000)))

      (let ((bitmap (if qingeditor/syntax-checking/use-original-bitmaps
                        'flycheck-fringe-bitmap-double-arrow
                      'my-flycheck-fringe-indicator)))
        (flycheck-define-error-level
         'error
         :severity 2
         :overlay-category 'flycheck-error-overylay
         :fringe-bitmap bitmap
         :fringe-face 'flycheck-fringe-error)

        (flycheck-define-error-level
         'warning
         :severity 1
         :overlay-category 'flycheck-warning-overlay
         :fringe-bitmap bitmap
         :fringe-face 'flycheck-fringe-warning)

        (flycheck-define-error-level
         'info
         :severity 0
         :overlay-category 'flycheck-info-overlay
         :fringe-bitmap bitmap
         :fringe-face 'flycheck-fringe-info))

      ;; toggle flycheck window
      (defun qing-toggle-flycheck-error-list ()
        "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
        (interactive)
        (-if-let (window (flycheck-get-error-list-window))
            (quit-window nil window)
          (flycheck-list-errors)))

      (defun qing-goto-flycheck-error-list ()
        "Open and go to the error list buffer."
        (interactive)
        (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
          (flycheck-list-errors)
          (switch-to-buffer-other-window flycheck-error-list-buffer)))

      (qingeditor/key-binder/set-leader-keys-for-minor-mode
       'flycheck-error-list-mode
       "RET" #'flycheck-error-list-goto-error
       "j"   #'flycheck-error-list-next-error
       "k"   #'flycheck-error-list-previous-error)

      ;; key bindings
      (qingeditor/key-binder/set-leader-keys
       "ec" #'flycheck-clear
       "eh" #'flycheck-describe-checker
       "el" #'qing-toggle-flycheck-error-list
       "eL" #'qing-goto-flycheck-error-list
       "ee" #'flycheck-explain-error-at-point
       "es" #'flycheck-select-checker
       "eS" #'flycheck-set-checker-executable
       "ev" #'flycheck-verify-setup))))

(defun qingeditor/syntax-checking/init-flycheck-pos-tip ()
  (use-package flycheck-pos-tip
    :if qingeditor/syntax-checking/syntax-checking-enable-tooltips
    :defer t
    :init
    (with-eval-after-load 'flycheck
      (flycheck-pos-tip-mode))))

(defun qingeditor/syntax-checking/post-init-popwin ()
  (push '("^\\*Flycheck.+\\*$"
          :regexp t
          :dedicated t
          :position bottom
          :stick t
          :noselect t)
        popwin:special-display-config))
