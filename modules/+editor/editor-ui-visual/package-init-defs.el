;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qingeditor/editor-ui-visual/init-ansi-colors ()
  (add-hook 'compilation-filter-hook
            #'qingeditor/editor-ui-visual/compilation-buffer-apply-ansi-colors))


(defun qingeditor/editor-ui-visual/init-fancy-battery ()
  (use-package fancy-battery
    :defer t
    :init
    (progn
      (qingeditor/add-toggle mode-line-battery
        :mode fancy-battery-mode
        :documentation "Display battery info in mode-line."
        :leader "tmb")
      (setq-default fancy-battery-show-percentage t))))

(defun qingeditor/editor-ui-visual/init-fill-column-indicator ()
  (use-package fill-column-indicator
    :defer t
    :init
    (progn
      (setq fci-rule-width 1)
      (setq fci-rule-color "#D0BF8F")
      ;; manually register the minor mode since it does not define any
      ;; lighter
      (push '(fci-mode "") minor-mode-alist)
      (qingeditor/add-toggle fill-column-indicator
        :status fci-mode
        :on (turn-on-fci-mode)
        :off (turn-off-fci-mode)
        :documentation "Display the fill column indicator."
        :leader "tf")
      (qingeditor/font/diminish fci-mode " ⓕ" " f"))))

(defun qingeditor/editor-ui-visual/init-golden-ratio ()
  (use-package golden-ratio
    :defer t
    :init
    (progn
      (setq qing-window-manipulation-transient-state-add-bindings
            '(("g" qing-toggle-golden-ratio)))
      (qingeditor/add-toggle golden-ratio
        :status golden-ratio-mode
        :on (golden-ratio-mode) (golden-ratio)
        :off (golden-ratio-mode -1) (balance-windows)
        :documentation "Resize the focused window using the golden ratio."
        :leader "tg"))
    :config
    (progn
      ;; golden-ratio-exclude-modes
      (dolist (m '("bs-mode"
                   "calc-mode"
                   "ediff-mode"
                   "dired-mode"
                   "gud-mode"
                   "gdb-locals-mode"
                   "gdb-registers-mode"
                   "gdb-breakpoints-mode"
                   "gdb-threads-mode"
                   "gdb-frames-mode"
                   "gdb-inferior-io-mode"
                   "gdb-disassembly-mode"
                   "gdb-memory-mode"
                   "speedbar-mode"))
        (add-to-list 'golden-ratio-exclude-modes m))
      (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")

      ;; goldren-ratio-extra-commands
      (dolist (f '(ace-window
                   ace-delete-window
                   ace-select-window
                   ace-swap-window
                   ace-maximize-window
                   avy-pop-mark
                   buf-move-left
                   buf-move-right
                   buf-move-up
                   buf-move-down
                   quit-window
                   select-window-0
                   select-window-1
                   select-window-2
                   select-window-3
                   select-window-4
                   select-window-5
                   select-window-6
                   select-window-7
                   select-window-8
                   select-window-9
                   winmove-left
                   winmove-right
                   winmove-up
                   winmove-down))
        (add-to-list 'golden-ratio-extra-commands f))

      ;; golden-ratio-exclude-buffer-names
      (dolist (n '(" *NeoTree*"
                   "*LV*"
                   " *which-key*"))
        (add-to-list 'golden-ratio-exclude-buffer-names n))
      (add-to-list 'golden-ratio-inhibit-functions
                   #'qinegditor/editor-ui-visual/no-golden-ratio-guide-key)
      (qingeditor/font/diminish golden-ratio-mode " ⓖ" " g"))))
