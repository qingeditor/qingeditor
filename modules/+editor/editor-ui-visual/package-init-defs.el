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

(defun qingeditor/editor-ui-visual/init-hl-todo ()
  (use-package hl-todo
    :defer t
    :init (qingeditor/add-to-hooks '(text-mode-hook
                                     prog-mode-hook)
                                   #'hl-todo-mode)))

(defun qingeditor/editor-ui-visual/init-neotree ()
  (use-package neotree
    :defer t
    :commands neo-global--window-exists-p
    :init
    (progn
      (setq neo-window-width 32
            neo-create-file-auto-open t
            neo-banner-message "Press ? for neotree help"
            neo-show-updir-line nil
            neo-mode-line-type 'neotree
            neo-smart-open t
            neo-dont-be-alone t
            neo-persist-show nil
            neo-show-hidden-files t
            neo-auto-indent-point t
            neo-modern-sidebar t
            neo-vc-integration nil)

      (qingeditor/define-transient-state neotree
        :title "NeoTree Key Hints"
        :doc "
Navigation^^^^             Actions^^             Visual actions/config^^^
───────^^^^─────────────── ───────^^────────     ───────^^^────────────────
[_L_]   next sibling^^     [_c_] create          [_TAB_] shrink/enlarge
[_H_]   previous sibling^^ [_d_] delete          [_|_]   vertical split
[_J_]   goto child^^       [_r_] rename          [_-_]   horizontal split
[_K_]   goto parent^^      [_R_] change root     [_gr_]  refresh^
[_l_]   open/expand^^      ^^                    [_s_]   hidden:^^^ %s(if neo-buffer--show-hidden-file-p \"on\" \"off\")
[_h_]   un/collapse^^      ^^                    ^^^
[_j_]   line down^^        ^^                    ^^^
[_k_]   line up^^          ^^                    ^^
[_RET_] open               ^^^^                  [_?_]   close hints
"
        :bindings
        ("RET"   neotree-enter)
        ("TAB"   neotree-stretch-toggle)
        ("|"     neotree-enter-vertical-split)
        ("-"     neotree-enter-horizontal-split)
        ("?"     nil :exit t)
        ("c"     neotree-create-node)
        ("d"     neo-delete-node)
        ("gr"    neo-refresh)
        ("h"     qing-neotree-collapse-or-up)
        ("H"     neotree-select-previous-sibling-node)
        ("j"     neo-next-line)
        ("J"     neo-select-down-node)
        ("k"     neo-previous-line)
        ("K"     neo-select-up-node)
        ("l"     qing-neotree-expand-or-open)
        ("L"     neo-select-next-sibling-node)
        ("r"     neotree-rename-node)
        ("R"     neo-change-root)
        ("s"     neotree-hidden-file-toggle))

    (qingeditor/key-binder/set-leader-keys-for-minor-mode
       'neotree-mode
       "TAB" 'neotree-stretch-toggle
        "RET" 'neotree-enter
        "|"   'neotree-enter-vertical-split
        "-"   'neotree-enter-horizontal-split
        "c"   'neotree-create-node
        "d"   'neotree-delete-node
        "gr"  'neotree-refresh
        "h"   'qing-neotree-collapse-or-up
        "H"   'neotree-select-previous-sibling-node
        "j"   'neotree-next-line
        "J"   'neotree-select-down-node
        "k"   'neotree-previous-line
        "K"   'neotree-select-up-node
        "l"   'qing-neotree-expand-or-open
        "L"   'neotree-select-next-sibling-node
        "q"   'neotree-hide
        "r"   'neotree-rename-node
        "R"   'neotree-change-root
        "?"   'qing-neotree-transient-state/body
        "s"   'neotree-hidden-file-toggle)

    (qingeditor/key-binder/set-leader-keys
       "ft" 'neotree-toggle
       "pt" 'neotree-find-project-root))))

(defun qingeditor/editor-ui-visual/init-popup ()
  )

(defun qingeditor/editor-ui-visual/init-popwin ()
  (use-package popwin
    :config
    (popwin-mode 1)
    (qingeditor/key-binder/set-leader-keys "wpm" 'popwin:messages)
    (qingeditor/key-binder/set-leader-keys "wpp" 'popwin:close-popup-window)
    ;; dont use default value but manager it ourselves
    (setq popwin:special-display-config nil)

    ;; buffers that we manage
    (push '("*Help*"                  :dedicated t :position bottom :stick t :noselect t :height 0.4)   popwin:special-display-config)
    (push '("*compilation*"           :dedicated t :position bottom :stick t :noselect t :height 0.4)   popwin:special-display-config)
    (push '("*Shell Command Output*"  :dedicated t :position bottom :stick t :noselect nil)             popwin:special-display-config)
    (push '("*Async Shell Command*"   :dedicated t :position bottom :stick t :noselect nil)             popwin:special-display-config)
    (push '(" *undo-tree*"            :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
    (push '("*ert*"                   :dedicated t :position bottom :stick t :noselect nil          )   popwin:special-display-config)
    (push '("*grep*"                  :dedicated t :position bottom :stick t :noselect nil          )   popwin:special-display-config)
    (push '("*nosetests*"             :dedicated t :position bottom :stick t :noselect nil          )   popwin:special-display-config)
    (push '("^\*WoMan.+\*$"           :dedicated t :position bottom :stick t :noselect nil          )   popwin:special-display-config)))

(defun qingeditor/editor-ui-visual/init-smooth-scrolling ()
  (setq scroll-preserve-screen-position t
        scroll-margin 0
        scroll-conservatively (if qingeditor/config/smooth-scrolling 101 0))
  (qingeditor/add-toggle smooth-scrolling
    :status (= 101 scroll-conservatively)
    :on (qing-enable-smooth-scrolling)
    :off (qing-disable-smooth-scrolling)
    :documentation "Smooth scrolling."
    :leader "tv"))
