;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qingeditor/editor-editing-visual/init-adaptive-wrap ()
  (use-package adaptive-wrap
    :config
    (progn
      (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))))

(defun qingeditor/editor-editing-visual/init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :defer t
    :init
    (progn
      (setq ahs-case-fold-search nil
            ahs-default-range 'ahs-range-whole-buffer
            ;; by default disable autu-highlight of symbol
            ;; current symbol can aways be highlighted with `SPC s h'
            ahs-idle-timer 0
            ahs-idle-interval 0.25
            ahs-inhibit-face-list nil
            qingeditor/editor-ui-visual/symbol-highlight-state-doc
            "
 %s  [_n_] next  [_N_/_p_] previous        [_r_] change range   [_R_] reset        [_e_] iedit
 %s  [_d_/_D_] next/previous definition")
      ;; since we are creating our own maps,
      ;; prevent the default keymap from getting created
      (setq auto-highlight-symbol-mode-map (make-sparse-keymap))
      (qingeditor/add-toggle automatic-symbol-highlight
        :status (timerp ahs-idle-timer)
        :on (progn
              (auto-highlight-symbol-mode)
              (setq ahs-idle-timer
                    (run-with-idle-timer ahs-idle-interval t
                                         #'ahs-idle-function)))
        :off (when (timerp ahs-idle-timer)
               (auto-highlight-symbol-mode)
               (cancel-timer ahs-idle-timer)
               (setq ahs-idle-timer 0))
        :documentation "Automatic highlight of the current symbol."
        :leader "tha")

      (qingeditor/add-to-hooks #'auto-highlight-symbol-mode
                               '(prog-mode
                                 markdown-mode)))
    :config
    (progn
      (qingeditor/font/hide-lighter auto-highlight-symbol-mode)
      (defvar-local qingeditor/editor-ui-visual/last-ahs-highlight-p nil
        "Go to the last searched highlighted symbol.")
      (defvar-local qingeditor/editor-ui-visual/ahs-searching-forward t)

      (defun qing-goto-last-searched-ahs-symbol ()
        "Go to the last know occurrence of the last symbol searched with
`auto-highlight-symbol'."
        (interactive)
        (if qingeditor/editor-ui-visual/last-ahs-highlight-p
            (progn
              (goto-char (nth 1 qingeditor/editor-ui-visual/last-ahs-highlight-p))
              (qingeditor/editor-ui-visual/ahs-highlight-now-wrapper)
              (qing-symbol-highlight-transient-state/body))
          (message "No symbol has been searched for now.")))

      (defun qingeditor/editor-editing-visual/ensure-ahs-enabled-locally ()
        "Ensure ahs is enabled for the local buffer."
        (unless
            (bound-and-true-p ahs-mode-line)
          (auto-highlight-symbol-mode)))

      (defun qingeditor/editor-editing-visual/ahs-highlight-now-wrapper ()
        "Safe wrapper for ahs-highlight-now"
        (eval '(progn
                 (qingeditor/editor-editing-visual/ensure-ahs-enabled-locally)
                 (ahs-highlight-now)) nil))


      (defun qing-enter-ahs-forward ()
        "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'."
        (interactive)
        (setq qingeditor/editor-editing-visual/ahs-searching-forward t)
        (qing-quick-ahs-forward))

      (defun qing-enter-ahs-backward ()
        "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'."
        (interactive)
        (setq qingeditor/editor-editing-visual/ahs-searching-forward nil)
        (qing-quick-ahs-backward))

      (defun qing-quick-ahs-forward ()
        "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'."
        (interactive)
        (qingeditor/editor-editing-visual/quick-ahs-move t))

      (defun qing-quick-ahs-backward ()
        "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'."
        (interactive)
        (qingeditor/editor-editing-visual/quick-ahs-move nil))

      (defun qingeditor/editor-editing-visual/quick-ahs-move (forward)
        "Go t the next occurrence of symbol under point with
`auto-highlight-symbol'."
        (if (eq forward qingeditor/editor-editing-visual/ahs-searching-forward)
            (progn
              (qingeditor/editor-editing-visual/ahs-highlight-now-wrapper)
              (qing-symbol-highlight-transient-state/body)
              (ahs-forward))
          (progn
            (qingeditor/editor-editing-visual/ahs-highlight-now-wrapper)
            (qing-symbol-highlight-transient-state/body)
            (ahs-backward))))

      (defun qing-symbol-highlight ()
        "Highlighting the symbol under point with `auto-highlight-symbol'."
        (interactive)
        (qingeditor/editor-editing-visual/ahs-highlight-now-wrapper)
        (setq qingeditor/editor-editing-visual/last-ahs-highlight-p (ahs-highlight-p))
        (qing-symbol-highlight-transient-state/body))

      (defun qingeditor/editor-editing-visual/ahs-ms-on-exit ()
        ;; Restore user search direction state as ahs has exitted in a state
        ;; good for <C-s>, but not for 'n' and 'N'"
        (setq isearch-forward qingeditor/editor-editing-visual/ahs-searching-forward))

      (defun qing-symbol-highlight-reset-range ()
        "Reset the range for `auto-highlight-symbol'."
        (interactive)
        (ahs-change-range ahs-default-range))

      (qingeditor/key-binder/set-leader-keys
       "sh" 'qing-symbol-highlight
       "sH" 'qing-goto-last-searched-ahs-symbol)

      ;; micro-state to easily jump from a highlighted symbol to the others
      (dolist (sym '(ahs-forward
                     ahs-forward-definition
                     ahs-backward
                     ahs-backward-definition
                     ahs-back-to-start
                     ahs-change-range))
        (let* ((advice (intern (format "qingeditor/editor-editing-visual/%s-adv" (symbol-name sym)))))
          (eval `(defadvice ,sym (around ,advice activate)
                   (qingeditor/editor-editing-visual/ahs-highlight-now-wrapper)
                   ad-do-it
                   (qingeditor/editor-editing-visual/ahs-highlight-now-wrapper)
                   (setq qingeditor/editor-editing-visual/last-ahs-highlight-p (ahs-highlight-p))))))

      (defun qingeditor/editor-editing-visual/symbol-highlight-doc ()
        (let* ((i 0)
               (overlay-count (length ahs-overlay-list))
               (overlay (format "%s" (nth i ahs-overlay-list)))
               (current-overlay (format "%s" ahs-current-overlay))
               (st (ahs-stat))
               (plighter (ahs-current-plugin-prop 'lighter))
               (plugin (format "%s"
                               (cond ((string= plighter "HS") "Display")
                                     ((string= plighter "HSA") "Buffer")
                                     ((string= plighter "HSD") "Function"))))
               (face (cond ((string= plighter "HS") ahs-plugin-defalt-face)
                           ((string= plighter "HSA") ahs-plugin-whole-buffer-face)
                           ((string= plighter "HSD") ahs-plugin-bod-face))))
          (while (not (string= overlay current-overlay))
            (setq i (i+ i))
            (setq overlay (format "%s" (nth i ahs-overlay-list))))
          (let* ((x/y (format "[%s/%s]" (- overlay-count i) overlay-count))
                 (hidden (if (< 0 (- overlay-count (nth 4 st))) "*" "")))
            (concat
             (propertize (format " %s " plugin) 'face face)
             (propertize (format " %s%s " x/y hidden) 'face
                         `(:foreground "#ffffff" :background "#000000"))))))

      (defun qing-ahs-to-iedit ()
        (interactive)
        (when (qingeditor/modulemgr/package-usedp 'iedit)
          (iedit-mode)
          (iedit-restrict-region (ahs-current-plugin-prop 'start)
                                 (ahs-current-plugin-prop 'end)))
        (ahs-edit-mode t))

      ;; transient state
      (defun qingeditor/editor-editing-visual/symbol-highlight-ts-doc ()
        (qingeditor/transient-state-make-doc
         'symbol-highlight
         (format qingeditor/editor-editing-visual/symbol-highlight-state-doc
                 (symbol-highlight-doc)
                 (make-string (length (symbol-highlight-doc)) 32))))

      (qingeditor/define-transient-state symbol-highlight
        :title "Symbol Highlight Transient State"
        :dynamic-hint (qingeditor/editor-editing-visual/symbol-highlight-ts-doc)
        :before-exit  (qingeditor/editor-editing-visual/ahs-ms-on-exit)
        :bindings
        ("d" ahs-forward-definition)
        ("D" ahs-backward-definition)
        ("e" ahs-to-iedit :exit t)
        ("n" qing-quick-ahs-forward)
        ("N" qing-quick-ahs-backward)
        ("p" qing-quick-ahs-backward)
        ("R" ahs-back-to-start)
        ("r" ahs-change-range)
        ("q" nil :exit t)))))

(defun qingeditor/editor-editing-visual/init-column-enforce-mode ()
  (use-package column-enforce-mode
    :commands (column-enforce-mode global-column-enforce-mode)
    :init
    (progn
      (qingeditor/add-toggle highlight-long-lines
        :status column-enforce-mode
        :prefix columns
        :on (column-enforce-n (or columns column-enforce-column))
        :on-message (format "long-lines enabled for %s columns." (or columns column-enforce-column))
        :off (column-enforce-mode -1)
        :documentation "Highlighting the characters past the 80th column."
        :leader "t8")

      (qingeditor/add-toggle highlight-long-lines-globally
        :mode global-column-enforce-mode
        :documentation "Globally Highlighting the characters past the 80th columns."
        :leader "t C-8")
      (qingeditor/font/diminish column-enforce-mode "⑧" "8"))))

(defun qingeditor/editor-editing-visual/init-hide-comnt ()
  (use-package hide-comnt
    :commands hide/show-comments-toggle
    :init
    (progn
      (qingeditor/key-binder/set-leader-keys "ch" #'hide/show-comments-toggle))))

(defun qingeditor/editor-editing-visual/init-highlight-indentation ()
  (use-package highlight-identation
    :defer t
    :init
    (progn
      (qingeditor/add-toggle highlight-identation
        :mode highlight-identation-mode
        :documentation "Highlight identation levels."
        :leader "thi")
      (qingeditor/add-toggle highlight-indentation-current-column
        :mode highlight-indentation-current-column-mode
        :documentation "Highlighting identation level at point."
        :leader "thc")

      (qingeditor/font/diminish highlight-identation-mode " ⓗi" " hi")
      (qingeditor/font/diminish highlight-indentation-current-column-mode " ⓗc" " hc"))))

(defun qingeditor/editor-editing-visual/init-highlight-numbers ()
  (use-package highlight-numbers
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook #'highlight-numbers-mode)
      (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1))))))

(defun qingeditor/editor-editing-visual/init-highlight-parentheses ()
  (use-package highlight-parentheses
    :defer t
    :init
    (progn
      (when (member qingeditor/config/highlight-delimiters '(all current))
        (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
      (setq hl-paren-delay 0.2)
      (qingeditor/key-binder/set-leader-keys "tCp" #'highlight-parentheses-mode)
      (setq hl-paren-colors '("Springgreen3"
                              "IndianRed1"
                              "IndianRed3"
                              "IndianRed4")))
    :config
    (progn
      (qingeditor/font/hide-lighter highlight-parentheses-mode)
      (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))))

(defun qingeditor/editor-editing-visual/init-hl-anything ()
  (use-package hl-anything
    :init
    (progn
      (hl-highlight-mode)
      (setq-default hl-highlight-save-file
                    (concat qingeditor/cache-dir ".hl-save"))
      (qingeditor/key-binder/set-leader-keys
       "hc" #'hl-unhighlight-all-local
       "hC" #'hl-unhighlight-all-global
       "hh" #'hl-highlight-thingatpt-local
       "hH" #'hl-highlight-thingatpt-global
       "hn" #'hi-find-next-thing
       "hN" #'hi-find-prev-thing
       "hr" #'hl-restore-highlights
       "hs" #'hl-save-highlights))
    :config (qingeditor/font/hide-lighter hl-highlight-mode)))

(defun qingeditor/editor-editing-visual/init-indent-guide ()
  (use-package indent-guide
    :defer t
    :init
    (progn
      (setq indent-guide-delay 0.3)
      (qingeditor/add-toggle indent-guide
        :mode indent-guide-mode
        :documentation "Highlighting indentation level at point. (alternative to highlight-indentation)."
        :leader "ti")

      (qingeditor/add-toggle indent-guide-globally
        :mode indent-guide-global-mode
        :documentation "Highlighting indentation level at point globally. (alternative to highlight-indentation)."
        :leader "t TAB")
      (qingeditor/font/diminish indent-guide-mode " ⓘ" " i"))))

(defun qingeditor/editor-editing-visual/init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :init
    (progn
      (qingeditor/key-binder/set-leader-keys "tCd" #'rainbow-delimiters-mode)
      (when (member qingeditor/config/highlight-delimiters '(any all))
        (qingeditor/add-to-hooks 'rainbow-delimiters-mode '(prog-mode-hook))))))

(defun qingeditor/editor-editing-visual/init-volatile-highlights ()
  (use-package volatile-highlights
    :config
    (progn
      ;; addtional extensions
      ;; undo-tree
      (vhl/define-extension 'undo-tree
                            'undo-tree-move
                            'undo-tree-yank)
      (with-eval-after-load 'undo-tree
        (vhl/install-extension 'undo-tree)
        (vhl/load-extension 'undo-tree))
      (volatile-highlights-mode)
      (qingeditor/font/hide-lighter volatile-highlights-mode))))
