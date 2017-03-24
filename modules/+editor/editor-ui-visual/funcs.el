;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

;; golden ratio
(defun qingeditor/editor-ui-visual/no-golden-artio-for-buffers (bufname)
  "Disable golden-ratio if `bufname' is the name of a visible buffer."
  (and (get-buffer bufname) (get-buffer-window bufname 'visible)))

(defun qinegditor/editor-ui-visual/no-golden-ratio-guide-key ()
  "Disable golden-ratio for guide-key popwin buffer."
  (or (qingeditor/editor-ui-visual/no-golden-artio-for-buffers " *guide-key*")
      (qingeditor/editor-ui-visual/no-golden-artio-for-buffers " *popwin-dummy*")))

;; ansi-colors
(defun qingeditor/editor-ui-visual/compilation-buffer-apply-ansi-colors ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

;; neotree

(defun qing-neotree-expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when neo-auto-indent-point
              (next-line)
              (neo-point-auto-indent)))
        (call-interactively 'neotree-enter)))))

(defun qing-neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p nide)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when neo-auto-indent-point
        (neo-point-auto-indent)))))

(defun qing-neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (qing-neotree-collapse)
            (neotree-select-up-node))
        (neotree-select-up-node)))))

(defun qing-neotree-find-project-root ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((origin-buffer-file-name (buffer-file-name)))
      (neotree-find (projectile-project-root))
      (neotree-find origin-buffer-file-name))))

(defun qingeditor/editor-ui-visual/neotree-maybe-attach-window ()
  (when (get-buffer-window (neo-global--get-buffer))
    (neo-global--attach)))

;; smooth scrolling
(defun qing-enable-smooth-scrolling ()
  "Enable smooth scrolling."
  (interactive)
  (setq scroll-conservatively 101))

(defun qing-disable-smooth-scrolling ()
  "Disable smooth scrolling."
  (interactive)
  (setq scroll-conservatively 0))

;; spaceline

(defun qingeditor/editor-ui-visual/customize-powerline-faces ()
  "Alter powerline face to make them work with more themes."
  (when (boundp 'powerline-inactive2)
    (set-face-attribute 'powerline-inactive2 nil
                        :inherit 'font-lock-comment-face)))

;; spaceline
(defun qingeditor/editor-ui-visual/customize-powerline-faces ()
  "Alter powerline face to make them work with more themes."
  (when (boundp 'powerline-inactive2)
    (set-face-attribute 'powerline-inactive2 nil
                        :inherit 'font-lock-comment-face)))

(defun qingeditor/editor-ui-visual/prepare-diminish ()
  (when spaceline-minor-modes-p
    (let ((unicodep (qingeditor/symbol-value
                     qingeditor/config/show-mode-line-unicode-symbols)))
      (setq spaceline-minor-modes-separator
            (if unicodep (if (display-graphic-p) "" " ") "|"))
      (dolist (mm qingeditor/font/diminished-minor-modes)
        (let ((mode (car mm)))
          (when (and (boundp mode) (symbol-value mode))
            (let* ((unicode (cadr mm))
                   (ascii (caddr mm))
                   (dim (if unicodep
                            unicode
                          (if ascii ascii unicode))))
              (diminish mode dim))))))))

(defun qingeditor/editor-ui-visual/set-powerline-for-startup-buffers ()
  "Set the powerline for buffers created when Emacs starts."
  (dolist (buffer '("*Messages*" "*qingeditor*" "*Compile-Log*"))
    (when (and (get-buffer buffer)
               (qingeditor/modulemgr/package-usedp 'spaceline))
      (qingeditor/editor-ui-visual/restore-powerline buffer))))

(defun qingeditor/editor-ui-visual/restore-powerline (buffer)
  "Restore the powerline in buffer"
  (with-current-buffer buffer
    (setq-local mode-line-format (default-value 'mode-line-format))
    (powerline-set-selected-window)
    (powerline-reset)))
