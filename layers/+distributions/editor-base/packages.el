(setq qingeditor-editor-base-packages
  '(
    (abbrev :location built-in)
    ace-window
    (archive-mode :location built-in)
    (bookmark :location built-in)
    (centered-buffer-mode :location local)
    (conf-mode :location built-in)
    (dired :location built-in)
    (dired-x :location built-in)
    (electric-indent-mode :location built-in)
    (ediff :location built-in)
    (eldoc :location built-in)
    evil-escape
    (evil-evilified-state :location local :step pre :protected t)
    evil-visualstar
    (exec-path-from-shell :step pre)
    help-fns+
    (hi-lock :location built-in)
    (holy-mode :location local :step pre)
    (hybrid-mode :location local :step pre)
    (image-mode :location built-in)
    (imenu :location built-in)
    (linum :location built-in)
    (occur-mode :location built-in)
    (package-menu :location built-in)
    ;; 这个已经在内核中包含
    (page-break-lines :location built-in)
    pcre2el
    (process-menu :location built-in)
    projectile
    (recentf :location built-in)
    (savehist :location built-in)
    (saveplace :location built-in)
    spacemacs-theme
    (subword :location built-in)
    (tar-mode :location built-in)
    (tar-mode :location built-in)
    (uniquify :location built-in)
    (url :location built-in)
    (visual-line-mode :location built-in)
    (whitespace :location built-in)
    (winner :location built-in)))

;; 初始化packages

(defun qingeditor/editor-base/init-abbrev ()
  (qingeditor/ui/editor-font/hide-lighter abbrev-mode))

(defun qingeditor/editor-base/init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (progn
      (qingeditor/core/key-binder/set-leader-keys
       "bD"
       ;; FIXME: Needs new binding.
       ;; "wC" 'qingeditor/ace-center-window
       "wD"
       "wM" 'ace-swap-window
       "wW" 'ace-window)
      ;; 设置`ace-window'按键到`home-row'
      (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))))

(defun qingeditor/editor-base/init-archive-mode ()
  ;;;(evilified-state-evilify-map
  ;; archive-mode-map
  ;; :mode archive-mode
  ;; :eval-after-load archive-mode)
  )

(defun qingeditor/editor-base/init-bookmark ())
(defun qingeditor/editor-base/init-centered-buffer-mode ())
(defun qingeditor/editor-base/init-conf-mode ())
(defun qingeditor/editor-base/init-dired ())
(defun qingeditor/editor-base/init-dired-x ())
(defun qingeditor/editor-base/init-electric-indent-mode ())
(defun qingeditor/editor-base/init-ediff ())
(defun qingeditor/editor-base/init-eldoc ())
(defun qingeditor/editor-base/init-evil-escape ())

(defun qingeditor/editor-base/init-evil-evilified-state ()
  ;; (use-package evil-evilified-state)
  ;; (define-key evil-qingeditor-evilified-state-map
  ;;   (kbd qingeditor/core/user-cfg/leader-key) qingeditor/core/key-binder/default-map)
  )

(defun qingeditor/editor-base/init-evil-visualstar ())
(defun qingeditor/editor-base/init-exec-path-from-shell ())
(defun qingeditor/editor-base/init-help-fns+ ())
(defun qingeditor/editor-base/init-hi-lock ())
(defun qingeditor/editor-base/init-holy-mode ())
(defun qingeditor/editor-base/init-hybrid-mode ())
(defun qingeditor/editor-base/init-image-mode ())
(defun qingeditor/editor-base/init-imenu ())
(defun qingeditor/editor-base/init-linum ())
(defun qingeditor/editor-base/init-occur-mode ())
(defun qingeditor/editor-base/init-package-menu ())
(defun qingeditor/editor-base/init-page-break-lines ())
(defun qingeditor/editor-base/init-pcre2el ())
(defun qingeditor/editor-base/init-process-menu ())
(defun qingeditor/editor-base/init-projectile ())
(defun qingeditor/editor-base/init-recentf ())
(defun qingeditor/editor-base/init-savehist ())
(defun qingeditor/editor-base/init-saveplace ())
(defun qingeditor/editor-base/init-spacemacs-theme ())
(defun qingeditor/editor-base/init-subword ())
(defun qingeditor/editor-base/init-tar-mode ())
(defun qingeditor/editor-base/init-uniquify ())
(defun qingeditor/editor-base/init-url ())
(defun qingeditor/editor-base/init-visual-line-mode ())
(defun qingeditor/editor-base/init-whitespace ())
(defun qingeditor/editor-base/init-winner ())
