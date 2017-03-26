;; Copyright (c) 2016-2017 zzu_softboy & Contributors
;;
;; Author: zzu_softboy <zzu_softboy@163.com>
;; Github: https://www.github.com/qingeditor/qingeditor
;;
;; This file is not part of GNU Emacs.
;; License: GPLv3

(defun qingeditor/helm/init-ace-jump-helm-line ()
  (use-package ace-jump-helm-line
    :defer t
    :init
    (with-eval-after-load 'helm
      (define-key helm-map (kbd "C-q") 'ace-jump-helm-line))))

(defun qingeditor/helm/init-auto-highlight-symbol ()
  (qingeditor/use-package-add-hook auto-highlight-symbol
    :post-init
    ;; add some functions to ahs transient states
    (setq qing-symbol-highlight-transient-state-doc
          (concat qing-symbol-highlight-transient-state-doc
                  "  [_b_] search buffers [_/_] search proj [_f_] search files")
          qing-symbol-highlight-transient-state-add-bindings
          '(("/" qing-helm-project-smart-do-search-region-or-symbol :exit t)
            ("b" qing-helm-buffers-smart-do-search-region-or-symbol :exit t)
            ("f" qing-helm-files-smart-do-search-region-or-symbol :exit t)))))

(defun qingeditor/helm/post-init-bookmark ()
  (qingeditor/key-binder/set-leader-keys "fb" #'helm-filtered-bookmarks))

(defun qingeditor/helm/init-helm ()
  (use-package helm
    :defer 0.5
    :commands (qing-helm-find-files)
    :init
    (progn
      (add-hook 'helm-cleanup-hook #'qingeditor/helm/helm-cleanup)
      ;; key bindings
      ;; Use helm to provide :ls, unless ibuffer is used
      (unless (qingeditor/modulemgr/package-usedp 'smex)
        (global-set-key (kbd "M-x") 'helm-M-x))
      (global-set-key (kbd "C-x C-f") #'qing-helm-find-files)
      (global-set-key (kbd "C-x b") #'helm-buffers-list)
      ;; use helm everywhere
      (qingeditor/key-binder/set-leader-keys
       "<f1>"   #'helm-apropos
       "a'"     #'qing-helm-available-repls
       "bb"     #'helm-mini
       "Cl"     #'helm-colors
       "ff"     #'qing-helm-find-files
       "fF"     #'helm-find-files
       "fL"     #'helm-locate
       "fr"     #'helm-recentf
       "hdd"    #'helm-apropos
       "hdF"    #'qing-helm-faces
       "hi"     #'helm-info-at-point
       "hm"     #'helm-man-woman
       "iu"     #'helm-ucs
       "jI"     #'helm-imenu-in-all-buffers
       "rm"     #'helm-all-mark-rings
       "rl"     #'helm-resume
       "rr"     #'helm-register
       "rs"     #'qing-resume-last-search-buffer
       "ry"     #'helm-show-kill-ring
       "sl"     #'qing-resume-last-search-buffer
       "sj"     #'qing-helm-jump-in-buffer)

      ;; search with grep
      (qingeditor/key-binder/set-leader-keys
       "sgb"  #'qing-helm-buffers-do-grep
       "sgB"  #'qing-helm-buffers-do-grep-region-or-symbol
       "sgf"  #'qing-helm-files-do-grep
       "sgF"  #'qing-helm-files-do-grep-region-or-symbol
       "sgg"  #'qing-helm-file-do-grep
       "sgG"  #'qing-helm-file-do-grep-region-or-symbol)

      ;; various key bindings
      (qingeditor/set-helm-key "fel" helm-locate-library)
      (qingeditor/set-helm-key "hdm" describe-mode)
      (qingeditor/set-helm-key "sww" helm-wikipedia-suggest)
      (qingeditor/set-helm-key "swg" helm-google-suggest)

      (with-eval-after-load 'helm-files
        (define-key helm-find-files-map
          (kbd "C-c C-e") 'qing-helm-find-files-edit))
      ;; Add minibuffer history with `helm-minibuffer-history'
      (define-key minibuffer-local-map (kbd "C-c C-l") #'helm-minibuffer-history)
      ;; define the key binding at the very end in order to allow the user
      ;; to overwrite any key binding
      (add-hook 'emacs-startup-hook
                (lambda ()
                  (unless (qingeditor/modulemgr/package-usedp 'smex)
                    (qingeditor/key-binder/set-leader-keys
                     qingeditor/config/command-key #'helm-M-x)))))
    :config
    (progn
      (helm-mode)
      (advice-add 'helm-grep-save-results-1 :after #'qingeditor/helm/gne-init-helm-grep)
      ;; helm-locate uses es (from everything on windows which doesnt like fuzzy)
      (helm-locate-set-command)
      (setq helm-locate-fuzzy-match (string-match "locate" helm-locate-command))
      ;; alter helm-bookmark key bindings to be simpler
      ;; TODO check if there is a more elegant solution to setup these bindings
      (defun qingeditor/helm/simpler-helm-bookmark-keybindings ()
        (define-key helm-bookmark-map (kbd "C-d") #'helm-bookmark-run-delete)
        (define-key helm-bookmark-map (kbd "C-e") #'helm-bookmark-run-edit)
        (define-key helm-bookmark-map
          (kbd "C-f") #'helm-bookmark-toggle-filename)
        (define-key helm-bookmark-map
          (kbd "C-o") #'helm-bookmark-run-jump-other-window)
        (define-key helm-bookmark-map (kbd "C-/") #'helm-bookmark-help))
      (add-hook 'helm-mode-hook #'qingeditor/helm/simpler-helm-bookmark-keybindings)
      (with-eval-after-load 'helm-mode ; required
        (qingeditor/font/hide-lighter helm-mode)))))

(defun qingeditor/helm/init-helm-ag ()
  (use-package helm-ag
    :defer t
    :init
    (progn
      (defun qingeditor/helm/do-ag-region-or-symbol (func &optional dir)
        "Search with `ag' with a default input."
        (require 'helm-ag)
        (cl-letf*
            (((symbol-value 'helm-ag-insert-at-point) 'symbol)
             ;;  make thing-at-point choosing the active region first
             ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
             ((symbol-function 'thing-at-point)
              (lambda (thing)
                (let ((res (if (region-active-p)
                               (buffer-substring-no-properties
                                (region-beginning)
                                (region-end))
                             (this-fn thing))))
                  (when res (rxt-quote-pcre res))))))
          (funcall func dir)))

      (defun qingeditor/helm/do-search-find-tool (base tools default-inputp)
        "Create a cond form given a TOOLS string list and evaluate it."
        (eval
         `(cond
           ,@(mapcar
              (lambda (x)
                `((executable-find ,x)
                  ',(let ((func
                           (intern
                            (format (if default-inputp
                                        "qing-%s-%s-region-or-symbol"
                                      "qing-%s-%s")
                                    base x))))
                      (if (fboundp func)
                          func
                        (intern (format "%s-%s" base x))))))
              tools)
           (t 'helm-do-grep))))

      ;; Search in current file ----------------------------------------------
      (defun qing-helm-file-do-ag (&optional _)
        "Wrapper to execute `helm-ag-this-file'."
        (interactive)
        (helm-ag-this-file))

      (defun qing-helm-file-do-ag-region-or-symbol ()
        "Search in current file with `ag' using a default input."
        (interactive)
        (qingeditor/helm/do-ag-region-or-symbol #'qing-helm-file-do-ag))

      (defun qing-helm-file-smart-do-search (&optional default-inputp)
        "Search in current file using `qingeditor/config/search-tools'.
Search for a search tool in the order provided by `qingeditor/config/search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         qingeditor/helm/do-search-find-tool "helm-file-do"
         qingeditor/config/search-tools
         default-inputp))

      (defun qing-helm-file-smart-do-search-region-or-symbol ()
        "Search in current file using `qingeditor/config/search-tools' with
 default input.
Search for a search tool in the order provided by `qingeditor/config/search-tools'."
        (interactive)
        (qing-helm-file-smart-do-search t))

      ;; Search in files -----------------------------------------------------
      (defun qing-helm-files-do-ag (&optional dir)
        "Search in files with `ag' using a default input."
        (helm-do-ag dir))

      (defun qing-helm-files-do-ag-region-or-symbol ()
        "Search in files with `ag' using a default input."
        (interactive)
        (qingeditor/helm/do-ag-region-or-symbol #'qing-helm-files-do-ag))

      (defun qing-helm-files-do-ack (&optional dir)
        "Search in files with `ack'."
        (interactive)
        (let ((helm-ag-base-command "ack --nocolor --nogroup"))
          (helm-do-ag dir)))

      (defun qing-helm-files-do-ack-region-or-symbol ()
        "Search in files with `ack' using a default input."
        (qingeditor/helm/do-ag-region-or-symbol #'qing-helm-files-do-ack))

      (defun qing-helm-files-do-pt (&optional dir)
        "Search in files with `pt'."
        (interactive)
        (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
          (helm-do-ag dir)))

      (defun qing-helm-files-do-pt-region-or-symbol ()
        "Search in files with `pt' using a default input."
        (interactive)
        (qingeditor/helm/do-ag-region-or-symbol #'qing-helm-files-do-pt))

      (defun qing-helm-files-smart-do-search (&optional default-inputp)
        "Search in opened buffers using `qingeditor/config/search-tools'.
Search for a search tool in the order provided by `qingeditor/config/search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (qingeditor/helm/do-search-find-tool
          "helm-files-do"
          qingeditor/config/search-tools
          default-inputp)))

      (defun qing-helm-files-smart-do-search-region-or-symbol ()
        "Search in opened buffers using `qingeditor/config/search-tools'.
with default input.
Search for a search tool in the order provided by `qingeditor/config/search-tools'."
        (qing-helm-files-smart-do-search t))

      ;; Search in buffers ---------------------------------------------------

      (defun qing-helm-buffers-do-ag (&optional _)
        "Wrapper to execute `helm-do-ag-buffers'."
        (interactive)
        (helm-do-ag-buffers))

      (defun qing-helm-buffers-do-ag-region-or-symbol ()
        "Search in opened buffers with `ag' with a default input."
        (interactive)
        (qingeditor/helm/do-ag-region-or-symbol #'qing-helm-buffers-do-ag))

      (defun qing-helm-buffers-do-ack (&optional _)
        "Search in opened buffers with `ack'."
        (interactive)
        (let ((helm-ag-base-command "ack --nocolor --nogroup"))
          (helm-do-ag-buffers)))

      (defun qing-helm-buffers-do-ack-region-or-symbol ()
        "Search in opened buffers with `ack' with a default input."
        (qingeditor/helm/do-ag-region-or-symbol #'qing-helm-buffers-do-ack))

      (defun qing-helm-buffers-do-pt (&optional _)
        "Search in opened buffers with `pt'."
        (interactive)
        (let ((helm-ag-base-commad "pt -e --nocolor --nogroup"))
          (helm-do-ag-buffers)))

      (defun qing-helm-buffers-do-pt-region-symbol ()
        "Search in opened buffers with `pt' using a default input."
        (interactive)
        (qingeditor/helm/do-ag-region-or-symbol #'qing-helm-buffers-do-pt))

      (defun qing-helm-buffers-smart-do-search (&optional default-inputp)
        "Search in opened buffers using `qingeditor/config/search-tools'.
Search for a search tool in the order provided by `qingeditor/config/search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (qingeditor/helm/do-search-find-tool "helm-buffers-do"
                                              qingeditor/config/search-tools
                                              default-inputp)))

      (defun qing-helm-buffers-smart-do-search-region-or-symbol ()
        "Search in opened buffers using `qingeditor/config/search-tools' with
default input.
Search for a search tool in the order provided by `qingeditor/config/search-tools'."
        (interactive)
        (qing-helm-buffers-smart-do-search t))

      ;; Search in project ---------------------------------------------------

      (defun qing-helm-project-do-ag ()
        "Search in current project with `ag'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (helm-do-ag dir)
            (message "error: Not in a project."))))

      (defun qing-helm-project-do-ag-region-or-symbol ()
        "Search in current project with `ag' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (qingeditor/helm/do-ag-region-or-symbol #'helm-do-ag dir)
            (message "error: Not in a project."))))

      (defun qing-helm-project-do-ack ()
        "Search in current project with `ack'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (qing-helm-files-do-ack dir)
            (message "error: Not in a project."))))

      (defun qing-helm-project-do-ack-region-or-symbol ()
        "Search in current project with `ack' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (qingeditor/helm/do-ag-region-or-symbol
               #'qing-helm-files-do-ack dir)
            (message "error: Not in a project."))))

      (defun qing-helm-project-do-pt ()
        "Search in current project with `pt'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (qing-helm-files-do-pt dir)
            (message "erro: Not in a project."))))

      (defun qing-helm-project-do-pt-region-or-symbol ()
        "Search in current project with `pt' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (qingeditor/helm/do-ag-region-or-symbol
               #'qing-helm-files-do-pt dir)
            (message "error: Not in a project."))))

      (defun qing-helm-project-smart-do-search (&optional default-inputp)
        "Search in current project using `qingeditor/config/search-tools'.
Search for a search tool in the order provided by `qingeditor/config/search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (let ((projectile-require-project-root nil))
          (call-interactively
           (qing-helm-do-search-find-tool "helm-project-do"
                                          qingeditor/config/search-tools
                                          default-inputp))))

      (defun qing-helm-project-smart-do-search-region-or-symbol ()
        "Search in current project using `qingeditor/config/search-tools' with
 default input.
Search for a search tool in the order provided by `qingeditor/config/search-tools'."
        (interactive)
        (qing-helm-project-smart-do-search t))

      ;; This overrides the default C-s action in helm-projectile-switch-project
      ;; to search using ag/pt/whatever instead of just grep
      (with-eval-after-load 'helm-projectile
        (defun qing-helm-project-smart-do-search-in-dir (dir)
          (interactive)
          (let ((default-directory dir))
            (qing-helm-project-smart-do-search)))

        (define-key helm-projectile-projects-map
          (kbd "C-s")
          (lambda ()
            (interactive)
            (helm-exit-and-execute-action
             #'qing-helm-project-smart-do-search-in-dir))))

      (qingeditor/key-binder/set-leader-keys-for-minor-mode
       'helm-grep-mode
       (kbd "RET") 'helm-grep-mode-jump-other-window
       (kbd "q") 'quit-window)

      (qingeditor/key-binder/set-leader-keys
       ;; helm-ag marks
       "s`" #'helm-ag-pop-stack
       ;; opened buffers scope
       "sb"  #'qing-helm-buffers-smart-do-search
       "sB"  #'qing-helm-buffers-smart-do-search-region-or-symbol
       "sab" #'helm-do-ag-buffers
       "saB" #'qing-helm-buffers-do-ag-region-or-symbol
       "skb" #'qing-helm-buffers-do-ack
       "skB" #'qing-helm-buffers-do-ack-region-or-symbol
       "stb" #'qing-helm-buffers-do-pt
       "stB" #'qing-helm-buffers-do-pt-region-or-symbol
       ;; current file scope
       "ss"  #'qing-helm-file-smart-do-search
       "sS"  #'qing-helm-file-smart-do-search-region-or-symbol
       "saa" #'helm-ag-this-file
       "saA" #'qing-helm-file-do-ag-region-or-symbol
       ;; files scope
       "sf"  #'qing-helm-files-smart-do-search
       "sF"  #'qing-helm-files-smart-do-search-region-or-symbol
       "saf" #'helm-do-ag
       "saF" #'qing-helm-files-do-ag-region-or-symbol
       "skf" #'qing-helm-files-do-ack
       "skF" #'qing-helm-files-do-ack-region-or-symbol
       "stf" #'qing-helm-files-do-pt
       "stF" #'qing-helm-files-do-pt-region-or-symbol
       ;; current project scope
       "/"   #'qing-helm-project-smart-do-search
       "*"   #'qing-helm-project-smart-do-search-region-or-symbol
       "sp"  #'qing-helm-project-smart-do-search
       "sP"  #'qing-helm-project-smart-do-search-region-or-symbol
       "sap" #'qing-helm-project-do-ag
       "saP" #'qing-helm-project-do-ag-region-or-symbol
       "skp" #'qing-helm-project-do-ack
       "skP" #'qing-helm-project-do-ack-region-or-symbol
       "stp" #'qing-helm-project-do-pt
       "stP" #'qing-helm-project-do-pt-region-or-symbol))
    :config
    (progn
      (advice-add 'helm-ag--save-results :after 'qingeditor/helm/gne-init-helm-ag)
      (qingeditor/key-binder/set-leader-keys-for-minor-mode
       'helm-ag-mode
       (kbd "SPC") qingeditor/default-map
       (kbd "gr") #'helm-ag--update-save-results
       (kbd "q") #'quit-window))))

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

(defun qingeditor/helm/post-init-popwin ()
  )

(defun qingeditor/helm/post-init-projectile ()
  )
