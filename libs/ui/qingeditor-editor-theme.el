;; 编辑器相关的主题设置

(defvar qingeditor/ui/editor-theme/emacs-build-in-themes
  (cons 'default (custom-available-themes)) "当前版本Emacs支持的所有的主题列表。")

(defvar qingeditor/ui/editor-theme/theme-name-to-package-alist
  '(
    (alect-black-alt . alect-themes)
    (alect-black     . alect-themes)
    (alect-dark-alt  . alect-themes)
    (alect-dark      . alect-themes)
    (alect-light-alt . alect-themes)
    (alect-light     . alect-themes)
    (ample-light . ample-theme)
    (ample-flat  . ample-theme)
    (apropospriate-light . apropospriate-theme)
    (apropospriate-dark  . apropospriate-theme)
    (base16-3024                . base16-theme)
    (base16-apathy              . base16-theme)
    (base16-ashes               . base16-theme)
    (base16-atelier-cave        . base16-theme)
    (base16-atelier-dune        . base16-theme)
    (base16-atelier-estuary     . base16-theme)
    (base16-atelier-forest      . base16-theme)
    (base16-atelier-heath       . base16-theme)
    (base16-atelier-lakeside    . base16-theme)
    (base16-atelier-plateau     . base16-theme)
    (base16-atelier-savanna     . base16-theme)
    (base16-atelier-seaside     . base16-theme)
    (base16-atelier-sulphurpool . base16-theme)
    (base16-bespin              . base16-theme)
    (base16-brewer              . base16-theme)
    (base16-bright              . base16-theme)
    (base16-chalk               . base16-theme)
    (base16-codeschool          . base16-theme)
    (base16-darktooth           . base16-theme)
    (base16-default-dark        . base16-theme)
    (base16-default-light       . base16-theme)
    (base16-eighties            . base16-theme)
    (base16-embers              . base16-theme)
    (base16-flat                . base16-theme)
    (base16-github              . base16-theme)
    (base16-google-dark         . base16-theme)
    (base16-google-light        . base16-theme)
    (base16-grayscale-dark      . base16-theme)
    (base16-grayscale-light     . base16-theme)
    (base16-green-screen        . base16-theme)
    (base16-harmonic16-dark     . base16-theme)
    (base16-harmonic16-light    . base16-theme)
    (base16-hopscotch           . base16-theme)
    (base16-ir-black            . base16-theme)
    (base16-isotope             . base16-theme)
    (base16-london-tube         . base16-theme)
    (base16-macintosh           . base16-theme)
    (base16-marrakesh           . base16-theme)
    (base16-mocha               . base16-theme)
    (base16-monokai             . base16-theme)
    (base16-ocean               . base16-theme)
    (base16-oceanicnext         . base16-theme)
    (base16-paraiso             . base16-theme)
    (base16-phd                 . base16-theme)
    (base16-pico                . base16-theme)
    (base16-pop                 . base16-theme)
    (base16-railscasts          . base16-theme)
    (base16-seti-ui             . base16-theme)
    (base16-shapeshifter        . base16-theme)
    (base16-solar-flare         . base16-theme)
    (base16-solarized-dark      . base16-theme)
    (base16-solarized-light     . base16-theme)
    (base16-summerfruit-dark    . base16-theme)
    (base16-summerfruit-light   . base16-theme)
    (base16-tomorrow-night      . base16-theme)
    (base16-tomorrow            . base16-theme)
    (base16-twilight            . base16-theme)
    (base16-unikitty-dark       . base16-theme)
    (base16-unikitty-light      . base16-theme)
    (sanityinc-solarized-dark    . color-theme-sanityinc-solarized)
    (sanityinc-solarized-light   . color-theme-sanityinc-solarized)
    (sanityinc-tomorrow-blue     . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-bright   . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-day      . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-eighties . color-theme-sanityinc-tomorrow)
    (sanityinc-tomorrow-night    . color-theme-sanityinc-tomorrow)
    (doom-one     . doom-themes)
    (doom-molokai . doom-themes)
    (solarized-light . solarized-theme)
    (solarized-dark . solarized-theme)
    (spacemacs-light . spacemacs-theme)
    (spacemacs-dark . spacemacs-theme)
    (colorsarenice-dark  . colorsarenice-theme)
    (colorsarenice-light . colorsarenice-theme)
    (hemisu-dark  . hemisu-theme)
    (hemisu-light . hemisu-theme)
    (majapahit-dark . majapahit-theme)
    (majapahit-light . majapahit-theme)
    (material-light . material-theme)
    (minimal-light . minimal-theme)
    (moe-dark  . moe-theme)
    (moe-light . moe-theme)
    (stekene-dark  . stekene-theme)
    (stekene-light . stekene-theme)
    (brin     . sublime-themes)
    (dorsey   . sublime-themes)
    (fogus    . sublime-themes)
    (graham   . sublime-themes)
    (granger  . sublime-themes)
    (hickey   . sublime-themes)
    (junio    . sublime-themes)
    (mccarthy . sublime-themes)
    (odersky  . sublime-themes)
    (omtose-darker . omtose-phellack-theme)
    (omtose-softer . omtose-phellack-theme)
    (ritchie  . sublime-themes)
    (spolsky  . sublime-themes)
    (wilson   . sublime-themes)
    (zonokai-blue . zonokai-theme)
    (zonokai-red  . zonokai-theme)
    (tao-yin . tao-theme)
    (tao-yang . tao-theme)
    (farmhouse-light . farmhouse-theme)
    (farmhouse-dark . farmhouse-theme))
  "主题的名字跟ELPA程序包之间的映射。")

(defvar qingeditor/ui/editor-theme/cur-theme-private nil
  "当前系统使用的使用的主题名称。")

(defvar qingeditor/ui/editor-theme/cycle-themes-private nil
  "周期切换主题列表。")

(defface qingeditor/ui/org-kbd
  '((t (:background "LemonChiffon1" :foreground "black" :box
                    (:line-width 2 :color nil :style release-button))))
  "在QingeditorEditor文档中显示key binding的face设置。"
  :group 'org-faces)

(defun qingeditor/ui/editor-theme/load-theme (theme)
  "加载指定的主题程序包。"
  ;; 在这里指定的主题包可能依赖其他的主题包
  (condition-case-unless-debug err
      (progn
        (when (or (eq 'zonokai-blue theme)
                  (eq 'zonokai-red theme)
                  (eq 'solarized-light theme)
                  (eq 'solarized-dark theme))
          (qingeditor/pkg/installer/load-or-install-package 'dash))
        ;; 在是内置的主题并且不是默认的主题
        (unless (or (memq theme (custom-available-themes))
                    (eq 'default theme))
          (cond
           ;; 显示定义的主题的ELPA软件包的名称
           ((assq theme qingeditor/ui/editor-theme/theme-name-to-package-alist)
            (let* ((pkg (qingeditor/ui/editor-theme/get-theme-pkg-by-name theme))
                   (pkg-dir (qingeditor/pkg/installer/load-or-install-package pkg)))
              (when (or (eq 'moe-light theme)
                        (eq 'moe-dark theme))
                (load-file (concat pkg-dir "moe-light-theme.el"))
                (load-file (concat pkg-dir "moe-dark-theme.el")))
              (add-to-list 'custom-theme-load-path pkg-dir)))
           (t
            ;; 系统默认认为主题的包名称以`-theme'结尾，在这里我们不处理其他特殊的情况
            (let ((pkg (qingeditor/ui/editor-theme/get-theme-pkg-by-name theme)))
              (qingeditor/ui/editor-theme/load-or-install-package pkg))))))
    ('error
     (setq theme 'default)
           (display-warning
            'qingeditoreditor
            (format "加载主题包出错，系统将使用默认主题，错误信息：%s" err)
            :warning)))
  (mapc 'disable-theme custom-enabled-themes)
  (if (eq 'default theme)
      ;; 如果是默认的就不需要加载了，设置相关标志变量然后执行钩子函数就可以了
      (progn
        (setq qingeditor/ui/editor-theme/cur-theme-private 'default)
        (qingeditor/ui/editor-theme/run-post-theme-init 'default))
    (load-theme theme t)
    ;; 当GUI程序第一次加载的时候主动重新加载theme
    (eval `(qingeditor/ui/do-after-display-system-init
            (load-theme ',theme t)))))

(defun qingeditor/ui/editor-theme/run-post-theme-init (theme)
  "在主题加载之后我们通常是需要做一些处理的，我们可以将处理函数放在钩子函数里面。"
  (run-hooks 'qingeditor/gvars/post-theme-change-hook))

(defun qingeditor/ui/run-post-theme-init (theme)
  "在主题加载之后我们通常是需要做一些处理的，我们可以将处理函数放在钩子函数里面。
暂时参数`theme'没有使用。"
  (interactive)
  (qingeditor-run-post-theme-init qingeditor/ui/editor-theme theme))

(defun qingeditor/ui/editor-theme/get-theme-pkg-by-name (theme)
  "通过主题的名称获取这个主题的ELPA的软件包的名称。"
  (cond
   ;; 首先判断是否是内置的主题
   ((memq theme qingeditor/ui/editor-theme/emacs-build-in-themes)
    nil)
   ;; 判断是否在我们的QingeditorEditor的主题列表里面
   ((assq theme qingeditor/ui/editor-theme/theme-name-to-package-alist)
    (cdr (assq theme qingeditor/ui/editor-theme/theme-name-to-package-alist)))
   ;; 容错处理情况
   (t (intern (format "%S-theme" theme)))))

(provide 'qingeditor-editor-theme)
