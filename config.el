;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(recentf-mode 1)

(defun efs/display-startup-time ()
  (message "Emacs loaded with %d garbage collections." gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(push 'org straight-built-in-pseudo-packages)

(setq auto-save-default nil
      make-backup-files nil
      large-file-warning-threshold nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package sudo-edit
  :straight t)

(use-package diminish
  :straight t)

(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"))

(defun emacs-run-launcher ()
  "Creates a Run Launcher similar to KRunner, set to C-M-<SPC>"
  (interactive)
  (with-selected-frame 
    (make-frame '((name . "emacs-run-launcher")
                  (minibuffer . only)
                  (fullscreen . 0) ; no fullscreen
                  (undecorated . t) ; remove title bar
                  (auto-raise . t) ; focus on this frame
                  ;;(tool-bar-lines . 0)
                  ;;(menu-bar-lines . 0)
                  (internal-border-width . 10)
                  (width . 80)
                  (height . 11)))
                  (unwind-protect
                    (app-launcher-run-app)
                    (delete-frame))))

(use-package catppuccin-theme
  :straight t)
(straight-use-package 'doom-themes)
(load-theme 'catppuccin :no-confirm)

(use-package rainbow-delimiters
  :straight t
  :after prism
  :hook ((prog-mode html-ts-mode css-ts-mode) . rainbow-delimiters-mode)
  ((prism-mode prism-whitespace-mode) . rainbow-delimiters-mode))

(use-package prism
  :straight (prism :host github :repo "alphapapa/prism.el")
  :custom (prism-comments nil)
  :config
  (prism-set-colors
    :lightens '(0)
    :desaturations '(0)
    :colors (mapcar #'catppuccin-get-color '(red sapphire green yellow lavender peach mauve)))
  :hook
  (emacs-lisp-mode . prism-mode)
  (python-ts-mode . prism-whitespace-mode))

(use-package indent-bars
  :straight t
  :init
  (defun pkoa/treesit-parser-custom-lang (lang-mode-symbol)
    (when (and (treesit-available-p)
               (treesit-language-available-p lang-mode-symbol))
      (treesit-parser-create lang-mode-symbol)))
  (defun pkoa/org-simple-elisp-mode ()
    (if (string-prefix-p " *org-src-fontification:" (buffer-name))
  (delay-mode-hooks (emacs-lisp-mode))
  (emacs-lisp-mode)))
  (setf (alist-get "emacs-lisp" org-src-lang-modes) 'pkoa/org-simple-elisp)
  :hook
  (emacs-lisp-mode . (lambda () (pkoa/treesit-parser-custom-lang 'elisp)))
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-lists '(?\[ ?\())
  (indent-bars-treesit-wrap '((c argument_list parameter_list init_declarator parenthesized_expression)
                              (rust arguments parameters)
                              (python argument_list parameters
			                          list list_comprehension
			                          dictionary dictionary_comprehension
			                          parenthesized_expression subscript)
                              (elisp quote special_form function_definition)))
  (indent-bars-treesit-scope '((rust trait_item impl_item
                                     macro_definition macro_invocation
                                     struct_item enum_item mod_item
                                     const_item let_declaration
                                     function_item for_expression
                                     if_expression loop_expression
                                     while_expression match_expression
                                     match_arm call_expression
                                     token_tree token_tree_pattern
                                     token_repetition)))
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  :config
  (setopt
   indent-bars-color '(highlight :face-bg t :blend 0.8)
   indent-bars-pattern "."
   indent-bars-color-by-depth '(:palette ("#f38ba8" "#89b4fa" "#a6e3a1" "#fab387" "#cba6f7") :blend 0.8)
   indent-bars-highlight-current-depth '(:blend 1.0 :width 0.4 :pad 0.1 :pattern "!.!.!." :zigzag 0.1)
   indent-bars-pad-frac 0.3
   indent-bars-ts-highlight-current-depth '(no-inherit) ; equivalent to nil
   indent-bars-ts-color-by-depth '(no-inherit)
   indent-bars-ts-color '(inherit fringe :face-bg t :blend 0.2))
  :hook ((rust-ts-mode python-ts-mode c-ts-mode emacs-lisp-mode) . indent-bars-mode))

(use-package colorful-mode
  :diminish
  :straight t
  :custom
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :hook ((prog-mode css-ts-mode html-ts-mode org-mode text-mode) . colorful-mode))

(use-package fic-mode-xtra
  :straight '(fic-mode-xtra :host github :repo "PeanutKoa/fic-mode-xtra")
  :hook (prog-mode . fic-mode))

;; sets the font directly
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 125)
;; ligature support
(use-package ligature
  :straight t
  :init
  (setq jetbrains-full-ligatures '("--" "---" "==" "===" "!=" "!==" "=!="
                                   "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                                   "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                                   "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                                   "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                                   "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                                   "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                                   "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                                   "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                                   "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                                   "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                                   ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                                   "<:<" ";;;"))
  :config
  (ligature-set-ligatures 'prog-mode jetbrains-full-ligatures)
  (ligature-set-ligatures 'html-ts-mode jetbrains-full-ligatures)
  (ligature-set-ligatures 'mhtml-mode jetbrains-full-ligatures)
  (ligature-set-ligatures 'css-ts-mode jetbrains-full-ligatures)
  (global-ligature-mode t))

;;setup
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
;;turn off for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
		        eat-mode-hook
		        woman-mode-hook
                tldr-mode-hook
		        eww-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package ultra-scroll
  :straight t
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        redisplay-dont-pause t
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(use-package nerd-icons
  :straight t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package dashboard
  :straight t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-banner-logo-title "PeanutKoa's Emacs, Powered by Evil!")
  (setq dashboard-startup-banner "~/.emacs.d/banner.txt") 
  (setq dashboard-items '((recents   . 5)
  			  (bookmarks . 5)
  	                  (projects  . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(electric-pair-mode +1)

(use-package blimpy
  :straight (blimpy :host github :repo "progfolio/blimpy")
  :after (evil)
  :config
  (add-hook 'blimpy-before-typing-the-word-blimpy-in-emacs-hook
            (apply-partially #'evil-insert 1)))

(use-package general
  :straight t
  :after evil
  :config
  (general-evil-setup)
  (general-create-definer pkoa/leader
  :states '(normal insert visual emacs)
  :prefix "SPC"
  :non-normal-prefix "C-SPC"))
(use-package hydra
  :straight t)

(general-define-key
 "C-x M-x" 'redraw-display
 "<escape>" 'keyboard-escape-quit)

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(which-key-mode 1)
(setq which-key-idle-delay -1)

(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
	     ("C-h v" . helpful-variable)
	     ("C-h k"    .   helpful-key)
	     ("C-h x"  . helpful-command)))

(use-package jinx
  :straight t
  :bind (("M-$" . jinx-correct)
       ("C-M-$" . jinx-languages)))

(use-package mentor
  :straight t)

(use-package emms
  :straight t
  :init
  (emms-all)
  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-info-functions '(emms-info-native
                         emms-info-metaflac
                         emms-info-ogginfo))
  (emms-volume-change-function 'emms-volume-mpv-change)
  (emms-volume-mpv-method 'smart))

(use-package tldr
  :straight t)

(use-package treemacs
  :straight t
  :config
  (setq treemacs-position 'right)
  :defer t)

(use-package treemacs-evil
  :after (treemacs evil)
  :straight t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :straight t)

(use-package treemacs-magit
  :after (treemacs magit)
  :straight t)

(use-package treemacs-nerd-icons
  :straight t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package vertico
  :straight t
  :custom
  (vertico-cycle t)
  :bind
  (:map vertico-map
  	("C-j" . vertico-next)
  	("C-k" . vertico-previous))
  :init
  (vertico-mode))

(use-package consult
  :straight t)

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package embark
  :straight t
  :bind
  (("C-e" . embark-act)         ;; pick some comfortable binding
   ("M-e" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :custom
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-hook 'context-menu-functions #'embark-context-menu 100)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) 
  (completion-pcm-leading-wildcard t))

(use-package org
  :hook (org-mode . visual-line-mode)
  :custom
  (org-hide-emphasis-markers t)
  (org-ellipsis "...")
  :hook (org-mode . org-indent-mode))

(dolist (face '((org-level-1 .  1.2)
                (org-level-2 .  1.1)
                (org-level-3 . 1.05)
                (org-level-4 .  1.0)
                (org-level-5 .  1.0)
                (org-level-6 .  1.0)
                (org-level-7 .  1.0)
                (org-level-8 .  1.0)))
  (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'regular :height (cdr face)))

(use-package org-modern
  :straight t
  :custom
  (org-modern-star 'replace)
  (org-modern-replace-stars "◉○✸✱✿")
  :init
  (global-org-modern-mode))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package toc-org
  :straight t
  :hook (org-mode . toc-org-mode))

(use-package org
  :custom
  (org-agenda-files '("~/Documents/org_agenda/")))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'default))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package rg
  :straight t)

(use-package ag
  :straight t)

(use-package magit
  :straight t
  :commands magit-status)

(use-package forge
  :straight t
  :init (setq forge-add-default-bindings nil)
  :after magit)

(use-package dirvish
  :straight t
  :init (dirvish-override-dired-mode)
  :custom (dirvish-attributes
           (append
            ;; The order of these attributes is insignificant, they are always
            ;; displayed in the same position.
            '(vc-state subtree-state nerd-icons collapse)
            ;; Other attributes are displayed in the order they appear in this list.
            '(git-msg file-modes file-time file-size))))
(dirvish-define-preview eza (file)
  "Use `eza' to generate directory preview."
  :require ("eza") ; tell Dirvish to check if we have the executable
  (when (file-directory-p file) ; we only interest in directories here
    `(shell . ("eza" "-al" "--color=always" "--icons=always"
               "--group-directories-first" ,file))))

(push 'eza dirvish-preview-dispatchers)

(use-package dired-single
  :straight t)

(use-package dired-open
  :straight t
  :config
  (setq dired-open-extensions '(("gif"  .  "gwenview")
				                ("jpg"  .  "gwenview")
				                ("png"  .  "gwenview")
				                ("mov"  .    "haruna")
				                ("mp4"  .    "haruna")
				                ("mkv"  .    "haruna")
				                ("mp3"  . "audacious")
                                ("flac" . "audacious")
				                ("ogg"  . "audacious")
				                ("acm"  . "audacious")
				                ("wav"  . "audacious"))))

(use-package lsp-mode
  :after markdown-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-inlay-hint-enable t)
  :hook (((python-ts-mode rust-ts-mode c-ts-mode) . lsp-deferred)
         (lsp-mode . lsp-inlay-hints-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package consult-lsp
  :straight '(consult-lsp :host github :repo "gagbo/consult-lsp")
  :after lsp-mode)

(use-package lsp-treemacs
  :straight t
  :after lsp-mode)

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :commands lsp-ui-mode)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package dap-mode
  :straight t
  :init
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (require 'dap-gdb)
  (setq dap-gdb-debug-program '("rust-gdb" "-i" "dap")))

(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode))
(use-package yasnippet
  :straight t
  :hook ((lsp-mode . yas-minor-mode)))

(use-package corfu
  :straight t
  :custom
  (corfu-min-width 60)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.05)
  (corfu-cycle t)
  :bind
  (:map corfu-map ("s-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))
(setq lsp-completion-provider :none)
(defun corfu-lsp-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))
(add-hook 'lsp-mode-hook #'corfu-lsp-setup)

(use-package nerd-icons-corfu
  :straight t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package cape
  :straight t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )

(use-package lua-mode
  :straight t)

(use-package rspec-mode
  :straight t)
(use-package inf-ruby
  :straight t)

(use-package c-ts-mode
  :config
  (setq c-ts-mode-indent-offset 8)
  (setq c-ts-mode-indent-style 'linux))

(use-package rust-mode
  :straight t)

(use-package hyprlang-ts-mode
  :straight t)
(add-to-list 'treesit-language-source-alist
	     '(hyprlang "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang"))

(use-package markdown-mode
  :straight t)

;; enables treesitter modes whenever possible
(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))
;; adds support for folding functions
(use-package treesit-fold
  :straight (treesit-fold :type git :host github
                          :repo "emacs-tree-sitter/treesit-fold")
  :init (global-treesit-fold-indicators-mode 1)
  (treesit-fold-line-comment-mode 1))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args
  
  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :straight t
  :hook (term-mode . eterm-256color-mode))

(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))
       :hook (eshell-load . eat-eshell-mode)))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  
  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'beginning-of-line)
  (evil-normalize-keymaps)
  
  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :straight t
  :after eshell)

(use-package eshell-z
  :straight t
  :after eshell)

(use-package esh-help
  :straight t
  :after eshell
  :config (setup-esh-help-eldoc))

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "nvim" "gdu")))
  
  (eshell-git-prompt-use-theme 'multiline2))

(pkoa/leader
  "w" '(:ignore t :which-key "Window")
  "wd" '(delete-window :which-key "Delete Window")
  "wv" '(evil-window-vsplit :which-key "Split Vertically")
  "ws" '(evil-window-split :which-key "Split Horizontally")
  "wh" '(evil-window-left :which-key "Switch Window Left")
  "wl" '(evil-window-right :which-key "Switch Window Right")
  "wk" '(evil-window-up :which-key "Switch Window Up")
  "wj" '(evil-window-down :which-key "Switch Window Down")
  "ww" '(evil-window-next :which-key "Next Window")
  "wr" '(redraw-display :which-key "Refresh Window/Display")
  "wi" '(delete-other-windows :which-key "Isolate Window"))

(pkoa/leader
 "b" '(:ignore t :which-key "Buffer")
 "bc" '(recenter :which-key "Center on Cursor")
 "bw" '(save-buffer :which-key "Save Current Buffer")
 "bd" '(kill-buffer :which-key "Kill Current Buffer")
 "bs" '(consult-buffer :which-key "Switch Buffer"))

(pkoa/leader
  "f" '(:ignore t :which-key "File")
  "ff" '(find-file :which-key "Find File")
  "fF" '(consult-fd :wich-key "Fd")
  "fP" '((lambda () (interactive)
	 (find-file "~/.emacs.d/config.org"))
	 :which-key "Emacs config.org")
  "fg" '(consult-ripgrep :which-key "RipGrep")
  "fG" '(ag :which-key "Silver-Searcher")
  "fe" '(eval-last-sexp :which-key "evaluate")
  "fs" '(consult-line :which-key "Search File")
  "fw" '(write-file :which-key "Write File to...")
  "fr" '(consult-recent-file :which-key "Recent Files")
  "fu" '(sudo-edit-find-file :j which-key "Sudo Find File")
  "fU" '(sudo-edit :which-key "Sudo Edit File"))

(pkoa/leader
  "m" '(:ignore t :which-key "Magit")
  "mm" '(magit-status :which-key "Magit Status")
  "md" '(magit-dispatch :which-key "Dispatch")
  "mf" '(with-editor-finish :which-key "Confirm")
  "ms" '(magit-stage-modified :which-key "Stage")
  "mS" '(magit-unstage-all :which-key "Unstage")
  "mc" '(magit-commit :which-key "Commit")
  "mp" '(magit-push :which-key "Push")
  "mP" '(magit-pull :which-key "Pull"))

(pkoa/leader
  "o" '(:ignore t :which-key "Org")
  "oa" '(org-agenda :which-key "Agenda")
  "ot" '(org-todo :which-key "Todo")
  "ol" '(org-insert-link :which-key "Insert Link")
  "oo" '(org-open-at-point :which-key "Open")
  "oi" '(org-set-tags-command :which-key "Set Tags"))

(pkoa/leader
  "SPC" '(execute-extended-command :which-key "M-x"))

(setq gc-cons-threshold (* 2 1000 1000))
