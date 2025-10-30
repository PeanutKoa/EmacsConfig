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
(straight-use-package 'use-package)

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
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :straight t
  :hook
  (prog-mode . rainbow-mode)
  (org-mode . rainbow-mode))

(use-package fic-mode-xtra
  :straight '(fic-mode-xtra :host github :repo "PeanutKoa/fic-mode-xtra")
  :hook (prog-mode . fic-mode))

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 120)

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
		        eww-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

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
                          (agenda    . 5)
  	                    (projects  . 5)))
  :config
  (dashboard-setup-startup-hook))

(add-to-list 'default-frame-alist '(alpha-background . 95))

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
  (("C-SPC" . embark-act)         ;; pick some comfortable binding
   ("C-M-SPC" . embark-dwim)        ;; good alternative: M-.
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

(defun pkoa/hyphen-dot ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org
  :hook (org-mode . visual-line-mode)
  :config
  (pkoa/hyphen-dot))
(setq org-hide-emphasis-markers t
	org-ellipsis "…")

(use-package org-modern
  :straight t
  :init
  (global-org-modern-mode))
(setq org-modern-fold-stars '(("◉" . "◉") ("○" . "○") ("✸" . "✸") ("✱" . "✱") ("✿" . "✿") ("◉" . "◉") ("○" . "○") ("✸" . "✸") ("✱" . "✱") ("✿" . "✿")))

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

(use-package nerd-icons-dired
  :straight t
  :hook (dired-mode . nerd-icons-dired-mode))

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
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-ts-mode . lsp-deferred)
	     (rust-mode . lsp-deferred)
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

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))

(use-package lua-mode
  :straight t)

(use-package rspec-mode
  :straight t)
(use-package inf-ruby
  :straight t)

(use-package rust-mode
  :straight t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package hyprlang-ts-mode
  :straight t)
(add-to-list 'treesit-language-source-alist
	     '(hyprlang "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang"))

(use-package markdown-mode
  :straight t)

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
