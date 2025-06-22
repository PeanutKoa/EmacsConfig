;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

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
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (setq dashboard-banner-logo-title "PeanutKoa's Emacs, Powered by Evil!")
  (setq dashboard-startup-banner "~/.emacs.d/evil.png") 
  (setq dashboard-items '((recents   . 5)
  			  (bookmarks . 5)
  			  (projects  . 5)
  			  (registers . 5)))
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

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :straight t)
;; please bind to "<leader>"+";"

(straight-use-package '(blimpy :host github :repo "progfolio/blimpy"))
  (use-package blimpy
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

(general-define-key
 "C-x M-x" 'redraw-display
 "<escape>" 'keyboard-escape-quit)

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(which-key-mode 1)

(use-package helpful
  :straight t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package jinx
  :straight t
  :bind (("M-$" . jinx-correct)
       ("C-M-$" . jinx-languages)))

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

(use-package ivy
  :straight t
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :straight t
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package flycheck
  :straight t
  :after lsp-mode
  :defer t
  :diminish
  :hook (lsp-mode . flycheck-mode))

(use-package nerd-icons-ivy-rich
  :straight t
  :init
  (nerd-icons-ivy-rich-mode 1))

  (use-package ivy-rich
    :straight t
    :init
    (ivy-rich-mode 1))

(use-package ivy-prescient
  :straight t
  :config
  (ivy-prescient-mode 1))

(defun pkoa/hyphen-dot ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

  (use-package org
    :hook (org-mode . visual-line-mode)
    (org-mode . org-indent-mode)
    :config
    (setq org-ellipsis " ▾")
    (pkoa/hyphen-dot))

(use-package visual-fill-column
  :straight t
  :config
  (setq visual-fill-column-width 170
	visual-fill-column-center-text t)
  :hook (org-mode . visual-fill-column-mode))

(use-package org-bullets
  :straight t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package toc-org
  :straight t
  :hook (org-mode . toc-org-mode))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :straight t
  :after projectile
  :config (counsel-projectile-mode))

(use-package rg
  :straight t)

(use-package ag
  :straight t)

(use-package magit
  :straight t
  :commands magit-status)

(use-package forge
  :after magit
  :straight t)

(use-package nerd-icons-dired
  :straight t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-single
  :straight t)

(use-package dired-open
  :straight t
  :config
  (setq dired-open-extensions '(("gif" .  "gwenview")
				("jpg" .  "gwenview")
				("png" .  "gwenview")
				("mov" .    "haruna")
				("mp4" .    "haruna")
				("mkv" .    "haruna")
				("mp3" . "audacious")
				("ogg" . "audacious")
				("acm" . "audacious")
				("wav" . "audacious"))))

(defun pkoa/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . pkoa/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :straight t 
  :after lsp)

(use-package lsp-ivy
  :straight t
  :after lsp)

(use-package company
  :straight t
  :after lsp-mode
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (global-company-mode t))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :straight t
  :hook (company-mode . company-prescient-mode))

(use-package lua-mode
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
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
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
  
  (eshell-git-prompt-use-theme 'powerline))

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
 "bs" '(switch-to-buffer :which-key "Switch Buffer"))

(pkoa/leader
  "f" '(:ignore t :which-key "File")
  "ff" '(find-file :which-key "Find File")
  "fP" '((lambda () (interactive)
	 (find-file "~/.emacs.d/config.org"))
	 :which-key "Emacs config.org")
  "fg" '(rg :which-key "RipGrep")
  "fG" '(ag :which-key "Silver-Searcher")
  "fe" '(eval-last-sexp :which-key "evaluate")
  "fs" '(swiper :which-key "Search File")
  "fw" '(write-file :which-key "Write File to...")
  "fr" '(counsel-recentf :which-key "Recent Files")
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

(setq gc-cons-threshold (* 2 1000 1000))
