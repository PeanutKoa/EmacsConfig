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

;; Setting all of the standard shit, such as fonts, theme, and removing scroll bar
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 120)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode t)
(use-package catppuccin-theme :straight t)
(load-theme 'catppuccin :no-confirm)
