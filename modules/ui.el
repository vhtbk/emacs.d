;; modules/ui.el -*- lexical-binding: t; -*-

;; Ensure core module is available
(require 'core)

(use-package emacs
  :ensure nil
  :custom
  (line-spacing 0.1)                                      ; Better readability
  (show-trailing-whitespace t)                            ; Show trailing whitespace
  (indicate-empty-lines t)                                ; Show where file ends
  :config
  (global-hl-line-mode 1)                                 ; Highlight current line
  (show-paren-mode 1)                                     ; Highlight matching brackets
  (global-display-line-numbers-mode 1)
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))      ; Disable icon tool bar
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))) ; Disable side scroll bar

(when (display-graphic-p)
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Apply font settings
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'vhtbk/setup-fonts)
  (vhtbk/setup-fonts))

;; Icons and Themes
(use-package nerd-icons)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one-light t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Disable line numbers in specific modes where they don't belong
(dolist (hook '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                dired-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

;; Frame focus
(add-hook 'window-setup-hook #'vhtbk/raise-frame-and-focus)
(add-hook 'server-after-make-frame-hook #'vhtbk/raise-frame-and-focus)

(provide 'ui)
