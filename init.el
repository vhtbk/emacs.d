;; init.el -*- lexical-binding: t; -*-

;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 100 1024 1024))))  ;; 100MB

;; Show load time in minibuffer
(add-hook 'emacs-startup-hook
          (lambda () (message "Emacs ready in %s with %d GCs." (emacs-init-time) gcs-done)))

;; Add all necessary directories to load-path
(dolist (dir '("modules" "lisp"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

;; Load Package Manager
(require 'package-manager)

;; Load custom configs
(let ((custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; Load Core
(require 'core)

;; Load Tree-sitter
(require 'treesitter)

;; Load my custom functions
(require 'functions)

;; Load Dired, use custom one
(require 'vhtbk-dired)

;; Load UI
(unless (bound-and-true-p vhtbk/disable-ui)
  (require 'ui))

;; Load vertico, orderless, marginalia, corfu
(unless (bound-and-true-p vhtbk/disable-completion)
  (require 'completion))

;; Load consult
(unless (bound-and-true-p vhtbk/disable-search)
  (require 'search))

;; Load projectile
(unless (bound-and-true-p vhtbk/disable-project)
  (require 'vhtbk-project))

;; Load keybinds
(unless (bound-and-true-p vhtbk/disable-keybinds)
  (require 'keybinds))

;; Load DevOps
(unless (bound-and-true-p vhtbk/disable-dev)
  (require 'dev))
(unless (bound-and-true-p vhtbk/disable-ops)
  (require 'ops))

;; Load Org
(unless (bound-and-true-p vhtbk/disable-org)
  (require 'org-config))
(unless (bound-and-true-p vhtbk/disable-roam)
  (require 'vhtbk-roam))

;; Load AI
(unless (bound-and-true-p vhtbk/disable-ai)
  (require 'ai))

;;
;; Done.
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
