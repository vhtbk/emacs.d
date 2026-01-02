;; modules/dev.el -*- lexical-binding: t; -*-

;; Treesit highlighting
(use-package treesit
  :ensure nil
  :config
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (c-mode . c-ts-mode)
          (go-mode . go-ts-mode)
          (python-mode . python-ts-mode)
          (rust-mode . rust-ts-mode))))

;; LSP support
(use-package eglot
  :ensure nil
  :hook ((bash-ts-mode
          c-ts-mode
          go-ts-mode
          python-ts-mode
          rust-ts-mode) . eglot-ensure))

;; Visual helpers
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; SPACE and TAB config
(use-package emacs
  :ensure nil
  :custom
  (indent-tabs-mode nil)       ; Use spaces, not tabs
  (tab-width 4)                ; 4-space tabs
  (c-basic-offset 4)           ; C: 4 spaces
  (python-indent-offset 2)     ; Python: 2 spaces
  (sh-basic-offset 2)          ; Bash: 2 spaces
  (sh-indentation 2)
  (rust-indent-offset 4))      ; Rust: 4 spaces

;; Go: TAB
(add-hook 'go-mode-hook #'vhtbk/setup-go-mode)

;; Puppet: 2 spaces
(setq-default puppet-indent-level 2)
(add-hook 'puppet-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode nil
                        tab-width 2)))

;; YAML: 2 spaces
(add-hook 'yaml-mode-hook
          (lambda ()
            (setq-local yaml-indent-offset 2)))

;; Highlight
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-char ?│)
  (highlight-indent-guides-delay 0.1))

;; Clean whitespace on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(provide 'dev)
