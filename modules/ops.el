;; modules/ops.el -*- lexical-binding: t; -*-

;; PATH
(use-package exec-path-from-shell
  :if (or vhtbk/is-macos (display-graphic-p))
  :config
  ;; Add more if shell exports them
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOROOT" "PYTHONPATH"))
  (exec-path-from-shell-initialize))

;; Tree-sitter support
(use-package treesit
  :ensure nil
  :config
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
  (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode)))

;; LSP support
(use-package eglot
  :ensure nil
  :hook ((ansible-mode
          dockerfile-ts-mode
          markdown-mode
          puppet-mode
          python-base-mode
          sql-mode
          yaml-ts-mode) . eglot-ensure)
  :config
  (setq eldoc-echo-area-use-multiline-p 10)) ; Limits the popup height to 10 lines

;; Docker
(use-package docker
  :after dired
  :bind ("C-c d" . docker))

(use-package dockerfile-mode)

;; Kubernetes
(use-package kubernetes
  :after dired
  :commands (kubernetes-overview))

;; IaC, Configuration Management
(use-package yaml-mode)

(use-package ansible
  :hook (yaml-ts-mode . ansible-hook))

(use-package puppet-mode)

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

;; Git interface
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-fullscreen-v1))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  ;; Update gutter signs on save
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Shell file and formatting
(setq sh-shell-file "/bin/bash")

(use-package shfmt
  :hook (sh-mode . shfmt-on-save-mode)
  :config
  (setq shfmt-arguments '("-i" "2" "-ci")))

;; TRAMP
(use-package tramp
  :ensure nil  ;; Tramp is builtin, no need to download
  :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq remote-file-name-inhibit-cache nil
        tramp-default-method "ssh"
        tramp-use-ssh-controlmaster-options nil
        tramp-verbose 1))

;; Slurm
(use-package slurm-mode
  :mode ("\\.slurm\\'" "\\.batch\\'" "\\.sbatch\\'")
  :config
  (setq slurm-highlight-rate 0.5))

(use-package vlf
  :config
  (require 'vlf-setup))

;; SQL
(use-package sql
  :ensure nil
  :config
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines 1)
              (setq-local show-trailing-whitespace nil))))

(use-package sql-indent
  :hook (sql-mode . sqlindent-minor-mode))

(use-package sqlup-mode
  :hook (sql-mode . sqlup-mode))

;; Markdown
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode) ; use GitHub Markdown
  :init (setq markdown-command "pandoc")
  :config (add-hook 'markdown-mode-hook 'visual-line-mode))

;; Template system for Emacs
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets" user-emacs-directory)           ; Tracked
              (expand-file-name "snippets/private" user-emacs-directory))) ; Untracked
  (yas-reload-all))

;; HTTP client for Emacs
(use-package verb
  :after org
  :config
  (define-key org-mode-map (kbd "C-c C-r") 'verb-command-map))

(provide 'ops)
