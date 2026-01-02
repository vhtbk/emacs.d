;; modules/vhtbk-project.el -*- lexical-binding: t; -*-

(use-package projectile
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-project-search-path '("~/Documents/org" "~/scratch"))
  (projectile-completion-system 'vertico)
  :config
  (projectile-discover-projects-in-search-path))

(use-package consult-projectile
  :after (projectile consult))

(provide 'vhtbk-project)
