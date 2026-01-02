;; modules/vhtbk-dired.el -*- lexical-binding: t; -*-

(require 'dired)

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t))

;; Add colorful icons to Dired
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Expand folders inside Dired with TAB
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map ("<tab>" . dired-subtree-toggle)))

(provide 'vhtbk-dired)
