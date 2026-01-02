;; modules/completion.el -*- lexical-binding: t; -*-

;; Vertico: Vertical completion UI in the minibuffer
(use-package vertico
  :init (vertico-mode))

;; Orderless: Allows searching by typing parts of a string in any order
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia: Adds helpful descriptions (like file sizes or docstrings) next to candidates
(use-package marginalia
  :init (marginalia-mode))

;; Corfu: In-buffer completion popups (IDE-style)
(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2))

(provide 'completion)
