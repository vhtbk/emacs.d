;; search.el -*- lexical-binding: t; -*-

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-c a" . consult-ripgrep)
         ("C-x b" . consult-buffer)))

(use-package rg)

(provide 'search)

