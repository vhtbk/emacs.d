;; modules/keybinds.el -*- lexical-binding: t; -*-

;; Set a Leader Key (C-c l)
(defconst vhtbk/leader "C-c l")

;; Quick navigation
(global-set-key (kbd (concat vhtbk/leader " f")) #'find-file)
(global-set-key (kbd (concat vhtbk/leader " b")) #'consult-buffer)
(global-set-key (kbd (concat vhtbk/leader " s")) #'consult-ripgrep)

;; Org-mode entry points
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)

;; Window management
(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-scope 'frame
        aw-dispatch-always t
        aw-ignore-on t))

(provide 'keybinds)
