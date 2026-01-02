;; modules/vhtbk-roam.el -*- lexical-binding: t; -*-

(use-package org-roam
  :init
  (let ((roam-path (expand-file-name "~/Documents/org/roam")))
    (unless (file-exists-p roam-path)
      (make-directory roam-path t))
    (setq org-roam-directory roam-path))
  :bind
  (("C-c n l" . org-roam-buffer-toggle) ; Show backlinks for current note
   ("C-c n f" . org-roam-node-find)     ; Find or create a note
   ("C-c n i" . org-roam-node-insert)   ; Insert a link to another note
   ("C-c n g" . org-roam-graph)         ; Visualize your note network
   ("C-c n c" . org-roam-capture)       ; Quick capture to roam
   ("C-c n d n" . org-roam-dailies-capture-today))
  :config
  ;; Initialize the database and sync on startup
  (org-roam-db-autosync-mode)

  ;; Customizing the display template for node-finding
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))))

(use-package org-roam-ui
  :after org-roam
  ;; Grouping these under the 'n' (note) prefix
  :bind ("C-c n u" . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(provide 'vhtbk-roam)
