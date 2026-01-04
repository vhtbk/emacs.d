;;; org-config.el --- Org mode configuration -*- lexical-binding: t; -*-

(require 'org)
(require 'org-capture)

(use-package org
  :ensure nil ; Built-in
  :custom
  ;; Directories & files
  (org-directory "~/Documents/org/")
  (org-agenda-files '("~/Documents/org/agenda/todo.org"))

  ;; Basic behavior
  (org-ellipsis " ↴")
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-use-sub-superscripts '{})
  (org-log-into-drawer t)

  ;; TODO workflow
  (org-todo-keywords
   '((sequence "TODO" "⏳IN-PROGRESS" "|" "🔎REVIEW" "✅DONE")))

  ;; Agenda settings
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday 1)
  (org-agenda-window-setup 'current-window)
  (org-agenda-show-log t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)

  ;; Refile
  (org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 3)))

  :config
  (define-key org-mode-map (kbd "C-c C-e") #'org-export-dispatch)
  ;; Clocking / time tracking
  (setq org-clock-persist 'history
        org-clock-idle-time 10
        org-clock-in-resume t
        org-clock-out-remove-zero-time-clocks t
        org-clock-history-length 20
        org-log-into-drawer t)
  (org-clock-persistence-insinuate)

  ;; Babel configuration
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))

  ;; Custom Agenda Commands
  (setq org-agenda-custom-commands
        '(("d" "Daily agenda"
           ((agenda "")
            (todo "⏳IN-PROGRESS")))
          ("w" "Weekly review"
           ((agenda "" ((org-agenda-span 'week)))
            (todo "✅DONE")
            (stuck "")))))

  ;; Capture Templates
  (setq org-capture-templates
        `(("i" "Inbox" entry (file+headline ,(expand-file-name "agenda/todo.org" org-directory) "Inbox")
           "* %?\n  Captured on %U\n  %i")
          ("t" "Todo" entry (file+headline ,(expand-file-name "agenda/todo.org" org-directory) "Tasks")
           "* TODO %?\n  SCHEDULED: %t\n  Captured on %U\n  %i")
          ("w" "Web Bookmark" entry (file+headline ,(expand-file-name "agenda/todo.org" org-directory) "Bookmarks")
           "* %? :bookmark:\n  %^L\n  Added: %U\n  Description: %i"))))

(setq initial-major-mode 'org-mode)
(setq calendar-week-start-day 1)

;; Hide line numbers in Org buffers only
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Format Org file on save
(add-hook 'before-save-hook #'vhtbk/org-format-buffer)

;; TOC
(use-package toc-org
  :after org
  :hook (org-mode . toc-org-mode))

;; Pandoc export
(use-package ox-pandoc
  :after org)

(provide 'org-config)
