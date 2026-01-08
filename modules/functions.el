;; modules/functions.el -*- lexical-binding: t; -*-

(defun vhtbk/ai-generate-commit-message ()
  "Draft a Git commit message based on the current staged changes."
  (interactive)
  (let ((diff (shell-command-to-string "git diff --cached")))
    (gptel-request
     (format "Based on this git diff, write a concise commit message following Conventional Commits standards:\n\n%s" diff)
     :callback (lambda (response info)
                 (if response
                     (with-current-buffer (get-buffer-create "*AI-Commit*")
                       (erase-buffer)
                       (insert response)
                       (display-buffer (current-buffer)))
                   (message "AI failed to generate message."))))))


(defun vhtbk/gptel-add-project-files ()
  "Add all files in the current Projectile project to the gptel context."
  (interactive)
  (if (projectile-project-p)
      (let ((files (projectile-current-project-files)))
        (dolist (file files)
          (let ((full-path (expand-file-name file (projectile-project-root))))
            (when (file-readable-p full-path)
              (gptel-add full-path))))
        (message "Added %d files from project %s to AI context."
                 (length files) (projectile-project-name)))
    (error "Not in a Projectile project")))


(defun vhtbk/gptel-add-roam-notes ()
  "Add all Org-roam notes to the gptel context for analysis."
  (interactive)
  (let ((notes (directory-files org-roam-directory t "\\.org$")))
    (dolist (note notes)
      (gptel-add note))
    (message "Added %d roam notes to AI context." (length notes))))


(defun vhtbk/install-grammars ()
  "Install all Tree-sitter grammars."
  (interactive)
  (mapc (lambda (lang) (treesit-install-language-grammar (car lang)))
        treesit-language-source-alist))


(defun vhtbk/kill-this-buffer ()
  "Kill the current buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))


(defun vhtbk/markdown-to-org ()
  "Convert current Markdown buffer to Org using Pandoc."
  (interactive)
  (when (and buffer-file-name (executable-find "pandoc"))
    (let ((output (concat (file-name-sans-extension buffer-file-name) ".org")))
      (shell-command
       (format "pandoc -f markdown -t org %s -o %s"
               (shell-quote-argument buffer-file-name)
               (shell-quote-argument output)))
      (message "Converted to %s" output))))


(defun vhtbk/org-format-buffer ()
  "Format on save."
  (when (eq major-mode 'org-mode)
    ;; Ignore source blocks
    (setq-local org-fill-paragraph-ignore-working-at-source-block t)
    (save-excursion
      (goto-char (point-min))
      (let ((fill-column 80))
        (while (< (point) (point-max))
          (cond
           ;; Skip tables and source blocks
           ((or (org-at-table-p)
                (org-in-src-block-p)
                (org-at-block-p))
            (forward-line))

           ;; Only fill if it's a normal paragraph
           (t
            (org-fill-paragraph nil t)
            (forward-paragraph))))))))

(defun vhtbk/open-init-file ()
  "Open init.el file quickly."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))


(defun vhtbk/org-insert-weekly-clock-report ()
  "Insert or refresh weekly clock table at top of agenda file."
  (interactive)
  (with-current-buffer (find-file-noselect vhtbk/org-default-agenda-file)
    (goto-char (point-min))
    (when (re-search-forward "^\\* Weekly Clock Report" nil t)
      (let ((beg (match-beginning 0)))
        (if (re-search-forward "^\\*" nil t)
            (delete-region beg (match-beginning 0))
          (delete-region beg (point-max)))))
    (goto-char (point-min))
    (insert "* Weekly Clock Report\n")
    (insert "#+BEGIN: clocktable :scope file :block week :maxlevel 3\n")
    (insert "#+END:\n\n")
    (save-buffer)))


(defun vhtbk/org-insert-weekly-review ()
  "Insert weekly review section at end of agenda file."
  (interactive)
  (with-current-buffer (find-file-noselect vhtbk/org-default-agenda-file)
    (goto-char (point-max))
    (insert "\n* Weekly Review\n")
    (insert "** Completed Tasks\n")
    (insert "** Stuck Tasks\n")
    (insert "** Notes\n")
    (save-buffer)))


(defun vhtbk/open-org-agenda-directory ()
  "Open the agenda in Dired."
  (interactive)
  (let ((agenda-path (expand-file-name "agenda" org-directory)))
    (if (file-directory-p agenda-path)
        (dired agenda-path)
      (user-error "Agenda directory does not exist: %s" agenda-path))))


(defun vhtbk/open-org-notes-directory ()
  "Open the notes in Dired."
  (interactive)
  (let ((notes-path (expand-file-name "notes" org-directory)))
    (if (file-directory-p notes-path)
        (dired notes-path)
      (user-error "Notes directory does not exist: %s" notes-path))))


(defun vhtbk/org-to-markdown ()
  "Convert current Org buffer to Markdown using Pandoc."
  (interactive)
  (when (and buffer-file-name (executable-find "pandoc"))
    (let ((output (concat (file-name-sans-extension buffer-file-name) ".md")))
      (shell-command
       (format "pandoc -f org -t markdown %s -o %s"
               (shell-quote-argument buffer-file-name)
               (shell-quote-argument output)))
      (message "Converted to %s" output))))


(defun vhtbk/raise-frame-and-focus ()
  "Raise the current frame and grab focus."
  (select-frame-set-input-focus (selected-frame))
  (raise-frame))


(defun vhtbk/set-c-style-kernel ()
  "Set C indentation to 8-character tabs (Linux Kernel standard)."
  (interactive)
  (setq-local indent-tabs-mode t
              c-basic-offset 8
              tab-width 8)
  (message "C-Style: Linux Kernel (8 Tabs)"))


(defun vhtbk/setup-fonts ()
  "Font configuration."
  (let* ((mac-fonts   '("JetBrains Mono" "SF Mono" "Menlo"))
         (linux-fonts '("JetBrains Mono" "DejaVu Sans Mono" "monospace"))
         ;; Pick the first font that actually exists on this OS
         (family (cl-find-if (lambda (f) (member f (font-family-list)))
                             (if vhtbk/is-macos mac-fonts linux-fonts)))
         (size (if vhtbk/is-macos 140 120)))

    (set-face-attribute 'default nil :family family :height size)
    (set-face-attribute 'fixed-pitch nil :family family :height 1.0)
    (set-face-attribute 'variable-pitch nil :family "sans-serif" :height 1.1)))


(defun vhtbk/setup-go-mode ()
  (setq tab-width 8
        indent-tabs-mode t))


(defun vhtbk/slurm-status ()
  "Show the current user's Slurm jobs."
  (interactive)
  (let ((buffer "*Slurm-Status*"))
    (with-current-buffer (get-buffer-create buffer)
      (read-only-mode -1)
      (erase-buffer)
      (insert (shell-command-to-string "squeue -u $USER"))
      (read-only-mode 1)
      (display-buffer buffer))))


(defun vhtbk/split-below-and-move ()
  "Split window below and jump to it."
  (interactive)
  (split-window-below)
  (other-window 1))


(provide 'functions)
