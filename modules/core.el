;; modules/core.el -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :custom
  ;; UI
  (ring-bell-function #'ignore)                      ; Silence the bell
  (cursor-in-non-selected-windows nil)               ; Hide cursor in inactive windows
  (window-combination-resize t)                      ; Resize windows proportionally
  (ad-redefinition-action 'accept)                   ; Silence redefinition warnings
  (native-comp-async-report-warnings-errors 'silent) ; Hide comp warnings
  ;; Editor behavior
  (fill-column 80)                                   ; Standard wrap column
  (sentence-end-double-space nil)                    ; Single space after sentences
  (use-short-answers t)                              ; y/n instead of yes/no

  :config
  ;; OS constants
  (defconst vhtbk/is-macos (eq system-type 'darwin))
  (defconst vhtbk/is-linux (eq system-type 'gnu/linux))
  ;; UTF-8 is everywhere
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8))

;; Move backup files to the backup directory
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      vc-make-backup-files t    ; Create backups even for files in Git/Version Control
      version-control t         ; Use version numbers for backups
      kept-old-versions 2       ; Keep the oldest version
      kept-new-versions 5       ; Keep the 5 newest versions
      delete-old-versions t)    ; Automatically delete excess backup versions

;; Move auto-save files to the same backup directory
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "backups/" user-emacs-directory) t)))

;; macOS config
(when vhtbk/is-macos
  (setq mac-command-modifier      'super
        mac-option-modifier       'meta
        mac-control-modifier      'control
        mac-right-option-modifier 'left
        ns-use-native-fullscreen t
        ns-pop-up-frames nil
        mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        mouse-wheel-follow-mouse 't
        scroll-step 1
        select-enable-clipboard t))

(provide 'core)
