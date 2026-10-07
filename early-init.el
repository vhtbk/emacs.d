;; early-init.el -*- lexical-binding: t; -*-

;; Prevent UI flicker
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Increase garbage collection limit for startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; libgccjit derives the macOS version from the Darwin kernel version
;; Pass the real OS version to the gcc driver
(when (and (eq system-type 'darwin)
           (not (getenv "MACOSX_DEPLOYMENT_TARGET")))
  (let ((ver (string-trim
              (shell-command-to-string "/usr/bin/sw_vers -productVersion"))))
    (when (string-match-p "\\`[0-9]+\\(\\.[0-9]+\\)*\\'" ver)
      (setenv "MACOSX_DEPLOYMENT_TARGET" ver))))

;; Force load dired
(require 'dired)
