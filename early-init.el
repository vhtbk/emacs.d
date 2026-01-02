;; early-init.el -*- lexical-binding: t; -*-

;; Prevent UI flicker
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Increase garbage collection limit for startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Force load dired
(require 'dired)
