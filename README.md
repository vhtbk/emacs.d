# vhtbk Emacs Configuration

Welcome! This is my Emacs configuration.
It is designed for my current workflows. I'm a Debian user, but it works seamlessly on latest Ubuntu, Fedora and macOS.

## Key Workflows

* *Dev:* Syntax support for Bash, C, Go, Python and Rust.
* *Ops:* Syntax support for Ansible, Docker, Puppet, and YAML via Tree-sitter.
* *Documentation:* Org-mode setup for documentation and task tracking.
* *HPC Management:* Slurm job script support and TRAMP for remote cluster management.
* *AI Integration:* Support for Ollama via `gptel` for code explanation and troubleshooting.

## Installation

Ensure your operating system has the required binaries. Or, you can run `install_emacsh.sh` script:

* *Common:* `git`, `pandoc`, `ripgrep`
* *Debian/Ubuntu:* `emacs`, `build-essential`, `cmake`, `libtool-bin`, `libgccjit-12-dev`, `fd-find`, `curl`, `texlive-base`, `texlive-latex-extra` `pgformatter`
* *Fedora:* `emacs`, `libgccjit-devel`, `fd-find`, `texlive-scheme-basic`, `perl-pgFormatter`
* *macOS:* `coreutils`, `d12frosted/emacs-plus`, `fd`, `libgccjit`, `pgformatter`

Clone the repository into your home directory:

```bash
mv ~/.emacs.d ~/.emacs.d.bak
git clone https://github.com/vhtbk/emacs.d.git ~/.emacs.d
cd ~/.emacs.d/
./install_emacs.sh
```

Install LSP languages and nerd fonts:

```text
M-x vhtbk/install-grammars
M-x nerd-icons-install-fonts
```

## Customization

If you want to disable modules or add your own configuration, create a file called `custom.el` in the `~/.emacs.d/` directory. This file is ignored by Git.

```
;; custom.el -*- lexical-binding: t; -*-

;; Disable a specific block by setting a flag

;; Disable ui
;;(setq vhtbk/disable-ui t)

;; Disable completion
;;(setq vhtbk/disable-completion t)

;; Disable search
;;(setq vhtbk/disable-search t)

;; Disable project
;;(setq vhtbk/disable-project t)

;; Disable keybinds
;;(setq vhtbk/disable-keybinds t)

;; Disable dev
;;(setq vhtbk/disable-dev t)

;; Disable ops
;;(setq vhtbk/disable-ops t)

;; Disable org
;;(setq vhtbk/disable-org t)

;; Disable org-roam
;;(setq vhtbk/disable-roam t)

;; Disable ai
;;(setq vhtbk/disable-ai t)

(provide 'custom)

```

And, for templates you can use the private snippet directory. `private` directory is also ignored by Git.
- Public Snippets (Tracked):  `~/.emacs.d/snippets/`
- Private Snippets (Ignored): `~/.emacs.d/snippets/private/`

