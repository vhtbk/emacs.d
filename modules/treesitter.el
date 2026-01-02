;; treesitter.el -*- lexical-binding: t; -*-

;; Tree-sitter configuration
(setq treesit-extra-load-path (list (expand-file-name "tree-sitter" user-emacs-directory)))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.20.5")
        (c "https://github.com/tree-sitter/tree-sitter-c" "v0.20.6")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.5")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "v0.1.0")
        (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
        (html "https://github.com/tree-sitter/tree-sitter-html" "v0.20.1")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1")
        (json "https://github.com/tree-sitter/tree-sitter-json" "v0.20.2")
        (make "https://github.com/alemuller/tree-sitter-make" "main")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1")
        (markdown-inline "https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1")
        (python "https://github.com/tree-sitter/tree-sitter-python" "v0.20.4")
        (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.20.4")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0")))

(provide 'treesitter)
