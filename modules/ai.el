;; modules/ai.el -*- lexical-binding: t; -*-

(use-package gptel
  :config
  ;; Ollama backend definition
  (setq vhtbk/ollama-backend
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"
          :stream t
          :models '("llama3:latest" "deepseek-coder:latest")))

  ;; AI prompts are in custom.el
  (when (boundp 'vhtbk/ai-prompts)
    (setq gptel-directives (append vhtbk/ai-prompts gptel-directives)))

  ;; Org-mode integration
  (setq gptel-default-mode 'org-mode)

  ;; Defaults
  (setq gptel-backend vhtbk/ollama-backend)
  (setq gptel-model "llama3:latest")
  (setq gptel-directive (alist-get 'coder gptel-directives)))

(provide 'ai)
