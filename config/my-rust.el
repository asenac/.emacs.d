
;; (use-package rust-mode
;;   :ensure t
;;   :config (setq rust-format-on-save t))

(use-package toml-mode)

(use-package rust-mode
  :hook (rust-mode . lsp)
  :config (setq rust-format-on-save t))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package rustic
;;   :ensure t
;;   :config (setq rustic-lsp-format t))

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(after 'lsp-mode
  (setq lsp-rust-server 'rust-analyzer))

(provide 'my-rust)
