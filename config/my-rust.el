(use-package toml-mode)

(use-package rust-mode
  :hook (rust-mode . lsp)
  :config (setq rust-format-on-save t))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;; (use-package rustic
;;   :ensure t
;;   :config (setq rustic-lsp-format t))

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(after 'lsp-mode
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-rust-analyzer-server-command '("~/.emacs.d/bin/rust-analyzer-wrapper.sh"))
  (setq lsp-rust-analyzer-proc-macro-enable t)  ; get access to proc macros, will eventually be on by default
  (setq lsp-rust-analyzer-import-merge-behaviour "last") ; materialize import style
  (setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t) ; ensure coordinated compilation dirs
  )

(after 'evil-leader
  (evil-leader/set-key-for-mode 'rust-mode
    "lj" 'lsp-find-definition
    "lr" 'lsp-ui-peek-find-references
    "lm" 'lsp-ui-doc-show
    "i" 'helm-lsp-code-actions)
  (evil-leader/set-key
    "ls" 'helm-lsp-workspace-symbol)
  )

;; disable all rust related checkers but lsp
(after 'flycheck
  (add-to-list 'flycheck-disabled-checkers 'cargo)
  (add-to-list 'flycheck-disabled-checkers 'rust)
  (add-to-list 'flycheck-disabled-checkers 'rust-clippy))

(provide 'my-rust)
