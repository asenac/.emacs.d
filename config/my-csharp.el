(use-package omnisharp
  :ensure nil
  :mode ("\\.cs\\'" . csharp-mode)
  :config
  (progn
    ;; Note: Flycheck support needs manual fix
    ;; https://github.com/OmniSharp/omnisharp-emacs/issues/262
    (defun my-csharp ()
      (omnisharp-mode)
      (add-to-list 'company-backends 'company-omnisharp))

    (after 'evil-leader
      (evil-leader/set-key-for-mode 'csharp-mode
        "cf" 'omnisharp-code-format
        "rj" 'omnisharp-go-to-definition
        "rv" 'omnisharp-find-implementations
        "rr" 'omnisharp-helm-find-usages
        "rF" 'omnisharp-fix-code-issue-at-point))

    (add-hook 'csharp-mode-hook 'my-csharp)))

(provide 'my-csharp)
