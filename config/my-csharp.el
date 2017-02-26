(use-package omnisharp
  :defer t
  :ensure nil
  :mode ("\\.cs\\'" . csharp-mode)
  :diminish 'omnisharp-mode
  :init
  (progn
    ;; Note: Flycheck support needs manual fix
    ;; https://github.com/OmniSharp/omnisharp-emacs/issues/262
    (defun my-csharp ()
      (omnisharp-mode)
      (add-to-list 'company-backends 'company-omnisharp))
    (add-hook 'csharp-mode-hook 'my-csharp t))
  :config
  (progn
    (evil-leader/set-key-for-mode 'csharp-mode
      "cf" 'omnisharp-code-format
      "rj" 'omnisharp-go-to-definition
      "rv" 'omnisharp-find-implementations
      "rr" 'omnisharp-helm-find-usages
      "rF" 'omnisharp-fix-code-issue-at-point)))


(provide 'my-csharp)
