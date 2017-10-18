(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(after 'evil-leader
  (evil-leader/set-key-for-mode 'go-mode
    "rj" 'godef-jump))

(provide 'my-go)
