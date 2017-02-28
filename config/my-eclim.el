(use-package eclim
  :config
  (progn
    (add-hook 'java-mode-hook 'eclim-mode)

    (after 'evil-leader
      (evil-leader/set-key-for-mode 'java-mode
        "rr" 'eclim-java-find-references
        "rj" 'eclim-java-find-declaration))
    ))

(provide 'my-eclim)
