(use-package eclim
  :config
  (progn
    (global-eclim-mode)

    (after 'evil-leader
      (evil-leader/set-key-for-mode 'java-mode
        "rr" 'eclim-java-find-references
        "rj" 'eclim-java-find-declaration))
    ))

(provide 'my-eclim)
