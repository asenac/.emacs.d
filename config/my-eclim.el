;; Note: For some unknown reason emacs-eclim's version in MELPA does not work
;; for me.
(add-to-list 'load-path "~/local/emacs-eclim")
(use-package eclim
  :ensure nil
  :config
  (progn
    (global-eclim-mode)

    (after 'evil-leader
      (evil-leader/set-key-for-mode 'java-mode
        "rr" 'eclim-java-find-references
        "rj" 'eclim-java-find-declaration))
    ))

(provide 'my-eclim)
