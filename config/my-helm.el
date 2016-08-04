(use-package helm
  :config
  (progn
    (use-package helm-ack :commands helm-ack)
    (use-package helm-ag :commands helm-ag)
    (use-package helm-swoop :commands helm-swoop)
    (use-package helm-c-yasnippet
      :commands (helm-yas-complete
                 helm-yas-visit-snippet-file
                 helm-yas-create-snippet-on-region))
    ;; (use-package helm-company)
    (use-package helm-describe-modes :commands heml-describe-modes)
    (use-package helm-flycheck :commands helm-flycheck)
    (use-package helm-google :commands helm-google)
    (use-package helm-helm-commands :commands helm-helm-commands)
    (use-package helm-mode-manager
      :commands (helm-switch-major-mode
                 helm-enable-major-mode
                 helm-disable-major-mode))
    (use-package helm-mt :commands helm-mt)
    (use-package helm-projectile)
    (use-package helm-unicode :commands helm-unicode)

    (setq helm-split-window-in-side-p t)
    (helm-mode 1)
    (global-set-key (kbd "M-x") #'helm-M-x)
    (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
    (global-set-key (kbd "C-x C-f") #'helm-find-files)

    (define-key helm-map [mouse-1] nil)
    (define-key helm-map [mouse-2] nil)

    (hbin-remove-mm-lighter 'helm-mode)

    (defun my/helm-ag--project-root ()
      (cl-loop for dir in '("build/" ".root" ".git/" ".hg/" ".svn/" ".ycm_extra_conf.py")
               when (locate-dominating-file default-directory dir)
               return it))

    (defun my/helm-ag-project-root ()
      (interactive)
      (let ((rootdir (my/helm-ag--project-root)))
        (unless rootdir
          (error "Could not find the project root. "))
        (helm-ag rootdir)))

    (after 'evil
      (add-to-list 'evil-emacs-state-modes 'helm-major-mode))

    (after 'evil-leader
      (evil-leader/set-key
        "*" 'helm-occur
        "ao" 'helm-projectile-switch-project
        "as" 'helm-projectile
        "lp" 'helm-projectile
        "ae" 'helm-buffers-list
        "ag" 'helm-projectile-ag
        "s" 'helm-swoop
        "M" 'helm-mini
        "i" 'helm-imenu
        "G" 'helm-google
        "le" 'helm-flycheck ;; list errors
        "e" 'helm-find-files
        "y" 'helm-show-kill-ring))
  ))

(provide 'my-helm)
