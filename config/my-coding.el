;;------------------------------------------------------------------------------
;; eopengrok
;;------------------------------------------------------------------------------
(use-package eopengrok
  :unless (string-equal system-type "windows-nt")
  :commands (eopengrok-make-index
             eopengrok-find-definition
             eopengrok-find-file
             eopengrok-find-reference
             eopengrok-find-text
             eopengrok-find-history
             eopengrok-switch-to-buffer
             eopengrok-jump-to-source)
  :init
  (progn
    (after 'evil-leader
      (evil-leader/set-key
        "ogi" 'eopengrok-make-index
        "ogd" 'eopengrok-find-definition
        "ogf" 'eopengrok-find-file
        "ogr" 'eopengrok-find-reference
        "ogt" 'eopengrok-find-text
        "ogh" 'eopengrok-find-history
        "ogb" 'eopengrok-switch-to-buffer
        "ogj" 'eopengrok-jump-to-source)))
  :config
  (progn
    (setq eopengrok-jar (expand-file-name "~/local/clj-opengrok-0.3.0-standalone.jar"))
    (if (string-equal system-type "darwin") ; Mac OS X
        (setq eopengrok-ctags "/usr/local/Cellar/ctags/5.8/bin/ctags")
      (setq eopengrok-ctags "/usr/bin/ctags"))))

;;------------------------------------------------------------------------------
;; ycmd
;;------------------------------------------------------------------------------
(use-package ycmd
  :disabled t
  :unless (string-equal system-type "windows-nt")
  ;; :diminish 'ycmd-mode
  :config
  (progn
    (let ((option-found (my/return-first-file-found
                         ["~/.emacs.d/static/ycmd/ycmd"
                         "~/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd"])))
      (if option-found
          (progn
            (message (concat "Ycmd found at: " option-found))
            (set-variable 'ycmd-server-command '("python"))
            (add-to-list
             'ycmd-server-command
             (expand-file-name option-found) t)
            (set-variable 'ycmd-extra-conf-whitelist '("~/*" "/*"))
            (ycmd-setup)

            (after 'company
              (use-package company-ycmd)
              (company-ycmd-setup))

            (after 'flycheck
              (use-package flycheck-ycmd
                :config (flycheck-ycmd-setup))))
        ; Not found
        (message "Ycmd not found!")))))

;;------------------------------------------------------------------------------
;; flycheck
;;------------------------------------------------------------------------------
(use-package flycheck
  ;; :diminish 'flycheck-mode
  :config
  (progn
    (setq flycheck-mode-line-prefix "F")

    (after 'evil
      (define-key evil-motion-state-map "[e" 'flycheck-previous-error)
      (define-key evil-motion-state-map "]e" 'flycheck-next-error))

    (add-hook 'after-init-hook 'global-flycheck-mode)))

;;------------------------------------------------------------------------------
;; yasnippet
;;------------------------------------------------------------------------------
(use-package yasnippet
  ;; :diminish 'yas-minor-mode
  :defer 2
  :config
  (progn
    ;; (yas-global-mode 1)

    (after 'evil
      ;; Instead of enabling yasnippet globally I think it's better to enable it
      ;; when entering insert mode
      (add-hook
       'evil-insert-state-entry-hook
       (lambda ()
         (if (string-equal major-mode "term-mode")
             (yas-minor-mode -1)
           (yas-minor-mode 1))))

      (add-hook
       'evil-insert-state-exit-hook
       (lambda ()
         (yas-minor-mode -1))))

    (setq my-yasnippets (expand-file-name "~/.emacs.d/snippets"))
    (if (and (file-exists-p my-yasnippets)
             (not (member my-yasnippets yas-snippet-dirs)))
        (add-to-list 'yas-snippet-dirs my-yasnippets))

    (yas-reload-all)
    ))

(use-package yasnippet-snippets
  :defer 2)

;;------------------------------------------------------------------------------
;; company
;;------------------------------------------------------------------------------
(use-package company
  ;; :diminish 'company-mode
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (after 'evil
        (define-key evil-insert-state-map (kbd "<C-return>") 'company-complete))

    (use-package company-jedi
      ;; Disabled, completion provided by ycmd
      :disabled t
      :config
      (progn
        (defun my/python-mode-hook ()
          (add-to-list 'company-backends 'company-jedi))
        (add-hook 'python-mode-hook 'my/python-mode-hook)))))

(provide 'my-coding)
