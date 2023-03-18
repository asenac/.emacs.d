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
        (define-key evil-insert-state-map (kbd "<C-return>") 'company-complete))))


;;------------------------------------------------------------------------------
;; lsp
;;------------------------------------------------------------------------------
(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-auto-guess-root t
        lsp-prefer-flymake nil
        ;; Kill the server when the last buffer in the workspace is deleted.
        lsp-keep-workspace-alive nil
        lsp-enable-file-watchers nil
        lsp-enable-imenu nil
        ;; This is just too slow in medium-size files
        lsp-lens-enable nil)

  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)

  ;; We need to use this instead of c-mode-common-hook etc. so it can
  ;; obey file-local and directory-local variable settings.
  ;; (add-hook 'hack-local-variables-hook
  ;;           (lambda () (when (derived-mode-p 'c-mode 'c++-mode) (lsp))))
  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        lsp-idle-delay 0.1)  ;; clangd is fast
  )

(after 'projectile
  (add-to-list 'projectile-globally-ignored-directories "contrib"))

;; (use-package lsp-ui
;;   :ensure t)

(after 'evil-leader
  (evil-leader/set-key-for-mode 'c-mode
    "lj" 'lsp-find-definition
    "lr" 'lsp-find-references)
  (evil-leader/set-key-for-mode 'c++-mode
    "lj" 'lsp-find-definition
    "lr" 'lsp-find-references))

(use-package helm-xref
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs-27))

(use-package helm-lsp)

;; (use-package company-lsp
;;   :ensure t
;;   :config
;;   (setq company-transformers nil
;;         company-lsp-async t
;;         company-lsp-cache-candidates nil))

;; (flycheck-define-checker
;;     python-mypy ""
;;     :command ("mypy"
;;               "--ignore-missing-imports" 
;;               "--python-version" "3.8"
;;               source-original)
;;     :error-patterns
;;     ((error line-start (file-name) ":" line ": error:" (message) line-end))
;;     :modes python-mode)

;; (add-to-list 'flycheck-checkers 'python-mypy t)
;; (flycheck-add-next-checker 'python-pylint 'python-mypy t)

(provide 'my-coding)
