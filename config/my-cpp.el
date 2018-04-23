(defun cquery//enable ()
  (condition-case nil
      (lsp-cquery-enable)
    (user-error nil)))

(defun my/company-lsp-hook ()
  (add-to-list 'company-backends 'company-lsp))

(use-package lsp-mode)
(use-package lsp-ui
  :config
  (progn
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)))
(use-package company-lsp
  :init (add-hook 'c-mode-common-hook #'my/company-lsp-hook))
(use-package cquery
  :init (add-hook 'c-mode-common-hook #'cquery//enable)
  :commands lsp-cquery-enable
  :config
  (progn
    (setq cquery-executable "~/dev/cquery/build/release/bin/cquery")
    (after 'evil-leader
      (evil-leader/set-key-for-mode 'c++-mode
        "cj" 'xref-find-definitions
        "cr" 'xref-find-references))
    ))
(use-package helm-xref
  :config
  (progn
    (setq xref-show-xrefs-function 'helm-xref-show-xrefs)))

(provide 'my-cpp)
