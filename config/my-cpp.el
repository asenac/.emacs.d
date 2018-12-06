(defun cquery//enable ()
  (condition-case nil
      (lsp-cquery-enable)
    (user-error nil)))

(use-package lsp-mode
  :diminish 'lsp-mode)
(use-package lsp-ui
  :config
  (progn
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)))
(use-package company-lsp
  :disabled t
  :config
  (progn
    (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)))
(use-package cquery
  :commands lsp-cquery-enable
  :config
  (progn
    (setq cquery-executable "~/dev/cquery/build/release/bin/cquery")
    ;(setq cquery-extra-args '("--log-file=/tmp/cq.log"))
    (after 'evil-leader
      (evil-leader/set-key-for-mode 'c++-mode
        "cj" 'xref-find-definitions
        "cr" 'xref-find-references))
    ))
(use-package helm-xref
  :config
  (progn
    (setq xref-show-xrefs-function 'helm-xref-show-xrefs)))

(defun my-c-mode-hook ()
    ;; (cquery//enable)
    ;; (add-to-list 'company-backends 'company-lsp)
    ;; my customizations for all of c-mode and related modes
    (if (not (string-equal major-mode "bison-mode"))
        (flycheck-select-checker 'rtags))
        ;; (flycheck-select-checker 'lsp-ui))
    (local-set-key [s-mouse-1] 'rtags-find-symbol-at-point)
    )
(if (not (string-equal system-type "windows-nt"))
    (progn
      (add-hook 'c-mode-hook 'my-c-mode-hook)
      (add-hook 'c++-mode-hook 'my-c-mode-hook)))

(provide 'my-cpp)
