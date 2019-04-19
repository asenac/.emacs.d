(defun my-c-mode-hook ()
    ;; my customizations for all of c-mode and related modes
    (if (not (string-equal major-mode "bison-mode"))
        (flycheck-select-checker 'rtags))
    (local-set-key [s-mouse-1] 'rtags-find-symbol-at-point))
(if (not (string-equal system-type "windows-nt"))
    (progn
      (add-hook 'c-mode-hook 'my-c-mode-hook)
      (add-hook 'c++-mode-hook 'my-c-mode-hook)))

(provide 'my-cpp)
