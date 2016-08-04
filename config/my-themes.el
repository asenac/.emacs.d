(use-package monokai-theme)
;; (use-package gruvbox-theme)
;; (use-package moe-theme)

(if (display-graphic-p)
    (load-theme 'monokai t)
  (load-theme 'tango-dark t))

(provide 'my-themes)
