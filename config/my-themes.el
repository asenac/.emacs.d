(use-package monokai-theme
  :disabled t
  :demand
  :if (display-graphic-p)
  :config (load-theme 'monokai t))

(use-package zenburn-theme
  :demand
  :if (display-graphic-p)
  :config (load-theme 'zenburn t))

(if (not (display-graphic-p))
    (load-theme 'tango-dark t))

(provide 'my-themes)
