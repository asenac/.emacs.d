(use-package monokai-theme
  :disabled t
  :demand
  :if (display-graphic-p)
  :config (load-theme 'monokai t))

(use-package zenburn-theme
  :disabled t
  :demand
  :if (display-graphic-p)
  :config (load-theme 'zenburn t))

(use-package atom-one-dark-theme
  :demand
  :if (display-graphic-p)
  :config (load-theme 'atom-one-dark t))


(if (not (display-graphic-p))
    (load-theme 'tango-dark t))

(provide 'my-themes)
