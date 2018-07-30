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

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(if (not (display-graphic-p))
    (load-theme 'tango-dark t))

(provide 'my-themes)
