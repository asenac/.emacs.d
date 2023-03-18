(after 'evil-leader
  (evil-leader/set-key
    "*"  'helm-occur
    "A"  'helm-resume
    "G"  'helm-google
    "M"  'helm-mini
    "W"  'whitespace-mode
    "b"  'ibuffer
    "e"  'helm-find-files
    "f"  'fiplr-find-file
    "h"  'my/evil-leader-describe-bindings
    "i"  'helm-imenu
    "k"  'kill-buffer
    "K"  'kill-buffer-and-window
    "le" 'helm-flycheck ;; list errors
    ;; "m"  'minimap-mode
    "of" 'browse-at-remote
    "s"  'helm-swoop
    "t"  'multi-term
    "w"  'toggle-truncate-lines
    "x"  'helm-M-x
    "y"  'helm-show-kill-ring
    "z"  'zoom-window-zoom

    ;; code formatting
    "c SPC" 'evilnc-comment-or-uncomment-lines
    "cf" 'clang-format
    "cs" 'string-inflection-all-cycle

    ;; project-related bindings
    "ab" 'projectile-compile-project
    "ac" 'projectile-run-project
    "ad" 'projectile-discover-projects-in-directory
    "ae" 'helm-buffers-list
    "af" 'ff-find-other-file
    "ag" 'helm-projectile-rg
    "ak" 'projectile-kill-buffers
    "ao" 'helm-projectile-switch-project
    "as" 'helm-projectile
    ;; "as" 'helm-find-files
    "at" 'my/projectile-multi-term-in-root
    "aT" 'projectile-run-eshell
    "ax" 'my/helm-execute
    "az" 'my/helm-gtest
    ))

(provide 'my-bindings)
