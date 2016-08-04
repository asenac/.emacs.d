(use-package evil-leader
  :config
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")

    (defun my/evil-leader-describe-bindings ()
      (interactive)
      (describe-bindings evil-leader/leader))
    (evil-leader/set-key
      "h" 'my/evil-leader-describe-bindings)
    ))

(use-package evil
  :config
  (progn
    ;; (use-package evil-magit)
    (use-package evil-mc)
    (use-package evil-nerd-commenter
      :commands evilnc-comment-or-uncomment-lines
      :init
      (progn
        (after 'evil-leader
          (evil-leader/set-key
            "c SPC" 'evilnc-comment-or-uncomment-lines))))
    (use-package evil-surround)
    (use-package evil-visual-mark-mode :defer t)

    (evil-mode 1)
    (global-evil-mc-mode  1)
    (global-evil-surround-mode 1)

    (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

    (evil-set-initial-state 'ibuffer-mode 'normal)

    (hbin-remove-mm-lighter 'evil-mc-mode)

    (setq evil-emacs-state-cursor '("red" box))
    (setq evil-motion-state-cursor '("orange" box))
    (setq evil-normal-state-cursor '("green" box))
    (setq evil-visual-state-cursor '("orange" box))
    (setq evil-insert-state-cursor '("red" bar))
    (setq evil-replace-state-cursor '("red" bar))
    (setq evil-operator-state-cursor '("red" hollow))
    ))

(provide 'my-evil)
