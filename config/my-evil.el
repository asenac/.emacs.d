(use-package evil-leader
  :config
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")

    (defun my/evil-leader-describe-bindings ()
      (interactive)
      (describe-bindings evil-leader/leader))
    ))

(use-package evil
  :config
  (progn
    ;; (use-package evil-magit)
    (use-package evil-mc
      :diminish 'evil-mc-mode)
    (use-package evil-nerd-commenter
      :commands evilnc-comment-or-uncomment-lines)
    (use-package evil-surround)
    (use-package evil-visual-mark-mode :defer t)

    (evil-mode 1)
    (global-evil-mc-mode  1)
    (global-evil-surround-mode 1)

    (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

    (evil-set-initial-state 'ibuffer-mode 'normal)

    (setq evil-emacs-state-cursor '("red" box))
    (setq evil-motion-state-cursor '("orange" box))
    (setq evil-normal-state-cursor '("green" box))
    (setq evil-visual-state-cursor '("orange" box))
    (setq evil-insert-state-cursor '("red" bar))
    (setq evil-replace-state-cursor '("red" bar))
    (setq evil-operator-state-cursor '("red" hollow))

    (define-key evil-visual-state-map (kbd "TAB") 'indent-region)
    ))

; When a region is active, as in evil’s visual mode, all the numbers within
; that region will be incremented/decremented (unlike in vim)
(use-package evil-numbers
  :config
  (progn
    (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)))

(provide 'my-evil)
