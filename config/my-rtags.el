
;;------------------------------------------------------------------------------
;; rtags
;;------------------------------------------------------------------------------
(add-to-list 'load-path "~/local/rtags/share/emacs/site-lisp/rtags")
(use-package rtags
  :ensure nil
  :config
  (progn
    (setq rtags-jump-to-first-match nil)
    (setq rtags-use-helm t)
    (setq rtags-use-bookmarks nil)
    (rtags-enable-standard-keybindings)

    (after 'evil
      (define-key evil-motion-state-map "[r" 'rtags-previous-match)
      (define-key evil-motion-state-map "]r" 'rtags-next-match)
      (evil-set-command-property 'rtags-find-symbol-at-point :jump t)
      (evil-set-command-property 'rtags-find-all-references-at-point :jump t)
      (evil-set-command-property 'rtags-find-symbol :jump t)
      (evil-set-command-property 'rtags-find-virtuals-at-point :jump t)

      ;; (add-to-list 'evil-emacs-state-modes 'rtags-mode)
      (evil-set-initial-state 'rtags-mode 'normal)
      (evil-define-key 'normal rtags-mode-map
        (kbd "RET") 'rtags-select-and-remove-rtags-buffer
        (kbd "SPC") 'rtags-select-other-window)
      )

    (after 'evil-leader
      (evil-leader/set-key-for-mode 'c-mode
        "rj" 'rtags-find-symbol-at-point
        "rv" 'rtags-find-virtuals-at-point
        "rr" 'rtags-find-all-references-at-point
        "rm" 'rtags-display-summary
        "i"  'rtags-imenu)
      (evil-leader/set-key-for-mode 'c++-mode
        "rj" 'rtags-find-symbol-at-point
        "rv" 'rtags-find-virtuals-at-point
        "rr" 'rtags-find-all-references-at-point
        "rm" 'rtags-display-summary
        "i"  'rtags-imenu)
      (evil-leader/set-key
        "rp" 'rtags-start-process-unless-running
        "rl" 'rtags-show-rtags-buffer
        "rs" 'rtags-find-symbol
        "rf" 'rtags-find-file
        "rF" 'rtags-fixit
        "rn" 'rtags-location-stack-forward
        "rb" 'rtags-location-stack-backward))

    (defun my-c-mode-common-hook ()
      ;; my customizations for all of c-mode and related modes
      (local-set-key [s-mouse-1] 'rtags-find-symbol-at-point))
    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
    )
  )

(provide 'my-rtags)
