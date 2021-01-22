;;------------------------------------------------------------------------------
;; clang-format
;;------------------------------------------------------------------------------
(use-package clang-format
  :commands clang-format)

;;------------------------------------------------------------------------------
;; rtags
;;------------------------------------------------------------------------------
(defvar rtags-path "~/local/rtags/share/emacs/site-lisp/rtags")
(add-to-list 'load-path rtags-path)
(use-package rtags
  :disabled t
  :if (file-directory-p rtags-path)
  :unless (string-equal system-type "windows-nt")
  :ensure nil
  :config
  (progn
    (require 'helm-rtags)
    (require 'flycheck-rtags)
    (setq rtags-jump-to-first-match nil)
    (setq rtags-display-result-backend 'helm)
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

    (defun my-c-mode-hook ()
      ;; my customizations for all of c-mode and related modes
      (if (not (string-equal major-mode "bison-mode"))
          (flycheck-select-checker 'rtags))
      (local-set-key [s-mouse-1] 'rtags-find-symbol-at-point))
    (if (not (string-equal system-type "windows-nt"))
        (progn
          (add-hook 'c-mode-hook 'my-c-mode-hook)
          (add-hook 'c++-mode-hook 'my-c-mode-hook)))

    (after 'hydra
      (defhydra hydra-rtags (:color pink :hint nil :exit t)
        "

[_j_]: symbol at point      [_s_]: find symbol    [_p_]: start process
[_v_]: virtuals at point    [_f_]: find file      [_l_]: buffer
[_r_]: refs at point        [_F_]: fixit
[_m_]: summary              [_i_]: imenu

"
        ("j" rtags-find-symbol-at-point)
        ("v" rtags-find-virtuals-at-point)
        ("r" rtags-find-all-references-at-point)
        ("m" rtags-display-summary)
        ("i" rtags-imenu)
        ("p" rtags-start-process-unless-running)
        ("l" rtags-show-rtags-buffer)
        ("s" rtags-find-symbol)
        ("f" rtags-find-file)
        ("F" rtags-fixit)
        ("q" nil "cancel"))
      (after 'evil-leader
        (evil-leader/set-key-for-mode 'c-mode
          "rj" 'rtags-find-symbol-at-point
          "rr" 'rtags-find-all-references-at-point
          "rv" 'rtags-find-virtuals-at-point
          "rm" 'rtags-display-summary
          "i"  'rtags-imenu)
        (evil-leader/set-key-for-mode 'c++-mode
          "rj" 'rtags-find-symbol-at-point
          "rr" 'rtags-find-all-references-at-point
          "rv" 'rtags-find-virtuals-at-point
          "rm" 'rtags-display-summary
          "i"  'rtags-imenu)
        (evil-leader/set-key
          ",r" 'hydra-rtags/body
          "rp" 'rtags-start-process-unless-running
          "rl" 'rtags-show-rtags-buffer
          "rs" 'rtags-find-symbol
          "rf" 'rtags-find-file
          "rF" 'rtags-fixit
          "rn" 'rtags-location-stack-forward
          "rb" 'rtags-location-stack-backward)))
    ))

(provide 'my-cpp)
