;;------------------------------------------------------------------------------
;; exec-path-from-shell
;;------------------------------------------------------------------------------
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (progn
      (exec-path-from-shell-initialize)))

;;------------------------------------------------------------------------------
;; git-gutter
;;------------------------------------------------------------------------------
(use-package git-gutter
  :diminish 'git-gutter-mode
  :config
  (progn
    (global-git-gutter-mode t)
    (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
    (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
    (after 'evil
      (define-key evil-motion-state-map "[c" 'git-gutter:previous-hunk)
      (define-key evil-motion-state-map "]c" 'git-gutter:next-hunk))))

;;------------------------------------------------------------------------------
;; linum-mode is obsolete in Emacs 26
;; https://github.com/syohex/emacs-git-gutter/issues/156
;;------------------------------------------------------------------------------
(setq display-line-numbers "%4d \u2502 ")
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'c-mode-common-hook #'display-line-numbers-mode)

;;------------------------------------------------------------------------------
;; flyspell
;;------------------------------------------------------------------------------
(use-package flyspell
  :diminish 'flyspell-prog-mode)

;;------------------------------------------------------------------------------
;; recentf
;;------------------------------------------------------------------------------
(use-package recentf
  :defer 1
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-menu-items 100)
    (after 'evil
      (evil-set-initial-state 'recentf-mode 'normal))))

;;------------------------------------------------------------------------------
;; server
;;------------------------------------------------------------------------------
(use-package server
  :defer t
  :config
  (progn
    (unless (server-running-p)
      (server-start))))

;;------------------------------------------------------------------------------
;; minimap
;;------------------------------------------------------------------------------
(use-package minimap
  :commands minimap-mode)

;;------------------------------------------------------------------------------
;; fiplr
;;------------------------------------------------------------------------------
(use-package fiplr
  :commands fiplr-find-file
  :config
  (progn
    (setq fiplr-root-markers
          '(".git" ".svn" ".root" ".ycm_extra_conf.py"))))

;;------------------------------------------------------------------------------
;; browse-at-remote
;;------------------------------------------------------------------------------
(use-package browse-at-remote
  :commands browse-at-remote)

;;------------------------------------------------------------------------------
;; zoom-window
;;------------------------------------------------------------------------------
(use-package zoom-window
  :commands zoom-window-zoom
  :config
  (progn
    (setq zoom-window-mode-line-color "DarkGreen")))

;;------------------------------------------------------------------------------
;; multi-term
;;------------------------------------------------------------------------------
(use-package multi-term
  :commands multi-term)

;;------------------------------------------------------------------------------
;; neotree
;;------------------------------------------------------------------------------
(use-package neotree
  :commands neotree)

;;------------------------------------------------------------------------------
;; org-mode
;;------------------------------------------------------------------------------
(use-package org
  :defer t
  :config
  (progn
    (use-package org-bullets)))

;;------------------------------------------------------------------------------
;; telephone-line
;;------------------------------------------------------------------------------
(use-package telephone-line
  :disabled t
  :config
  (progn
    (setq telephone-line-lhs
          '((evil   . (telephone-line-evil-tag-segment))
            (accent . (telephone-line-vc-segment
                       telephone-line-erc-modified-channels-segment
                       telephone-line-process-segment))
            (nil    . (telephone-line-minor-mode-segment
                       telephone-line-buffer-segment))))
    (setq telephone-line-rhs
          '((nil    . (telephone-line-misc-info-segment))
            (accent . (telephone-line-major-mode-segment))
            (evil   . (telephone-line-airline-position-segment))))
    (telephone-line-mode 1)))

(use-package spaceline
  :config
  (progn
    (require 'spaceline-config)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (setq powerline-default-separator 'wave)
    (set-face-attribute 'spaceline-evil-emacs nil :background "#be84ff")
    (set-face-attribute 'spaceline-evil-insert nil :background "#5fd7ff")
    (set-face-attribute 'spaceline-evil-motion nil :background "#ae81ff")
    (set-face-attribute 'spaceline-evil-normal nil :background "#a6e22e")
    (set-face-attribute 'spaceline-evil-replace nil :background "#f92672")
    (set-face-attribute 'spaceline-evil-visual nil :background "#fd971f")

    (spaceline-toggle-buffer-size-off)
    (spaceline-toggle-which-function-off)

    (spaceline-spacemacs-theme '(projectile-root))))

;;------------------------------------------------------------------------------
;; indent-guide
;;------------------------------------------------------------------------------
(use-package indent-guide
  :diminish 'indent-guide-mode
  :defer t
  :config
  (progn
    (indent-guide-global-mode)))

;;------------------------------------------------------------------------------
;; undo-tree
;;------------------------------------------------------------------------------
(use-package undo-tree
  :diminish 'undo-tree-mode)

;;------------------------------------------------------------------------------
;; projectile
;;------------------------------------------------------------------------------
(use-package projectile
  ;; :diminish 'projectile-mode
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-mode-line
          '(:eval (if (file-remote-p default-directory)
                      " P"
                    (format " P[%s]" (projectile-project-name)))))

    (defun my/projectile-multi-term-in-root ()
      "Invoke `multi-term' in the project's root."
      (interactive)
      (projectile-with-default-dir (projectile-project-root) (multi-term)))
    ))

;;------------------------------------------------------------------------------
;; restclient
;;------------------------------------------------------------------------------
(use-package restclient
  :commands restclient-mode
  :config
  (progn
    (use-package restclient-helm)))

;;------------------------------------------------------------------------------
;; rainbow-delimiters
;;------------------------------------------------------------------------------
(use-package rainbow-delimiters
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

;;------------------------------------------------------------------------------
;; dashboard
;;------------------------------------------------------------------------------
(use-package dashboard
  :config
  (progn
    (setq dashboard-items '((recents  . 20)
                            (projects . 10)))
    (dashboard-setup-startup-hook)))

;;------------------------------------------------------------------------------
;; other packages
;;------------------------------------------------------------------------------
(use-package magit :defer t)
(use-package git-timemachine
  :defer t
  :commands git-timemachine)
(use-package cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))
(use-package vimrc-mode
  :mode (("/\\.vimrc\\'" . vimrc-mode)
         ("/vimrc\\'" . vimrc-mode)
         ("\\.vim\\'" . vimrc-mode)))
(use-package js2-mode
  :mode "\\.js\\'")
(use-package json-mode
  :mode "\\.json\\'")
(use-package bison-mode
  :mode (("\\.ypp\\'" . bison-mode)))
(use-package htmlize :defer t)
(use-package simple-httpd :defer t)

(use-package string-inflection
  :commands string-inflection-all-cycle)

(use-package gud :ensure nil :commands lldb)

(use-package hydra)
(use-package paradox)
(use-package quickrun)

(provide 'my-pkgs)
