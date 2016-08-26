;;------------------------------------------------------------------------------
;; exec-path-from-shell
;;------------------------------------------------------------------------------
(use-package exec-path-from-shell
  :config
  (progn
    (exec-path-from-shell-initialize)))

;;------------------------------------------------------------------------------
;; linum-relative
;;------------------------------------------------------------------------------
(use-package linum-relative
  :config
  (progn
    (global-linum-mode)
    (if window-system
        (setq linum-relative-format "%4s")
      (setq linum-relative-format "%4s \u2502 "))
    (if window-system
        (setq linum-format "%4s")
      (setq linum-format "%4s \u2502 "))
    (setq linum-relative-current-symbol "")
    (linum-relative-on)

    (after 'evil
      (add-hook
       'evil-emacs-state-entry-hook
       (lambda ()
         (linum-relative-off)))

      (add-hook
       'evil-emacs-state-exit-hook
       (lambda ()
         (linum-relative-on))))

    (after 'evil-leader
      (evil-leader/set-key
        "lr" 'linum-relative-toggle))))

;;------------------------------------------------------------------------------
;; git-gutter
;;------------------------------------------------------------------------------
(use-package git-gutter
  :diminish 'git-gutter-mode
  :config
  (progn
    (global-git-gutter-mode t)
    (git-gutter:linum-setup)
    (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
    (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
    (after 'evil
      (define-key evil-motion-state-map "[c" 'git-gutter:previous-hunk)
      (define-key evil-motion-state-map "]c" 'git-gutter:next-hunk))))

;;------------------------------------------------------------------------------
;; yasnippet
;;------------------------------------------------------------------------------
(use-package yasnippet
  :diminish 'yas-minor-mode
  :defer 2
  :config
  (progn
    ;; (yas-global-mode 1)

    (after 'evil
      ;; Instead of enabling yasnippet globally I think it's better to enable it
      ;; when entering insert mode
      (add-hook
       'evil-insert-state-entry-hook
       (lambda ()
         (if (string-equal major-mode "term-mode")
             (yas-minor-mode -1)
           (yas-minor-mode 1))))

      (add-hook
       'evil-insert-state-exit-hook
       (lambda ()
         (yas-minor-mode -1))))

    ;; (setq yas-snippet-dirs '("~/.emacs.d/el-get/yasnippet-snippets"))
    (eval-after-load "yasnippet"
      '(progn
         (yas-reload-all)))))

;;------------------------------------------------------------------------------
;; company
;;------------------------------------------------------------------------------
(use-package company
  :diminish 'company-mode
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (after 'evil
        (define-key evil-insert-state-map (kbd "<C-return>") 'company-complete))

    ;; Completion for java
    (require 'company-eclim)

    (defun my/java-mode-hook ()
      (add-to-list 'company-backends 'company-eclim))
    (add-hook 'java-mode-hook 'my/java-mode-hook)

    (use-package company-jedi
      ;; Disabled, completion provided by ycmd
      :disabled t
      :config
      (progn
        (defun my/python-mode-hook ()
          (add-to-list 'company-backends 'company-jedi))
        (add-hook 'python-mode-hook 'my/python-mode-hook)))))

;;------------------------------------------------------------------------------
;; flycheck
;;------------------------------------------------------------------------------
(use-package flycheck
  :config
  (progn
    (setq flycheck-mode-line-prefix "F")

    (after 'evil
      (define-key evil-motion-state-map "[e" 'flycheck-previous-error)
      (define-key evil-motion-state-map "]e" 'flycheck-next-error))

    (add-hook 'after-init-hook 'global-flycheck-mode)))

;;------------------------------------------------------------------------------
;; ycmd
;;------------------------------------------------------------------------------
(use-package ycmd
  :diminish 'ycmd-mode
  :config
  (progn
    (let ((option-found (my/return-first-file-found
                         ["~/.emacs.d/static/ycmd/ycmd"
                         "~/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd"])))
      (if option-found
          (progn
            (message (concat "Ycmd found at: " option-found))
            (set-variable 'ycmd-server-command '("python"))
            (add-to-list
             'ycmd-server-command
             (expand-file-name option-found) t)
            (set-variable 'ycmd-extra-conf-whitelist '("~/*"))
            (ycmd-setup)

            (after 'company
              (use-package company-ycmd)
              (company-ycmd-setup))

            (after 'flycheck
              (use-package flycheck-ycmd)
              (flycheck-ycmd-setup)))
        ; Not found
        (message "Ycmd not found!")))))

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
  :commands minimap-mode
  :init
  (progn
    (after 'evil-leader
      (evil-leader/set-key
        "m" 'minimap-mode))))

;;------------------------------------------------------------------------------
;; clang-format
;;------------------------------------------------------------------------------
(use-package clang-format
  :commands clang-format
  :init
  (progn
    (after 'evil-leader
      (evil-leader/set-key
        "cf" 'clang-format))))

;;------------------------------------------------------------------------------
;; fiplr
;;------------------------------------------------------------------------------
(use-package fiplr
  :commands fiplr-find-file
  :init
  (progn
    (after 'evil-leader
      (evil-leader/set-key
        "f" 'fiplr-find-file)))
  :config
  (progn
    (setq fiplr-root-markers
          '(".git" ".svn" ".root" ".ycm_extra_conf.py"))))

;;------------------------------------------------------------------------------
;; browse-at-remote
;;------------------------------------------------------------------------------
(use-package browse-at-remote
  :commands browse-at-remote
  :init
  (progn
    (after 'evil-leader
      (evil-leader/set-key
        "of" 'browse-at-remote))))

;;------------------------------------------------------------------------------
;; zoom-window
;;------------------------------------------------------------------------------
(use-package zoom-window
  :commands zoom-window-zoom
  :init
  (progn
    (after 'evil-leader
      (evil-leader/set-key
        "z" 'zoom-window-zoom)))
  :config
  (progn
    (setq zoom-window-mode-line-color "DarkGreen")))

;;------------------------------------------------------------------------------
;; multi-term
;;------------------------------------------------------------------------------
(use-package multi-term
  :commands multi-term
  :init
  (progn
    (after 'evil-leader
      (evil-leader/set-key
        "t" 'multi-term))))

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
;; eopengrok
;;------------------------------------------------------------------------------
(use-package eopengrok
  :commands (eopengrok-make-index
             eopengrok-find-definition
             eopengrok-find-file
             eopengrok-find-reference
             eopengrok-find-text
             eopengrok-find-history
             eopengrok-switch-to-buffer
             eopengrok-jump-to-source)
  :init
  (progn
    (after 'evil-leader
      (evil-leader/set-key
        "ogi" 'eopengrok-make-index
        "ogd" 'eopengrok-find-definition
        "ogf" 'eopengrok-find-file
        "ogr" 'eopengrok-find-reference
        "ogt" 'eopengrok-find-text
        "ogh" 'eopengrok-find-history
        "ogb" 'eopengrok-switch-to-buffer
        "ogj" 'eopengrok-jump-to-source)))
  :config
  (progn
    (setq eopengrok-jar (expand-file-name "~/local/clj-opengrok-0.3.0-standalone.jar"))
    (if (string-equal system-type "darwin") ; Mac OS X
        (setq eopengrok-ctags "/usr/local/Cellar/ctags/5.8/bin/ctags")
      (ding)
      (setq eopengrok-ctags "/usr/bin/ctags"))))

;;------------------------------------------------------------------------------
;; telephone-line
;;------------------------------------------------------------------------------
(use-package telephone-line
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
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-mode-line
          '(:eval (if (file-remote-p default-directory)
                      " P"
                    (format " P[%s]" (projectile-project-name)))))

    (defun projectile-multi-term-in-root ()
      "Invoke `multi-term' in the project's root."
      (interactive)
      (projectile-with-default-dir (projectile-project-root) (multi-term)))

    (after 'evil-leader
      (evil-leader/set-key
        "ad" 'projectile-discover-projects-in-directory
        "ak" 'projectile-kill-buffers
        "at" 'projectile-multi-term-in-root
        "ac" 'projectile-run-shell-command-in-root
        "ab" 'projectile-compile-project))))

;;------------------------------------------------------------------------------
;; restclient
;;------------------------------------------------------------------------------
(use-package restclient
  :commands restclient-mode
  :config
  (progn
    (use-package restclient-helm)))

;;------------------------------------------------------------------------------
;; other packages
;;------------------------------------------------------------------------------
(use-package magit :defer t)
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
(use-package htmlize :defer t)
(use-package simple-httpd :defer t)

(provide 'my-pkgs)
