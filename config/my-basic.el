;;------------------------------------------------------------------------------
;; general configuration
;;------------------------------------------------------------------------------

;; disable menu bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq enable-local-variables :safe)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(setq-default tab-width 4)

;; whitespace.el configuration
(use-package whitespace
  :diminish whitespace-mode
  :config
  (progn
    (setq whitespace-style
          '(face empty lines-tail tabs tab-mark trailing))
    (global-whitespace-mode 1)
    (add-hook 'prog-mode-hook
              (lambda ()
                ;; (whitespace-mode 1)
                ;; show unncessary whitespace that can mess up your diff
                (setq show-trailing-whitespace 1)))
                (setq whitespace-line-column 120)
    ))

(setq-default indent-tabs-mode nil)
(setq c-default-style "linux"
      c-basic-style: "bsd"
      c-basic-offset 4)

;; setup emacs file to open in lisp-mode
(add-to-list 'auto-mode-alist '("emacs" . lisp-mode))

;; turn off alarms completely
(setq ring-bell-function 'ignore)

(setq window-min-height 1)

;; change the directory where backup files are created
(setq backup-directory-alist `((".*" . "~/.emacs.d/saves")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/saves/" t)))

;; disable lock files
(setq create-lockfiles nil)

;; save history
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)

;; Show the current function name in the header line
(which-function-mode)
(setq which-func-unknown "n/a")

;; always trucate long lines
(setq-default truncate-lines t)

;; Start maximized, please
;; (custom-set-variables
;;  '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Answer "y" rather than "yes"
(defalias 'yes-or-no-p 'y-or-n-p)

;; (setq shell-file-name "/bin/bash")

;;------------------------------------------------------------------------------
;; font configuration
;;------------------------------------------------------------------------------
(cond
 ((string-equal system-type "windows-nt") ; Windows
  (when (member "Consolas" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Consolas-10"))
    (add-to-list 'default-frame-alist '(font . "Consolas-10"))))
 ((string-equal system-type "darwin") ; Mac OS X
  (when (member "Monaco" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Monaco-12"))
    (add-to-list 'default-frame-alist '(font . "Monaco-12"))))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "Inconsolata for Powerline" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Inconsolata for Powerline-11"))
    (add-to-list 'default-frame-alist '(font . "Inconsolata for Powerline-11")))))

(provide 'my-basic)
