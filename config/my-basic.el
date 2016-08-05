;;------------------------------------------------------------------------------
;; general configuration
;;------------------------------------------------------------------------------

;; disable menu bar
(menu-bar-mode -1)
(if window-system
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

(setq enable-local-variables :safe)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(setq-default tab-width 4)

(defface extra-whitespace-face
  '((t (:background "pale green")))
  "Used for tabs and such.")
(defvar my-extra-keywords
  '(("\t" . 'extra-whitespace-face)))

(add-hook 'prog-mode-hook
          (lambda ()
            ;; Draw tabs with the same color as trailing whitespace
            (font-lock-add-keywords nil my-extra-keywords)
            ;; show unncessary whitespace that can mess up your diff
            (setq show-trailing-whitespace 1)))

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
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Answer "y" rather than "yes"
(defalias 'yes-or-no-p 'y-or-n-p)

;;------------------------------------------------------------------------------
;; font configuration
;;------------------------------------------------------------------------------
(cond
 ((string-equal system-type "darwin") ; Mac OS X
  (when (member "Monaco" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Monaco-12"))
    (add-to-list 'default-frame-alist '(font . "Monaco-12"))))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "Inconsolata" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Inconsolata-12"))
    (add-to-list 'default-frame-alist '(font . "Inconsolata-12")))))

(provide 'my-basic)
