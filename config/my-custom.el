;;------------------------------------------------------------------------------
;; evil
;;------------------------------------------------------------------------------

(defun evil-normalize-all-buffers ()
  "Force a drop to normal state."
  (unless (eq evil-state 'normal)
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (unless (or (minibufferp)
                  (eq evil-state 'emacs))
        (evil-force-normal-state)))
    (message "Dropped back to normal state in all buffers")))

(defvar evil-normal-timer
  (run-with-idle-timer 30 t #'evil-normalize-all-buffers)
  "Drop back to normal state after idle for 30 seconds.")

;; replace current word or selection using vim style for evil mode
(defun evil-replace-word-selection()
  (interactive)
  (if (use-region-p)
      (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
        (if (= (length selection) 0)
            (message "empty string")
          (evil-ex (concat "'<,'>s/" selection "/"))
          ))
    (evil-ex (concat "%s/\\<" (thing-at-point 'symbol) "\\>/"))))

(evil-leader/set-key
  "ar" 'evil-replace-word-selection)

(define-key evil-normal-state-map (kbd "TAB") 'evil-window-next)

;; Don't wait for any other keys after escape is pressed.
(setq evil-esc-delay 0)

;;------------------------------------------------------------------------------
;; gdb
;;------------------------------------------------------------------------------
(eval-after-load "gud"
  '(progn
     (define-key gud-mode-map (kbd "<up>") 'comint-previous-input)
     (define-key gud-mode-map (kbd "<down>") 'comint-next-input)))
(add-hook 'gud-mode-hook (lambda() (linum-mode -1)))
(setq gdb-many-windows t)

;;------------------------------------------------------------------------------
;; term
;;------------------------------------------------------------------------------
(add-hook
 'term-mode-hook
 (lambda()
   (linum-mode -1)
   (yas-minor-mode -1)))

;;------------------------------------------------------------------------------
;; helm-evil incompatibility
;;------------------------------------------------------------------------------

(defun my/helm-prepare-display ()
  ;; workaround for a helm-evil incompatibility
  ;; see https://github.com/syl20bnr/spacemacs/issues/3700
  (global-linum-mode -1)
  (when helm-prevent-escaping-from-minibuffer
    (define-key evil-motion-state-map [down-mouse-1] nil)))

(defun my/restore-previous-display-config ()
  ;; workaround for a helm-evil incompatibility
  ;; see https://github.com/syl20bnr/spacemacs/issues/3700
  (global-linum-mode 1)
  (when helm-prevent-escaping-from-minibuffer
    (define-key evil-motion-state-map [down-mouse-1] 'evil-mouse-drag-region)))

(add-hook 'helm-after-initialize-hook 'my/helm-prepare-display)
(add-hook 'helm-cleanup-hook 'my/restore-previous-display-config)

;;------------------------------------------------------------------------------
;; clipboard
;;------------------------------------------------------------------------------

;; Don't use system clipboard by default
;; (setq-default
;;   interprogram-cut-function   nil
;;   interprogram-paste-function nil)

;; visual selection should not override the clipboard
(setq-default x-select-enable-clipboard nil)
(fset 'evil-visual-update-x-selection 'ignore)

;;------------------------------------------------------------------------------
;; custom scripts: making scripts executable on save
;; link: http://emacswiki.org/emacs/MakingScriptsExecutableOnSave
;;------------------------------------------------------------------------------
; Check for shebang magic in file after save, make executable if found.
(setq my-shebang-patterns
        (list "^#!/usr/.*/perl\\(\\( \\)\\|\\( .+ \\)\\)-w *.*"
            "^#!/usr/.*/env"
            "^#!/usr/.*/sh"
            "^#!/usr/.*/bash"
            "^#!/bin/sh"
            "^#!/bin/bash"))
(add-hook
    'after-save-hook
    (lambda ()
    (if (not (= (shell-command (concat "test -x " (buffer-file-name))) 0))
        (progn
            ;; This puts message in *Message* twice, but minibuffer
            ;; output looks better.
            (message (concat "Wrote " (buffer-file-name)))
            (save-excursion
            (goto-char (point-min))
            ;; Always checks every pattern even after
            ;; match.  Inefficient but easy.
            (dolist (my-shebang-pat my-shebang-patterns)
                (if (looking-at my-shebang-pat)
                    (if (= (shell-command
                            (concat "chmod u+x " (buffer-file-name)))
                        0)
                        (message (concat
                                "Wrote and made executable "
                                (buffer-file-name))))))))
        ;; This puts message in *Message* twice, but minibuffer output
        ;; looks better.
        (message (concat "Wrote " (buffer-file-name))))))

;;------------------------------------------------------------------------------
;; custom scripts: add file to changelist on save
;;------------------------------------------------------------------------------
(defun my-current-changelist ()
  (getenv "CHANGELIST"))

(defun my-add-to-changelist (changelist file-name)
  (progn (shell-command (concat "svn add " file-name))
         (shell-command (concat "svn cl " changelist " " file-name))
         (message (concat "Added " file-name " to changelist " changelist))))

(defun my-get-svn-status (file-name)
  (shell-command-to-string (concat "svn st " file-name)))

(add-hook
 'after-save-hook
 (lambda ()
   ;; Add the saved file to the current changelist if defined
   (if (and (my-current-changelist)
            (not (string-match ".*not a working copy$"
                               (my-get-svn-status (buffer-file-name)))))
       (my-add-to-changelist (my-current-changelist) (buffer-file-name)))))

(defun set-changelist (changelist)
  (interactive "sChangelist: \n")
  (setenv "CHANGELIST" changelist)
  (message (concat "Set changelist: " changelist)))

(defun print-current-changelist ()
  (interactive)
  (message (my-current-changelist)))

;;------------------------------------------------------------------------------
;; Custom window title
;;------------------------------------------------------------------------------
(setq frame-title-format
      '((:eval (if (my-current-changelist)
                   (concat (my-current-changelist) " - ")))
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        (:eval (if (buffer-modified-p)
                   " [+]"))
        (:eval (if (evil-insert-state-p)
                   " --INSERT--"))
        (:eval (if (evil-visual-state-p)
                   " --VISUAL--"))
        ))

;;------------------------------------------------------------------------------
;; custom scripts: A command
;;------------------------------------------------------------------------------

(defun A ()
  (interactive)
  (ff-find-other-file nil t))

;; .test<->.expected
(require 'find-file)
(setq-default ff-other-file-alist
              (append '(("\\.expected\\'" (".test"))
                        ("\\.test\\'" (".expected")))
                      cc-other-file-alist))

;;------------------------------------------------------------------------------
;; C++
;;------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;------------------------------------------------------------------------------
;; custom scripts: helm stuff
;;------------------------------------------------------------------------------

(defvar my/helm-execute-history nil)
(defun my/find-executable-files (dir)
  (split-string
   (shell-command-to-string
    (concat "find " dir " -type f -perm +111"))
   "\n" t))
(defun my/helm-execute (program)
  (interactive (list
                (helm-comp-read
                 "Execute: "
                 (my/find-executable-files (projectile-project-root))
                 :must-match t
                 :del-input nil
                 :name "Execute"
                 :history my/helm-execute-history)))
  (compile program))

(defun my/find-gtest-tests (dir)
  "Find all gtest tests in dir"
  (split-string
   (shell-command-to-string
    (concat "find " dir " -type f -perm +111 -name \"*.gtest\""))
   "\n" t))

(defun my/gtest-list-tests (test)
  "Get the tests for using --gtest_filter option"
  (let ((res '("*"))
        (current-suite "")
        (list (split-string
               (shell-command-to-string
                (concat test " --gtest_list_tests"))
               "\n" t)))
    (cl-loop for x in list do
             (if (string-prefix-p " " x)
                 (add-to-list 'res (concat current-suite (string-trim-left x)))
               (setq current-suite x)
               (add-to-list 'res (concat current-suite "*"))))
    (reverse res)))

(defvar my/helm-gtest-history nil)
(defun my/helm-gtest (program)
  (interactive (list
                (helm-comp-read
                 "Execute: "
                 (my/find-gtest-tests (projectile-project-root))
                 :must-match t
                 :del-input nil
                 :name "Execute"
                 :history my/helm-gtest-history)))
  (let* ((selected
          (helm-comp-read
           "Filter: "
           (my/gtest-list-tests program)
           :must-match nil
           :name "Filter"))
         (filter (string-trim (replace-regexp-in-string "#.*" "" selected))))
    (compile (concat program " --gtest_filter="
                     (shell-quote-argument filter)))))

(provide 'my-custom)
