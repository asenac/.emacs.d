
(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path "~/.emacs.d/static/cc-mode")

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; (require 'my-el-get)
(require 'my-core)
(require 'my-basic)
(require 'my-evil)
(require 'my-helm)
(require 'my-themes)
(require 'my-pkgs)
(require 'my-coding)
(require 'my-custom)
(require 'my-bindings)
(require 'my-csharp)
(require 'my-go)
(require 'my-cpp)
(require 'my-rust)

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

(if (not (require 'my-work nil t))
    (message "It seems you are not at work... good for you"))

(use-package diminish)
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
