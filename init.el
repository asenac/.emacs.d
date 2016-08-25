
(add-to-list 'load-path (concat user-emacs-directory "config"))

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
(require 'my-rtags)
(require 'my-pkgs)
(require 'my-eclim)
(require 'my-themes)
(require 'my-custom)

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
