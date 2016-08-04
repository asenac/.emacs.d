;;------------------------------------------------------------------------------
;; el-get
;;------------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; Packages I want installed and updated
(setq my-packages
      (append '(
                yasnippet-snippets
                el-get
                )
              ;; Plus everything that is include in el-get-sources
              (mapcar 'el-get-source-name el-get-sources)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-recipes")

;; Sync my packages!
(el-get 'sync my-packages)

(provide 'my-el-get)
