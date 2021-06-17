(require 'sql)

;; mostly stuff copied from https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client

(defun my-sql-save-history-hook ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
        (let ((filename
               (concat "~/.emacs.d/sql/"
                       (symbol-name (symbol-value rval))
                       "-history.sql")))
          (set (make-local-variable lval) filename))
      (error
       (format "SQL history will not be saved because %s is nil"
               (symbol-name rval))))))

(add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)

(setq sql-connection-alist
      '((materialize (sql-product 'postgres)
                     (sql-port 6875)
                     (sql-server "localhost")
                     (sql-user "materialize")
                     (sql-database "materialize"))
        (pg (sql-product 'postgres)
            (sql-port 5432)
            (sql-server "localhost")
            (sql-user "asenac")
            (sql-database "asenac"))))

(defun my-sql-materialize ()
  (interactive)
  (my-sql-connect 'postgres 'materialize))

(defun my-sql-pg ()
  (interactive)
  (my-sql-connect 'postgres 'pg))

(defun my-sql-connect (product connection)
  ;; remember to set the sql-product, otherwise, it will fail for the first time
  ;; you call the function
  (setq sql-product product)
  (sql-connect connection))

(defvar my-sql-servers-list
  '(("Materialize" my-sql-materialize)
    ("Postgres" my-sql-pg))
  "Alist of server name and the function to connect")

(defun my-sql-connect-server (func)
  "Connect to the input server using my-sql-servers-list"
  (interactive
   (helm-comp-read "Select server: " my-sql-servers-list))
  (funcall func))

(after 'evil-leader
  (evil-leader/set-key
    "q" 'my-sql-connect-server))

(provide 'my-materialize)
