;;------------------------------------------------------------------------------
;; custom scripts: remove minor modes from the mode line
;;------------------------------------------------------------------------------
(defun hbin-remove-mm-lighter (mm)
  "Remove minor lighter from the mode line."
  (setcar (cdr (assq mm minor-mode-alist)) nil))

(hbin-remove-mm-lighter 'abbrev-mode)

;;------------------------------------------------------------------------------
;; custom scripts: from <https://github.com/bling/dotemacs/>
;;------------------------------------------------------------------------------
(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

;;------------------------------------------------------------------------------
;; Suppressing ad-handle-definition Warnings
;;------------------------------------------------------------------------------
(setq ad-redefinition-action 'accept)

;;------------------------------------------------------------------------------
;; Return first file found from a list
;;------------------------------------------------------------------------------
(defun my/return-first-file-found (options)
    (let ((option-found nil)
          (i 0)
          (len (length options)))
      (while (and (not option-found) (< i len))
        (if (file-exists-p (elt options i))
            (setq option-found (elt options i)))
        (setq i (1+ i)))
      option-found))

(provide 'my-core)
