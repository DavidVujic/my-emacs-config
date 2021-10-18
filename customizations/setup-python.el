;;; setup-python.el --- Python

;;; Commentary:
;;  python customizations


;;; Code:

(setq elpy-rpc-python-command "python3")

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(setq elpy-shell-echo-input nil)
(setenv "WORKON_HOME" "~/.pyenv/versions")

(elpy-enable)

(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Polymode: syntax highlighting for inline SQL statements in Python
(define-hostmode poly-python-hostmode :mode 'python-mode)

(define-innermode poly-sql-expr-python-innermode
    :mode 'sql-mode
    :head-matcher (rx (= 3 (char "\"'")) (zero-or-more (any "\t\n ")) (or "SELECT" "INSERT" "UPDATE" "DELETE"))
    :tail-matcher (rx (= 3 (char "\"'")))
    :head-mode 'host
    :tail-mode 'host)

(define-polymode poly-python-sql-mode
    :hostmode 'poly-python-hostmode
    :innermodes '(poly-sql-expr-python-innermode))

(defun setup-python-mode ()
  "Python mode setup."
  (require 'pyenv-mode-auto)
  (require 'py-isort)
  (setq gud-pdb-command-name "python -m pdb")
  (add-to-list 'company-backends 'company-jedi)
  (pipenv-mode +1)
  (pyenv-mode +1)
  (company-mode +1)
  (blacken-mode +1))

(add-hook 'python-mode-hook #'setup-python-mode)


(provide 'setup-python)

;;; setup-python.el ends here
