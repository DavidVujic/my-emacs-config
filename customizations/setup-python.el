;;; setup-python.el --- Python

;;; Commentary:
;;  python customizations


;;; Code:
(require 'elpy)
(require 'py-isort)
(require 'auto-virtualenv)

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
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  (setq python-shell-completion-native-enable nil)

  (setq elpy-shell-echo-input nil)
  (setenv "WORKON_HOME" "~/.pyenv/versions")

  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq gud-pdb-command-name "python -m pdb")
  (setq elpy-test-runner 'elpy-test-pytest-runner)
  (setq elpy-formatter 'black)

  (add-to-list 'company-backends 'company-jedi)

  (elpy-enable)
  (pyenv-mode +1)
  (auto-virtualenv-set-virtualenv)
)

(add-hook 'python-mode-hook #'setup-python-mode)

;; Python shell buffer
(setq display-buffer-alist
      '(
        ((derived-mode . inferior-python-mode)
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (dedicated . t)
         (window-height . fit-window-to-buffer)
         )
        )
      )

(provide 'setup-python)

;;; setup-python.el ends here
