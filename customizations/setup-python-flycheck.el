;;; setup-python-flycheck.el --- Python

;;; Commentary:
;;  Flycheck customizations for Python development


;;; Code:

(defun in-path? (path exe-path)
  "Return non-nil if the executable EXE-PATH is in PATH."
  (and (string-match-p path (file-truename exe-path)) t))

(defun local? (exe)
  "Return non-nil if the executable EXE is a local .venv install."
  (and (in-path? (auto-virtualenv-locate-project-root) exe) t))

(defun find-exe (name)
  "Return non-nil if executable NAME exists in current `exec-path`."
  (executable-find name))

(defun has-exe? (name)
  "Return non-nil if executable NAME exist and isn't a global install."
  (and (when-let ((p (find-exe name)))
         (local? p))
       t))

(defun has-mypy? ()
  "Check if mypy exists in the current environment."
   (has-exe? "mypy"))

(defun has-flake8? ()
    "Check if flake8 exists in the current environment."
    (has-exe? "flake8"))

(defun has-ruff? ()
    "Check if ruff exists in the current environment."
    (has-exe? "ruff"))

(defun has-ty? ()
    "Check if ty exists in the current environment."
    (has-exe? "ty"))

(defun has-flycheck-ty? ()
  "Non-nil if ty is registered in Flycheck."
  (and (boundp 'flycheck-checkers)
       (memq 'python-ty flycheck-checkers)))

(defun my/flycheck-register-python-ty ()
  "Define python-ty checker once."
  (when (and (has-ty?) (not (has-flycheck-ty?)))
    (flycheck-define-checker python-ty
      "Python type checker using ty."
      :command ("ty" "check" "--output-format" "concise"
                (eval (or buffer-file-name default-directory)))
      :error-patterns
      ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
      :modes (python-mode python-ts-mode))

    (add-to-list 'flycheck-checkers 'python-ty 'append)))

(defun my/flycheck-set-python-chain (linter)
  "Set LINTER as primary checker and chain to ty or mypy."
  (setq-local flycheck-checker linter)

  (when (fboundp 'flycheck-remove-next-checker)
    (flycheck-remove-next-checker linter 'python-ty)
    (flycheck-remove-next-checker linter 'python-mypy))

  (cond
   ((has-ty?)
    (flycheck-add-next-checker linter 'python-ty))
   ((has-mypy?)
    (flycheck-add-next-checker linter 'python-mypy))))

(defun my/flycheck-python-customizations ()
  "Pick ruff/flake8 and chain to ty or mypy."
  (my/flycheck-register-python-ty)

  (cond
   ((has-flake8?)
    (my/flycheck-set-python-chain 'python-flake8))
   ((has-ruff?)
    (my/flycheck-set-python-chain 'python-ruff))
   (t nil)))

(use-package flycheck
  :hook ((python-mode . my/flycheck-python-customizations)
         (python-ts-mode . my/flycheck-python-customizations)))


;;; setup-python-flycheck.el ends here
