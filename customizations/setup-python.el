;;; setup-python.el --- Python

;;; Commentary:
;;  python customizations


;;; Code:
(require 'py-isort)

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

(defun setup-pyenv ()
  "Pyenv."
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  (pyenv-mode +1))

(defun format-buffer ()
  "Format the Python buffer using isort and black."
  (interactive)
  (py-isort-buffer)
  (blacken-buffer))

(defun format-buffer-with-ruff ()
  "Format the Python buffer using `ruff`."
  (interactive)
  (save-buffer)
  (shell-command (concat "ruff check " (buffer-file-name) " --fix"))
  (shell-command (concat "ruff format " (buffer-file-name)))
  (revert-buffer t t t))

(defun setup-virtual-environment ()
  "Setup Python virtual environment."
  (auto-virtualenv-setup)
  (pyvenv-activate (auto-virtualenv-find-local-venv (auto-virtualenv-locate-project-root))))

(use-package auto-virtualenv
  :ensure t)

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setup-virtual-environment)
  (setq elpy-test-runner 'elpy-test-pytest-runner)
  (setq elpy-formatter 'black)
  (setq elpy-shell-echo-input nil)
  (setq gud-pdb-command-name "python -m pdb")
  (add-to-list 'company-backends 'company-jedi)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'pre-command-hook #'rdd-py/pre-command-hook)
  (add-hook 'post-command-hook #'rdd-py/post-command-hook))

(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load))

(use-package sideline
  :hook (flycheck-mode . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-flycheck)))

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup))

(use-package python
  :hook ((python-mode . setup-pyenv)
         (python-mode . rdd-py/setup-ipython)
         (python-mode . hs-minor-mode))
  :bind (:map python-mode-map
              ("<f5>" . format-buffer)
              ("<f6>" . format-buffer-with-ruff)))

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
