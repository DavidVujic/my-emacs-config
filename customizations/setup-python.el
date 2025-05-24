;;; setup-python.el --- Python

;;; Commentary:
;;  python customizations


;;; Code:


(defun setup-pyenv ()
  "Pyenv."
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  (pyenv-mode +1))

(defun format-python-buffer ()
  "Format the Python buffer using isort and black."
  (interactive)
  (py-isort-buffer)
  (blacken-buffer))

(defun format-python-buffer-with-ruff ()
  "Format the Python buffer using `ruff`."
  (interactive)
  (save-buffer)
  (shell-command (concat "ruff check " (buffer-file-name) " --fix"))
  (shell-command (concat "ruff format " (buffer-file-name)))
  (revert-buffer t t t))

(defun setup-python-virtual-environment ()
  "Setup Python virtual environment."
  (interactive)
  (auto-virtualenv-setup)
  (pyvenv-activate (auto-virtualenv-find-local-venv (auto-virtualenv-locate-project-root))))

(defun setup-elpy-command-hooks ()
  "Setup the rdd-py specific command hooks in elpy mode."
  (add-hook 'pre-command-hook #'rdd-py/pre-command nil t)
  (add-hook 'post-command-hook #'rdd-py/post-command nil t))

(use-package auto-virtualenv
  :ensure t)

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setup-python-virtual-environment)
  (setq elpy-test-runner 'elpy-test-pytest-runner)
  (setq elpy-formatter 'black)
  (setq elpy-shell-echo-input nil)
  (setq gud-pdb-command-name "python -m pdb")
  (add-to-list 'company-backends 'company-jedi)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  :hook (elpy-mode . setup-elpy-command-hooks))

(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load))

(use-package py-isort
  :ensure t
  :hook (python-mode . py-isort))

(use-package python
  :hook ((python-mode . setup-pyenv)
         (python-mode . rdd-py/setup-ipython)
         (python-mode . hs-minor-mode))
  :bind (:map python-mode-map
              ("<f5>" . format-python-buffer)
              ("<f6>" . format-python-buffer-with-ruff)))

;; Python shell buffer
(setq display-buffer-alist
      '(((derived-mode . inferior-python-mode)
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (dedicated . t)
         (window-height . fit-window-to-buffer))))

(provide 'setup-python)

;;; setup-python.el ends here
