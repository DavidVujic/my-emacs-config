;;; setup-python.el --- Python  -*- lexical-binding: t; -*-

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

(defvar-local my/python-virtualenv-root nil
  "Buffer-local variable to cache the project root for the current buffer.")

(defun setup-python-virtual-environment ()
  "Setup Python virtual environment."
  (interactive)
  (let ((project-root (auto-virtualenv-locate-project-root)))
    (when (and project-root
               (not (equal my/python-virtualenv-root project-root)))
      (auto-virtualenv-setup)
      (let ((venv (auto-virtualenv-find-local-venv project-root)))
        (when venv
          (pyvenv-activate venv)))
      (my/flycheck-python-customizations)
      (setq my/python-virtualenv-root project-root))))

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
  (add-hook 'python-mode-hook #'elpy-enable)
  :config
  (setq elpy-test-runner 'elpy-test-pytest-runner)
  (setq elpy-formatter 'black)
  (setq elpy-shell-echo-input nil)
  (setq gud-pdb-command-name "python -m pdb")
  ;; Disable elpy's Jedi and flymake to avoid conflicts with eglot
  (setq elpy-modules (delq 'elpy-module-jedi (delq 'elpy-module-flymake elpy-modules)))
  :hook (elpy-mode . setup-elpy-command-hooks))

;; Disable elpy in org-mode buffers
(add-hook 'org-mode-hook
          (lambda ()
            (when (bound-and-true-p elpy-mode)
              (elpy-mode -1))))

(use-package py-isort
  :ensure t)

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

;; Ensure setup-python-virtual-environment runs only once per Python buffer
(add-hook 'python-virtual-env-only-once-hook
          (lambda ()
            (when (and (derived-mode-p 'python-mode)
                       (not (derived-mode-p 'org-mode)))
              (setup-python-virtual-environment))))

(provide 'setup-python)

;;; setup-python.el ends here
