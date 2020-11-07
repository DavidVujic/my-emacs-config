;;; setup-python.el --- Python

;;; Commentary:
;;  python customizations


;;; Code:

(require 'pyenv-mode-auto)
(elpy-enable)

(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(add-hook 'python-mode-hook #'pipenv-mode)

(provide 'setup-python)

;;; setup-python.el ends heres
