;;; setup-python.el --- Python

;;; Commentary:
;;  python customizations


;;; Code:

(elpy-enable)

(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(defun setup-python-mode ()
  (require 'pyenv-mode-auto)
  (setq gud-pdb-command-name "python -m pdb")
  (setq jedi:complete-on-dot t)
  (setq jedi:get-in-function-call-delay 0)
  (add-to-list 'company-backends 'company-jedi)
  (jedi-mode)
  (pipenv-mode +1))

(add-hook 'python-mode-hook 'setup-python-mode)

(provide 'setup-python)

;;; setup-python.el ends heres
