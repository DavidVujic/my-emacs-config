;;; setup-python.el --- Python

;;; Commentary:
;;  python customizations


;;; Code:

(elpy-enable)

(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(defun setup-python-mode ()
  "Python mode setup."
  (require 'pyenv-mode-auto)
  (setq gud-pdb-command-name "python -m pdb")
  (add-to-list 'company-backends 'company-jedi)
  (jedi-mode)
  (pipenv-mode +1)
  (company-mode +1))

(add-hook 'python-mode-hook 'setup-python-mode)


(provide 'setup-python)

;;; setup-python.el ends here
