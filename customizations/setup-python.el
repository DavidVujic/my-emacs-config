;;; setup-python.el --- Python

;;; Commentary:
;; enable elpy

;;; Code:

(elpy-enable)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(add-hook 'python-mode-hook #'pipenv-mode)

(provide 'setup-python)

;;; setup-python.el ends here
