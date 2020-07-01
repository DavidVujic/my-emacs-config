;;; setup-yaml.el --- YAML

;;; Commentary:
;; YAML syntax highlighting.

;;; Code:
(require 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(provide 'setup-yaml)

;;; setup-yaml.el ends here
