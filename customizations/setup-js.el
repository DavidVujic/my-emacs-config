;;; setup-js.el --- JavaScript
;;; Commentary:
;; javascript customizations

;;; Code:
(require 'flycheck)

;; add local node modules to path
(add-hook 'flycheck-mode-hook 'add-node-modules-path)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))

(defun setup-tide-mode ()
  "Tide mode setup according to the official guide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(add-hook 'js2-mode-hook #'setup-tide-mode)

(flycheck-add-mode 'javascript-eslint 'js2-mode)

(provide 'setup-js)

;;; setup-js.el ends here
