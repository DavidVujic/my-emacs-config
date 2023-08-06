;;; setup-js.el --- JavaScript
;;; Commentary:
;; javascript customizations

;;; Code:
(require 'js-comint)
(require 'flycheck)

;; add local node modules to path
(add-hook 'flycheck-mode-hook 'add-node-modules-path)

(add-to-list 'auto-mode-alist '("\\.[c|m]?js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))


(defun setup-tide-mode ()
  "Tide mode setup according to the official guide."
  (interactive)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
)

(defun inferior-js-mode-hook-setup ()
  "Add hook according to the js-comint docs."
  (add-hook 'comint-output-filter-functions 'js-comint-process-output))

(add-hook 'js2-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-hook 'web-mode-hook
          (lambda ()
            (flycheck-add-mode 'typescript-tslint 'web-mode)
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)

(add-hook 'js2-mode-hook
          (lambda ()
            (flycheck-add-mode 'javascript-eslint 'js2-mode)
            (local-set-key (kbd "C-c C-e") 'js-send-last-sexp)
            (local-set-key (kbd "C-c C-b") 'js-send-buffer)
            (local-set-key (kbd "C-c C-c") 'js-clear)
            (local-set-key (kbd "C-c C-r") 'js-send-region)))


(provide 'setup-js)

;;; setup-js.el ends here
