;;; setup-js.el --- JavaScript and HTML
;;; Commentary:
;; javascript and html customizations

;;; Code:

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'html-mode-hook 'subword-mode)
(setq js-indent-level 2)
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))


(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(require 'flycheck)

;; add local node modules to path
(add-hook 'flycheck-mode-hook 'add-node-modules-path)

;; disable default jslint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

; Enable eslint checker for web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)

(provide 'setup-js)

;;; setup-js.el ends here
