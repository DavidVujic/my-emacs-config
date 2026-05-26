;;; elisp-editing.el --- elisp editing

;;; Commentary:
;; elisp editing customizations

;;; Code:

;; Automatically load paredit when editing a Lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)

(defun my/enable-paredit-in-eval-expression ()
  "Enable paredit in the `eval-expression' minibuffer.
Paredit normally rebinds both Return and line-feed to `paredit-newline',
which makes \\[eval-expression] impossible to submit.  Override that
locally via `minor-mode-overriding-map-alist' so \\[exit-minibuffer]
submits the expression for evaluation and \\[paredit-newline] inserts a
literal newline for multi-line input."
  (enable-paredit-mode)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")   #'exit-minibuffer)
    (define-key map (kbd "C-j")   #'paredit-newline)
    (setq-local minor-mode-overriding-map-alist
                (cons (cons 'paredit-mode map)
                      minor-mode-overriding-map-alist))))

(add-hook 'eval-expression-minibuffer-setup-hook
          #'my/enable-paredit-in-eval-expression)

(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(provide 'elisp-editing)

;;; elisp-editing.el ends here
