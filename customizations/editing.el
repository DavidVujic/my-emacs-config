;;; editing.el --- Editing
;;; Commentary:
;; Editor customizations

;;; Code:
(require 'saveplace)

(defun toggle-comment-on-line-or-region ()
  "Comment or uncomment current line, or region if selected."
  (interactive "*")
  (if (use-region-p)
    (comment-or-uncomment-region (region-beginning) (region-end))
  (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun setup-editing ()
  "Setup editing."
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))

  (show-paren-mode 1)

  (global-hl-line-mode 1)
  (setq company-tooltip-align-annotations t)

  ;; auto complete with docstrings
  (company-quickhelp-mode)
  (set-default 'truncate-lines t)
  (editorconfig-mode 1)
  ;; Replace highlighted text when you type
  (delete-selection-mode 1))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package emacs
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places"))
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups"))))
  (setq auto-save-default nil)
  (setq electric-indent-mode nil)

  ;; enable special chars in the editor, like ~ and ^.
  (load-library "iso-transl")
  (emojify-set-emoji-styles '(unicode))

  :bind (("C-;" . 'toggle-comment-on-line-or-region)
         ("M-/" . 'hippie-expand)

         ("C-s" . 'isearch-forward-regexp)
         ("C-r" . 'isearch-backward-regexp)
         ("C-M-s" . 'isearch-forward)
         ("C-M-r" . 'isearch-backward)

         ([(control shift up)] . 'move-text-up)
         ([(control shift down)] . 'move-text-down)))

;; syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :hook (add-node-modules-path))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

;; auto complete
(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'after-init-hook 'setup-editing)

(add-hook 'after-init-hook #'global-emojify-mode)

(provide 'editing)

;;; editing.el ends here
