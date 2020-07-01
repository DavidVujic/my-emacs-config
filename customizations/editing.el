;;; editing.el --- Editing
;;; Commentary:
;; Editor customizations

;;; Code:
(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(show-paren-mode 1)

(global-hl-line-mode 1)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(setq-default indent-tabs-mode nil)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

(defun toggle-comment-on-line-or-region ()
  "Comment or uncomment current line, or region if selected."
  (interactive "*")
  (if (use-region-p)
    (comment-or-uncomment-region (region-beginning) (region-end))
  (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "C-;") 'toggle-comment-on-line-or-region)

(setq electric-indent-mode nil)

;; enable special chars in the editor, like ~ and ^.
(load-library "iso-transl")

;; auto complete
(add-hook 'after-init-hook 'global-company-mode)

;; jump to definition
(dumb-jump-mode)
(global-set-key (kbd "C-M-å") 'dumb-jump-back)

(set-default 'truncate-lines t)
(editorconfig-mode 1)

(global-set-key [(control shift up)]  'move-text-up)
(global-set-key [(control shift down)]  'move-text-down)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Replace highlighted text when you type
(delete-selection-mode 1)

(emojify-set-emoji-styles '(unicode))
(add-hook 'after-init-hook #'global-emojify-mode)

(provide 'editing)

;;; editing.el ends here
