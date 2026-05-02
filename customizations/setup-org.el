;;; setup-org.el --- setup Org Mode  -*- lexical-binding: t; -*-

;;; Commentary:
;; setup-org customizations.

;;; Code:

;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)

(require 'my-fonts)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)))

;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font my/font :weight 'medium :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font my/font :weight 'bold :height 1.3)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Remove background from all Org blocks
(custom-set-faces
 '(org-block ((t :background nil :foreground nil)))
 '(org-block-begin-line ((t :background nil :foreground nil)))
 '(org-block-end-line ((t :background nil :foreground nil))))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; Configure fill width
(setq visual-fill-column-width 110
      visual-fill-column-center-text t)


(provide 'setup-org)

;;; setup-org.el ends here
