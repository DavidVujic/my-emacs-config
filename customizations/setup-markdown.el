;;; setup-markdown.el --- Markdown -*- lexical-binding: t; -*-

;;; Commentary:
;; Markdown customizations

;;; Code:

(defun my-markdown-mode-setup ()
  "Make Markdown buffers pleasant to read and write."
  (outline-minor-mode 1)
  (visual-line-mode 1)
  (variable-pitch-mode 1)
  (setq-local line-spacing 0.2)
  (setq-local fill-column 80)
  (display-line-numbers-mode -1))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc --from=gfm --to=html5 --standalone")
  :hook
  (markdown-mode . my-markdown-mode-setup)
  :custom
  (markdown-asymmetric-header t)
  (markdown-header-scaling t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-markup nil)
  (markdown-list-indent-width 2))

(if (member "Fira Sans" (font-family-list))
    (set-face-attribute 'variable-pitch nil
                        :family "Fira Sans"
                        :height 180))

(custom-set-faces
 '(markdown-header-face-1 ((t (:inherit variable-pitch :weight bold :height 1.8 :foreground "#c0c0c0"))))
 '(markdown-header-face-2 ((t (:inherit variable-pitch :weight bold :height 1.5 :foreground "#c0c0c0"))))
 '(markdown-header-face-3 ((t (:inherit variable-pitch :weight bold :height 1.3 :foreground "#c0c0c0"))))
 '(markdown-header-face-4 ((t (:inherit variable-pitch :weight semi-bold :height 1.15 :foreground "#c0c0c0"))))
 '(markdown-header-face-5 ((t (:inherit variable-pitch :weight semi-bold :foreground "#c0c0c0"))))
 '(markdown-header-face-6 ((t (:inherit variable-pitch :weight semi-bold :foreground "#c0c0c0"))))

 '(markdown-code-face ((t (:inherit fixed-pitch))))
 '(markdown-pre-face ((t (:inherit fixed-pitch))))
 '(markdown-inline-code-face ((t (:inherit fixed-pitch))))
 '(markdown-table-face ((t (:inherit fixed-pitch))))

 '(markdown-blockquote-face
   ((t (:inherit variable-pitch :slant italic :foreground "gray60")))))

(defun my-eca-mode-setup ()
  "Reset some of the Markdown customizations to make ECA buffers better looking."
  (visual-line-mode 1)
  (setq-local line-spacing 0.1)
  (display-line-numbers-mode -1)
  (setq-local buffer-face-mode-face 'fixed-pitch)
  (buffer-face-mode 1))

(add-hook 'eca-mode-hook #'my-eca-mode-setup)

(provide 'setup-markdown)

;;; setup-markdown.el ends here
