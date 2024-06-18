;;; setup-markdown.el --- Markdown

;;; Commentary:
;; Markdown customizations

;;; Code:

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc -f gfm -t html5 -s"))

      
(provide 'setup-markdown)

;;; setup-markdown.el ends here
