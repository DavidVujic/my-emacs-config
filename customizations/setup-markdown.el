;;; setup-markdown.el --- Markdown

;;; Commentary:
;; Markdown customizations

;;; Code:

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(setq markdown-command
      "pandoc -f markdown -t html -s --mathjax --highlight-style=pygments")
      
(provide 'setup-markdown)

;;; setup-markdown.el ends here
