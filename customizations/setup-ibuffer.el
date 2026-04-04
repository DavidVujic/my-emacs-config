;;; setup-ibuffer.el --- Ibuffer  -*- lexical-binding: t; -*-

;;; Commentary:
;;  ibuffer customizations

;;; Code:
(use-package ibuffer
  :ensure nil
  :config
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-title-face 'font-lock-doc-face)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("Python"
                  (or (mode . python-mode)
                      (mode - python-ts-mode)))
                 ("Config"
                  (or
                   (mode . conf-mode)
                   (mode . conf-toml-mode)
                   (mode . toml-ts-mode)))
                 ("Web"
                  (mode . web-mode))
                 ("JS"
                  (or (mode . js-ts-mode)
                      (mode . js2-mode)
                      (mode . typescript-mode)))
                 ("Markdown"
                  (or (mode . gfm-mode)
                      (mode . markdown-mode)))
                 ("YAML"
                  (mode . yaml-mode))
                 ("Org"
                  (mode . org-mode))
                 ("Eca"
                  (or (mode . eca-chat-mode)
                      (name . "^<eca:.*>")))
                 ("Lisp"
                  (mode . emacs-lisp-mode))))))
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-switch-to-saved-filter-groups "default"))))

(provide 'setup-ibuffer)
;;; setup-ibuffer.el ends here
