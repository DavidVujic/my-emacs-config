;;; init.el --- initialization file for Emacs

;;; Commentary:
;; Emacs startup file

;;; Code:
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(projectile . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(editorconfig . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(emojify . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(yaml-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(js2-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(tide . "melpa-stable") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(add-node-modules-path
    blacken
    cider
    clojure-mode
    clojure-mode-extra-font-locking
    color-theme-sanityinc-tomorrow
    company
    dockerfile-mode
    dumb-jump
    editorconfig
    elpy
    emojify
    exec-path-from-shell
    flycheck
    flycheck-clj-kondo
    ido-completing-read+
    js2-mode
    json-mode
    magit
    markdown-mode
    move-text
    multiple-cursors
    neotree
    pandoc-mode
    paredit
    projectile
    rainbow-delimiters
    smex
    tagedit
    tide
    web-mode
    yaml-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/customizations")

(load "shell-integration.el")
(load "navigation.el")
(load "ui.el")
(load "editing.el")
(load "misc.el")
(load "elisp-editing.el")
(load "setup-clojure.el")
(load "setup-html.el")
(load "setup-js.el")
(load "setup-python.el")
(load "setup-markdown.el")
(load "setup-yaml.el")
(load "setup-docker.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages (quote (spinner sesman queue parseedn))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-dir-link-face ((t (:height 125))))
 '(neo-file-link-face ((t (:height 125 :foreground "silver"))))
 '(neo-root-dir-face ((t (:height 125)))))

(provide 'init)

;;; init.el ends here
