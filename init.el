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
(add-to-list 'package-pinned-packages '(editorconfig . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(js2-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(projectile . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(super-save . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(yaml-mode . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(tide . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(auto-virtualenv . "melpa-stable") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(add-node-modules-path
    auto-virtualenv
    blacken
    cider
    clj-refactor
    clojure-mode
    clojure-mode-extra-font-locking
    color-theme-sanityinc-tomorrow
    company
    company-jedi
    company-quickhelp
    dockerfile-mode
    dumb-jump
    editorconfig
    elpy
    emojify
    exec-path-from-shell
    flycheck
    flycheck-clj-kondo
    flymake-ruff
    gptel
    graphql-mode
    ido-completing-read+
    py-isort
    js2-mode
    js-comint
    json-mode
    just-mode
    live-py-mode
    magit
    markdown-mode
    move-text
    multiple-cursors
    pandoc-mode
    paredit
    polymode
    projectile
    pyenv-mode
    prettier
    rainbow-delimiters
    sideline
    sideline-flycheck
    smex
    super-save
    tagedit
    terraform-mode
    tide
    typescript-mode
    treemacs
    web-mode
    which-key
    yaml-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/customizations")

(load "misc.el")
(load "ui.el")
(load "shell-integration.el")
(load "navigation.el")
(load "editing.el")
(load "elisp-editing.el")
(load "setup-python-rdd-shell.el")
(load "setup-python-rdd-overlay.el")
<<<<<<< HEAD
(load "setup-python-rdd-llm.el")
=======
(load "setup-python-rdd-jupyter.el")
(load "setup-python-rdd.el")
>>>>>>> 6761b63 (wip: rdd python)
(load "setup-python.el")
(load "setup-clojure.el")
(load "setup-html.el")
(load "setup-js.el")
(load "setup-markdown.el")
(load "setup-yaml.el")
(load "setup-docker.el")
(load "setup-llm.el")


(provide 'init)

;;; init.el ends here
