;;; setup-docker.el --- Docker  -*- lexical-binding: t -*-

;;; Commentary:
;; Docker syntax highlighting and customizations.

;;; Code:
(require 'dockerfile-mode)

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(provide 'setup-docker)

;;; setup-docker.el ends here
