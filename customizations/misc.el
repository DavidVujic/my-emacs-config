;;; misc.el --- Misc

;;; Commentary:
;; Miscellaneous customizations

;;; Code:

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; "suspend-frame" command, too easy to press it by mistake
(global-unset-key (kbd "C-z"))

(require 'which-key)
(which-key-mode)

(require 'super-save)
(super-save-mode +1)
(setq super-save-remote-files nil)

(provide 'misc)

;;; misc.el ends here
