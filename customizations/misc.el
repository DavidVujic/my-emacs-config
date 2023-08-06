;;; misc.el --- Misc

;;; Commentary:
;; Miscellaneous customizations

;;; Code:
(require 'which-key)
(require 'super-save)

;; Set default background color, to avoid a bright screen shock before loading a Dark Theme is triggered
(set-background-color "#000000")

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(defun misc-setup ()
  "Misc setup."
  (which-key-mode)
  (super-save-mode +1)
  (setq super-save-remote-files nil)

  ;; No need for ~ files when editing
  (setq create-lockfiles nil)

  ;; "suspend-frame" command, too easy to press it by mistake
  (global-unset-key (kbd "C-z"))

  ;; Changes all yes/no questions to y/n type
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; shell scripts
  (setq-default sh-basic-offset 2)
  (setq-default sh-indentation 2)
)

(add-hook 'after-init-hook #'misc-setup)

(provide 'misc)

;;; misc.el ends here
