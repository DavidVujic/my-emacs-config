;;; misc.el --- Misc

;;; Commentary:
;; Miscellaneous customizations

;;; Code:
(use-package emacs
  :init
  ;; Set default background color, to avoid a bright screen shock before loading a Dark Theme is triggered
  (set-background-color "#000000")

  ;; Go straight to scratch buffer on startup
  (setq inhibit-startup-message t)

  ;; No need for ~ files when editing
  (setq create-lockfiles nil)

  ;; "suspend-frame" command, too easy to press it by mistake
  (global-unset-key (kbd "C-z"))

  ;; Changes all yes/no questions to y/n type
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; shell scripts
  (setq-default sh-basic-offset 2)
  (setq-default sh-indentation 2))

(use-package super-save
  :config
  (super-save-mode +1)
  (setq super-save-remote-files nil))

(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.5) ; Open after .5s instead of 1s
  :config
  (which-key-mode))

(provide 'misc)

;;; misc.el ends here
