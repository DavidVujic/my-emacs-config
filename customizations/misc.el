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

(use-package magit
  :ensure t)

;; Save the Clipboard Before Killing
;; Here’s a scenario: you copy a URL from your browser, switch to Emacs, kill a line with C-k,
;; and then try to yank the URL you copied earlier with C-y. Gone.
;; The kill replaced it on the clipboard.
;; This setting makes Emacs save the existing clipboard content into the kill ring before
;; overwriting it. Now C-y gets the kill, and M-y gets you back to the URL.
;; Such a small thing, but it eliminates a genuinely annoying problem.
(setq save-interprogram-paste-before-kill t)

;; No Duplicates in the Kill Ring
;; Kill the same line three times and you get three identical entries in the kill ring, wasting slots.
;; This deduplicates them.
(setq kill-do-not-save-duplicates t)

(provide 'misc)

;;; misc.el ends here
