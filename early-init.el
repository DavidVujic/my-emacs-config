;;; early-init.el --- before initialization

;;; Commentary:
;; UI customizations

;;; Code:


;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; Don't show native OS scroll bars for buffers because they're redundant
(scroll-bar-mode -1)

;; Removes the graphical toolbar at the top.
(tool-bar-mode -1)

;; We up the gc threshold to temporarily prevent it from running, then
;; reset it later after startup is complete. Not resetting it will
;; cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.5)


;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar prot-emacs--file-name-handler-alist file-name-handler-alist)
(defvar prot-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8))
            (setq gc-cons-percentage 0.1)
            (setq file-name-handler-alist prot-emacs--file-name-handler-alist)
            (setq vc-handled-backends prot-emacs--vc-handled-backends)))

;;; early-init.el ends here
