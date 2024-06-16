;;; ui.el --- UI

;;; Commentary:
;; UI customizations

;;; Code:
(require 'treemacs)

(defun setup-theme ()
  "Editor theme."
  (load-theme 'sanityinc-tomorrow-bright)
  (set-face-background 'line-number "#000000"))

(use-package emacs
  :init

  ;; Turn off the menu bar at the top of each frame because it's distracting
  (menu-bar-mode -1)

  ;; Show line numbers
  (global-display-line-numbers-mode 1)

  ;; Show column number in the mode line
  (column-number-mode 1)

  ;; Removes the graphical toolbar at the top.
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

  ;; Don't show native OS scroll bars for buffers because they're redundant
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; Set fonts with fallback, and different sizes depending on system.
  (if (eq system-type 'darwin)
      (cond
       ((member "Fira Mono" (font-family-list))
        (set-face-attribute 'default nil :height 168 :font "Fira Mono"))
       ((member "Menlo" (font-family-list))
        (set-face-attribute 'default nil :height 168 :font "Menlo")))
    (cond
     ((member "Roboto Mono" (font-family-list))
      (set-face-attribute 'default nil :height 128 :font "Roboto Mono"))
     ((member "DejaVu Sans Mono" (font-family-list))
      (set-face-attribute 'default nil :height 138 :font "DejaVu Sans Mono"))))

  ;; These settings relate to how emacs interacts with your operating system
  (setq ;; makes killing/yanking interact with the clipboard
   select-enable-clipboard t

   ;; I'm actually not sure what this does but it's recommended?
   select-enable-primary t

   ;; Save clipboard strings into kill ring before replacing them.
   ;; When one selects something in another program to paste it into Emacs,
   ;; but kills something in Emacs before actually pasting it,
   ;; this selection is gone unless this variable is non-nil
   save-interprogram-paste-before-kill t

   ;; Mouse yank commands yank at point instead of at click.
   mouse-yank-at-point t)

  ;; No cursor blinking, it's distracting
  (blink-cursor-mode 0)

  ;; full path in title bar
  (setq-default frame-title-format "%b (%f)")

  ;; no bell
  (setq ring-bell-function 'ignore)

  (when (eq system-type 'darwin)
    (setq mac-option-modifier nil
          mac-command-modifier 'meta
          select-enable-clipboard t))

  (setq-default cursor-type 'bar)

  ;; Prevent re-centering when going up and down buffer with arrow keys
  (setq scroll-conservatively 101))

;; don't pop up font menu
(global-set-key (kbd "s-t") #'(lambda () (interactive)))

(add-hook 'treemacs-mode-hook
          (lambda ()
            (treemacs-resize-icons 15) ;; smaller icons than default
            (text-scale-adjust -2) ;; smaller font size than default
            (treemacs-toggle-fixed-width) ;; able to resize buffer width
            (treemacs-decrease-width 5)
            (display-line-numbers-mode -1)
            ))

(add-hook 'after-init-hook #'setup-theme)

(provide 'ui)

;;; ui.el ends here
