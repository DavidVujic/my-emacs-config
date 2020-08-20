;;; ui.el --- UI

;;; Commentary:
;; These customizations change the way Emacs looks and disable/enable some user interface elements.
;; Some useful customizations are commented out, and begin with the line "CUSTOMIZE".
;; These are more a matter of preference and may require some fiddling to match your preferences.

;;; Code:

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; Show line numbers
(global-linum-mode)

;; Show column number in the mode line
(column-number-mode 1)

;; You can uncomment this to remove the graphical toolbar at the top. After
;; awhile, you won't need the toolbar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(load-theme 'sanityinc-tomorrow-bright t)
;; override theme background color
(set-background-color "#141414")

;; Set fonts with fallback, and different sizes depending on system
(if (eq system-type 'darwin)
  (cond
    ((member "Fira Mono" (font-family-list))
      (set-face-attribute 'default nil :height 168 :font "Fira Mono"))
    ((member "Menlo" (font-family-list))
      (set-face-attribute 'default nil :height 168 :font "Menlo")))
   (cond
    ((member "Fira Mono" (font-family-list))
      (set-face-attribute 'default nil :height 138 :font "Fira Mono"))
    ((member "DejaVu Sans Mono" (font-family-list))
      (set-face-attribute 'default nil :height 138 :font "DejaVu Sans Mono"))))

;; Start Emacs maximized
(toggle-frame-maximized)
(toggle-frame-fullscreen)

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

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

(when (eq system-type 'darwin)
  (setq mac-option-modifier nil
      mac-command-modifier 'meta
      select-enable-clipboard t))

(setq-default cursor-type 'bar)

;; Prevent re-centering when going up and down buffer with arrow keys
(setq scroll-conservatively 101)

;; neotree settings
(custom-set-faces
 '(neo-root-dir-face ((t (:height 125))))
 '(neo-dir-link-face ((t (:height 125))))
 '(neo-file-link-face ((t (:height 125 :foreground "silver")))))

(setq neo-theme (if (display-graphic-p) 'nerd 'arrow))
(setq neo-window-fixed-size nil)

(provide 'ui)

;;; ui.el ends here
