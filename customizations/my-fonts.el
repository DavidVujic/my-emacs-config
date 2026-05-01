;;; my-fonts.el --- fonts  -*- lexical-binding: t; -*-

;;; Commentary:
;; Use fonts depending on OS.

;;; Code:

(defvar my/font
  (cond
   ((eq system-type 'darwin)
    (cond
     ((member "Fira Mono" (font-family-list)) '"Fira Mono")
     ((member "Menlo" (font-family-list)) '"Menlo")
     (t '"Monaco")))
   (t
    (cond
     ((member "Roboto Mono" (font-family-list)) '"Roboto Mono")
     ((member "DejaVu Sans Mono" (font-family-list)) '"DejaVu Sans Mono")
     (t '"Ubuntu Mono"))))
  "The font family based on the OS.")

(provide 'my-fonts)

;;; my-fonts.el ends here
