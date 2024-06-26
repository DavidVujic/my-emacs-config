;;; navigation.el --- Navigation


;;; Commentary:
;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
;; When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names.
;; The usual method for making buffer names unique adds ‘<2>’, ‘<3>’, etc to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html

;;; Code:
(require 'uniquify)
(require 'recentf)

(use-package emacs
  :init
  (setq uniquify-buffer-name-style 'forward)

  ;; Turn on recent file mode so that you can more easily switch to
  ;; recently edited files when you first start emacs
  (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode 1)
  (setq recentf-max-menu-items 40)

  :bind (
         ;; Shows a list of buffers
         ("C-x C-b" . 'ibuffer)))

(use-package ido
  ;; ido-mode allows you to more easily navigate choices. For example,
  ;; when you want to switch buffers, ido presents you with a list
  ;; of buffers in the the mini-buffer. As you start to type a buffer's
  ;; name, ido will narrow down the list of buffers to match the text
  ;; you've typed in
  ;; http://www.emacswiki.org/emacs/InteractivelyDoThings

  :init
  (ido-mode t)

  ;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
  (setq ido-enable-flex-matching t)

  ;; Turn this behavior off because it's annoying
  (setq ido-use-filename-at-point nil)

  ;; Don't try to match file across all "work" directories; only match files
  ;; in the current directory displayed in the minibuffer
  (setq ido-auto-merge-work-directories-length -1)

  ;; Includes buffer names of recently open files, even if they're not
  ;; open now
  (setq ido-use-virtual-buffers t)

  ;; This enables ido in all contexts where it could be useful, not just
  ;; for selecting buffer and file names
  (ido-ubiquitous-mode t)
  (ido-everywhere t))

(use-package dumb-jump
  :init
  (dumb-jump-mode)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package smex
  :init
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)

  ;; Enhances M-x to allow easier execution of commands. Provides
  ;; a filterable list of possible commands in the minibuffer
  ;; http://www.emacswiki.org/emacs/Smex
  :bind (("M-x" . 'smex)))

(use-package projectile
  :demand
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(provide 'navigation)

;;; navigation.el ends here
