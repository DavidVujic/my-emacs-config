;;; early-init.el --- before initialization

;;; Commentary:
;; UI customizations

;;; Code:

;; We up the gc threshold to temporarily prevent it from running, then
;; reset it later after startup is complete. Not resetting it will
;; cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.5)

;; Disable Bidirectional Text Scanning
;; If you don’t edit right-to-left languages (Arabic, Hebrew, etc.),
;; Emacs is doing a bunch of work on every redisplay cycle for nothing.
;; These settings tell Emacs to assume left-to-right text everywhere and
;; skip the bidirectional parenthesis algorithm.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Skip Fontification During Input
;; Emacs normally fontifies (syntax-highlights) text even while you’re actively typing.
;;This can cause micro-stutters, especially in tree-sitter modes or large buffers.
(setq redisplay-skip-fontification-on-input t)

;; Increase Process Output Buffer for LSP
;; The default read-process-output-max is 64KB, which is still quite conservative.
;; Modern LSP servers like rust-analyzer or clangd routinely send multi-megabyte responses.
;; Bumping this reduces the number of read calls Emacs has to make:
(setq read-process-output-max (* 4 1024 1024)) ; 4MB


;; Don’t Render Cursors in Non-Focused Windows
;; If you have several windows visible,
;; Emacs draws a cursor in each of them – even the ones you’re not working in.
;; It also highlights selections in non-focused windows.
;; This is mostly a visual preference, but it also reduces rendering work.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)


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
