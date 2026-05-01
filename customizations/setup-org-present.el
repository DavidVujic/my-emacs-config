;;; setup-org-present.el --- org-present  -*- lexical-binding: t; -*-

;;; Commentary:
;; org-present customizations.

;;; Code:

;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)

(defun my/org-present-start ()
  "Center the presentation and wrap lines."
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (display-line-numbers-mode -1)

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Tweak font sizes and ensure font family is set
  (setq-local face-remapping-alist `((default (:family ,(face-attribute 'default :family) :height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:family ,(face-attribute 'org-code :family) :height 1.55) org-code)
                                     (org-verbatim (:family ,(face-attribute 'org-verbatim :family) :height 1.55) org-verbatim)
                                     (org-block (:family ,(face-attribute 'org-block :family) :height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block))))

(defun my/org-present-end ()
  "Stop centering the document."
  (visual-fill-column-mode 0)
  (visual-line-mode 0)
  (display-line-numbers-mode 1)

  ;; Clear the header line format by setting to `nil'
  (setq header-line-format nil)
  ;; Reset font customizations for all relevant faces
  (setq-local face-remapping-alist nil))

(defun my/org-present-prepare-slide (_buffer-name _heading)
  "Show only top-level HEADING in the current BUFFER-NAME."
  (org-overview)

  ;; Unfold the current entry
  (org-fold-show-entry)

  (org-fold-show-all))

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)
(add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

(provide 'setup-org-present)

;;; setup-org-present.el ends here
