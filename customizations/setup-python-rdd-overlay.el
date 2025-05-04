;;; setup-python-rdd-overlay.el --- Python

;;; Commentary:
;;  Python REPL Driven Development customizations for Overlay


;;; Code:

(defvar rdd-py/overlay nil
  "Overlay used to display evaluation results near the cursor.")

(defvar rdd-py/my-last-cursor-pos nil
  "Stores the last known cursor position.")

(defvar rdd-py/overlay-content-max-length 400
  "Max length for content in overlay.")

(defun rdd-py/multiline? (content)
  "Check if CONTENT is multiline or not."
  (string-match-p "\n" content))

(defun rdd-py/handle-possible-multiline-content (result)
  "Apply formatting to the RESULT, handling single and multiline content."
  (if (rdd-py/multiline? result)
      (concat "\n" result "\n")
    result))

(defun rdd-py/overlay-styling (result)
  "The styling of the RESULT."
  (let ((content (rdd-py/handle-possible-multiline-content result) ))
    (propertize (format " => %s" content)
                'line-prefix "    "
                'face 'default)))

(defun rdd-py/truncate-content (result)
  "Truncate RESULT if longer than a configured limit."
  (if (> (length result) rdd-py/overlay-content-max-length)
      (concat (substring result 0 (- rdd-py/overlay-content-max-length 3)) "...")
    result))

(defun rdd-py/remove-existing-overlay ()
  "Remove any existing overlay."
  (when (overlayp rdd-py/overlay)
    (delete-overlay rdd-py/overlay)
    (setq rdd-py/overlay nil)))


(defun rdd-py/check-cursor-movement ()
  "Check if the cursor has moved, and remove the overlay if it has."
  (when
    (not (equal (point) rdd-py/my-last-cursor-pos))
    (rdd-py/remove-existing-overlay)
    (remove-hook 'post-command-hook #'rdd-py/check-cursor-movement)))

(defun rdd-py/format-content-with-mode (content)
  "Return CONTENT formatted as \"python-mode\"."
  (with-temp-buffer
    (insert content)
    (funcall 'python-mode)
    (setq font-lock-mode t)
    (font-lock-fontify-region (point-min) (point-max))
    (buffer-substring (point-min) (point-max))))

(defun rdd-py/format-output (output)
  "Apply formatting and truncating to OUTPUT."
  (let* ((styled-output (rdd-py/overlay-styling output))
         (python-styled-output (rdd-py/format-content-with-mode styled-output))
         (truncated-output (rdd-py/truncate-content python-styled-output)))
    truncated-output))

(defun rdd-py/output-overlay (output &optional skip-handle-cursor)
   "Show overlay for evaluated OUTPUT."
  (rdd-py/remove-existing-overlay)
  (let ((line-end (save-excursion
                    (move-end-of-line 1)
                    (skip-chars-backward " \t")
                    (point))))
    (setq rdd-py/overlay (make-overlay line-end line-end))
    (overlay-put rdd-py/overlay 'after-string (rdd-py/format-output output)))
  (setq rdd-py/my-last-cursor-pos (point))
  (when (not skip-handle-cursor)
    (add-hook 'post-command-hook #'rdd-py/check-cursor-movement)))

;;; setup-python-rdd-overlay.el ends here
