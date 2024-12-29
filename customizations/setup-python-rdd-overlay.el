;;; setup-python-rdd-overlay.el --- Python

;;; Commentary:
;;  REPL Driven Development customizations for Overlay


;;; Code:

(defvar rdd-py/overlay nil
  "Overlay used to display evaluation results near the cursor.")

(defvar rdd-py/my-last-cursor-pos nil
  "Stores the last known cursor position.")

(defvar rdd-py/overlay-content-max-length 400
  "Max length for content in overlay.")

(defun rdd-py/overlay-face ()
  "The styling of the contents in an overlay."
  '(:foreground "green"
               :background "grey10"
               :box (:line-width -1 :color "black" :alpha 0.5)
               :weight normal))

(defun rdd-py/overlay-styling (result)
  "The styling of the RESULT."
  (propertize (format " => %s" result) 'face (rdd-py/overlay-face)))

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
    (not (equal (point) rdd-py/my-last-cursor-pos)) ; Check if cursor moved
    (rdd-py/remove-existing-overlay)
    (remove-hook 'post-command-hook #'rdd-py/check-cursor-movement)))

(defun rdd-py/output-overlay (output)
   "Show overlay for evaluated OUTPUT."
  (rdd-py/remove-existing-overlay)
  ;; Calculate the position after the last statement
  (let ((line-end (save-excursion
                    (move-end-of-line 1)
                    (skip-chars-backward " \t") ; Ignore trailing spaces/tabs
                    (point))))
    ;; Create a new overlay at the calculated position
    (setq rdd-py/overlay (make-overlay line-end line-end))
    (overlay-put rdd-py/overlay 'after-string
                 (rdd-py/overlay-styling (rdd-py/truncate-content output))))
  ;; Record the current cursor position
  (setq rdd-py/my-last-cursor-pos (point))
  ;; Add a hook to check for cursor movement
  (add-hook 'post-command-hook #'rdd-py/check-cursor-movement))

;;; setup-python-rdd-overlay.el ends here
