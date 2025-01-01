;;; setup-python-rdd-shell.el --- Python

;;; Commentary:
;;  REPL Driven Development customizations for Overlay


;;; Code:

(defvar rdd-py/python-buffer-name "*Python*"
  "Name of the Python buffer.")

(defvar rdd-py/python-out-regex "^Out\\[[0-9]+\\]: \\(.*\\)"
  "Regular expression to match the 'Out[n]:' prompt and its inline output.")

(defvar rdd-py/python-in-regex "^\\s-*In \\[\\s-*\\([0-9]+\\)\\]:"
  "Regular expression to match the 'In[n]:' prompt with optional whitespace.")

(defvar rdd-py/non-empty-line "\\S-"
  "Non-empty line.")

(defun rdd-py/find-python-buffer ()
  "Find and return an active Python buffer, or nil if none is found."
  (let ((buffer (get-buffer rdd-py/python-buffer-name)))
    (if (and buffer (buffer-live-p buffer))
        buffer
      nil)))

(defun rdd-py/capture-position-and-output ()
  "Capture inline output on the same line.
Return position and inline output."
  (cons (point) (match-string 1) ))

(defun rdd-py/shell-contains-new-output ()
  "Make sure the shell has new input.
This is done by verifying there are not two consecutive 'In' at the end."
  (save-excursion
    (when (re-search-backward rdd-py/python-in-regex nil t)
      (let ((latest-in (point)))
        (when (re-search-backward rdd-py/python-in-regex nil t)
          (let ((previous-in (point)))
            (goto-char previous-in)
            (re-search-forward rdd-py/python-out-regex latest-in t)))))))


(defun rdd-py/find-out-prompt ()
  "Find the latest 'Out[n]:' in the buffer and return its start position.
Captures inline output on the same line, return nil if not found."
  (if (rdd-py/shell-contains-new-output)
    (if (re-search-backward rdd-py/python-out-regex nil t)
        (rdd-py/capture-position-and-output)
      nil)))

(defun rdd-py/add-output-if-exists (inline-output)
  "Add output if INLINE-OUTPUT exists."
  (if (string-match-p rdd-py/non-empty-line inline-output)
      (list inline-output)
    '()))

(defun rdd-py/non-empty-and-not-in-regex (line)
  "Include non-empty LINE that do not match the in-regex prompt."
  (and (string-match-p rdd-py/non-empty-line line)
       (not (string-match-p rdd-py/python-in-regex line))))

(defun rdd-py/collect-output-lines (start end inline-output)
  "Collect and return all output lines between START and END.
INLINE-OUTPUT is included if present."
  (goto-char start)
  (let ((lines (rdd-py/add-output-if-exists inline-output)))
    (while (< (point) end)
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (when (rdd-py/non-empty-and-not-in-regex line)
          (push line lines)))
      (forward-line))
    (reverse lines))) ; Return lines in correct order


(defun rdd-py/calculate-output-end ()
  "Ensure output-end stops at the line containing the in-regex, if found."
  (or (when (re-search-forward rdd-py/python-in-regex nil t)
        (line-beginning-position)) ; Stop at the start of the in-regex.
      (point-max)))

(defun rdd-py/extract-shell-output (out-prompt)
  "Extract the output starting from OUT-PROMPT."
  (let* ((output-start (progn (goto-char (car out-prompt)) (forward-line) (point)))
         (inline-output (cdr out-prompt))
          (output-end (rdd-py/calculate-output-end))
         (lines (rdd-py/collect-output-lines output-start output-end inline-output)))
    (let ((output (string-join lines "\n")))
      (if (string-match-p rdd-py/non-empty-line output)
          output
        nil))))

(defun rdd-py/go-to-end-of-buffer ()
  "Go to end of the buffer."
  (goto-char (point-max)))

(defun rdd-py/ensure-plain-text (output)
  "Ensure plain text is returned from OUTPUT."
  (substring-no-properties output))

(defun rdd-py/get-latest-python-shell-output ()
  "Return the latest Python shell output."
  (let ((python-buffer (rdd-py/find-python-buffer)))
    (when python-buffer
      (with-current-buffer python-buffer
        (save-excursion
          (rdd-py/go-to-end-of-buffer)
          (let ((out-prompt (rdd-py/find-out-prompt)))
            (when out-prompt
              (rdd-py/ensure-plain-text (rdd-py/extract-shell-output out-prompt)))))))))

;;; setup-python-rdd-shell.el ends here
