;;; Setup-python-rdd-jupyter.el --- Python

;;; Commentary:
;;  Python REPL Driven Development using Jupyter and connecting to a running kernel


;;; Code:


(defvar rdd-py/top-folder nil
  "Top folder name, i.e. top namespace, for Python code.")

(defun rdd-py/find-top-namespace ()
  "Find the top namespace of the current Python project."
  (let ((root (auto-virtualenv-locate-project-root))
        (folder (or rdd-py/top-folder "")))
    (if (string= folder "")
        root
        (concat root folder))))

(defun rdd-py/get-absolute-file-path ()
  "Return the absolute path of the current buffer's file."
  (let ((file (buffer-file-name)))
    (expand-file-name file)))

(defun rdd-py/extract-relevant-path (base path)
  "Extract the relative path from PATH by removing the BASE part."
  (let ((regexp (concat "^" (regexp-quote base))))
    (if (string-match regexp path)
        (substring path (match-end 0))
      path)))

(defun rdd-py/convert-path-to-python-namespace (path)
  "Convert a file PATH into a valid Python namespace."
  (let* ((namespace (file-name-sans-extension path))
         (namespace (replace-regexp-in-string "^[/]+" "" namespace))
         (namespace (replace-regexp-in-string "[^a-zA-Z0-9_/]" "_" namespace))
         (namespace (replace-regexp-in-string "/" "." namespace)))
    namespace))

(defun rdd-py/selected-region ()
  "Return the text of the currently selected region as-is."
  (when (use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))


(defun rdd-py/send-to-python-repl (code)
  "Send the given Python CODE directly to the *Python* REPL."
  (let ((python-buffer (get-buffer rdd-py/python-buffer-name)))
    (with-current-buffer python-buffer
      (goto-char (point-max))
      (insert code)
      (comint-send-input))))

(defun rdd-py/python-namespace-from-buffer ()
  "Generate a Python namespace from the current buffer's absolute file path."
  (let* ((file-path (rdd-py/get-absolute-file-path))
         (top-namespace (rdd-py/find-top-namespace))
         (relative-path (rdd-py/extract-relevant-path top-namespace file-path))
         (selected-text (rdd-py/selected-region)))
    (let ((namespace (rdd-py/convert-path-to-python-namespace relative-path)))
      (if selected-text
          (concat namespace "." selected-text)
        namespace))))

(defun rdd-py/eval-python-with-full-namespace ()
  "Evaluate selected region, with a calculated namespace, to an open *Python* REPL session."
  (interactive)
  (rdd-py/send-to-python-repl (rdd-py/python-namespace-from-buffer)))

(defun rdd-py/ask-for-kernel-file ()
  "Ask for a running Jupyter kernel."
  (read-string "Jupyter kernel name: "))

(defun rdd-py/construct-jupyter-interpreter-args (kernel-file)
  "Construct the Jupyter interpreter args with a KERNEL-FILE."
  (format "console --simple-prompt --existing %s" kernel-file))

(defun rdd-py/set-jupyter-as-python-shell (kernel-file)
  "Set Jupyter as the Python shell with the KERNEL-FILE as an interpreter arg."
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args (rdd-py/construct-jupyter-interpreter-args kernel-file)
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))

(defun rdd-py/connect-jupyter ()
  "Prompt for a Jupyter kernel file name and set up the Python shell."
  (let ((kernel-file (rdd-py/ask-for-kernel-file)))
    (rdd-py/set-jupyter-as-python-shell kernel-file)
    (run-python (python-shell-calculate-command) nil t)))


;;; setup-python-rdd-jupyter.el ends here
