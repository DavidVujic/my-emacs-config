;;; Setup-python-rdd-jupyter.el --- Python

;;; Commentary:
;;  Python REPL Driven Development using Jupyter and connecting to a running kernel


;;; Code:

(defvar rdd-py/src-folder nil
  "Top folder name, i.e. top namespace, for Python code.")

(defun rdd-py/possible-src-folders ()
  "A list of possible Python src top folders."
  (let ((folders '("src" "components" "bases")))
    (if rdd-py/src-folder (cons rdd-py/src-folder folders) folders)))

(defun rdd-py/find-project-root ()
  "Find the root of the current Python project."
  (auto-virtualenv-locate-project-root))

(defun rdd-py/selected-region ()
  "Return the text of the currently selected region as-is."
  (when (use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun rdd-py/get-absolute-file-path ()
  "Return the absolute path of the current buffer's file."
  (let ((file (buffer-file-name)))
    (expand-file-name file)))

(defun rdd-py/namespace-package? (file-path)
  "Check if the current FILE-PATH is a Python namespace package."
  (let* ((parent (file-name-parent-directory file-path))
         (possible-ns-package-file (concat parent "__init__.py")))
    (file-exists-p possible-ns-package-file)))

(defun rdd-py/extract-relevant-path (base path)
  "Extract the relative path from PATH by removing the BASE part."
  (let ((regexp (concat "^" (regexp-quote base))))
    (if (string-match regexp path)
        (substring path (match-end 0))
      path)))

(defun rdd-py/convert-path-to-python-namespace (path)
  "Convert a file PATH into a valid Python namespace."
  (let* ((without-file-ext (file-name-sans-extension path))
        (with-allowed-chars (replace-regexp-in-string "-" "_" without-file-ext))
        (separated (remove "" (split-string with-allowed-chars "/"))))
    (mapconcat 'identity separated ".")))


(defun rdd-py/remove-possible-src-folder (path folders)
  "Remove the first match of FOLDERS from the beginning of PATH."
  (let* ((pattern (concat "^" (mapconcat #'regexp-quote folders "\\|"))))
    (replace-regexp-in-string pattern "" path)))

(defun rdd-py/python-namespace-from-buffer (file-path)
  "Generate a Python namespace from the current buffer's absolute FILE-PATH."
  (let* ((project-root (rdd-py/find-project-root))
         (relative-path (rdd-py/extract-relevant-path project-root file-path))
         (possible-src-folders (rdd-py/possible-src-folders))
         (without-src-folder (rdd-py/remove-possible-src-folder relative-path possible-src-folders)))
    (rdd-py/convert-path-to-python-namespace without-src-folder)))


(defun rdd-py/strip-parens-from-selected-region (s)
  "Remove parentheses and everything between them from string S."
  (replace-regexp-in-string "(.*)" "" s))

(defun rdd-py/reload-package (namespace)
  "Reload the current namespace package (i.e. parent) for the NAMESPACE."
  (let* ((parts (split-string namespace "\\."))
        (without-module (butlast parts))
        (namespace-package (mapconcat 'identity without-module ".")))
    (python-shell-send-string "import importlib")
    (python-shell-send-string (concat "importlib.reload(" namespace-package ")"))))

(defun rdd-py/import-python-namespace (namespace)
  "Import the NAMESPACE by sending Python code to the REPL."
  (let ((top-namespace (car (split-string namespace "\\.")))
        (selected-text (rdd-py/strip-parens-from-selected-region (rdd-py/selected-region)))
        (selected-module (car (split-string selected-text "\\."))))
    (python-shell-send-string-no-output (concat "import " top-namespace))
    (python-shell-send-string-no-output (concat "from " namespace " import " selected-module))))

(defun rdd-py/eval-python-namespace ()
  "Evaluate a calculated namespace, based on the current buffer, to a REPL session."
  (let* ((file-path (rdd-py/get-absolute-file-path))
         (namespace (rdd-py/python-namespace-from-buffer file-path)))
    (rdd-py/import-python-namespace namespace)
    (when (rdd-py/namespace-package? file-path) (rdd-py/reload-package namespace))))

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
