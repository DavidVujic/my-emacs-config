;;; setup-python-rdd.el --- Python

;;; Commentary:
;;  Python REPL Driven Development


;;; Code:

(defun rdd-py/jupyter-shell? ()
  "Return t if the current Python shell is a Jupyter shell, otherwise nil."
  (let ((cmd (car (process-command (get-buffer-process rdd-py/python-buffer-name)))))
    (and cmd (if (string-match "jupyter" cmd) t nil))))


(defun rdd-py/pre-command-hook ()
  "When running with Jupyter: evaluate the current Python namespace."
  (when (eq this-command 'elpy-shell-send-region-or-buffer)
    (when (rdd-py/jupyter-shell?)
      (rdd-py/eval-python-namespace))))

(defun rdd-py/post-command-hook ()
  "Run the REPL Driven Development overlay after the elpy command."
  (when (eq this-command 'elpy-shell-send-region-or-buffer)
    (let ((command this-command))
      (run-at-time
       "0.1 sec" nil
       (lambda (_)
         (rdd-py/output-overlay (rdd-py/get-latest-python-shell-output)))
       command))))

(defun rdd-py/connect-to-existing-jupyter-kernel ()
  "Connect to an existing Jupyter kernel."
  (interactive)
  (rdd-py/connect-jupyter))

(defun rdd-py/use-ipython()
  "Setup the Python shell, using IPython."
  (interactive)
  (rdd-py/setup-ipython))

;;; setup-python-rdd.el ends here
