;;; setup-python-rdd.el --- Python

;;; Commentary:
;;  Python REPL Driven Development


;;; Code:

(defun rdd-py/jupyter-kernel-shell? ()
  "Return t if the current Python shell is a Jupyter shell, otherwise nil."
  (let ((current-python-repl (get-buffer-process rdd-py/python-buffer-name)))
    (when current-python-repl
      (let ((cmd (process-command current-python-repl)))
        (if (and cmd (member "jupyter" cmd) (member "--existing" cmd)) t nil)))))

(defun rdd-py/prepare-jupyter-console ()
  "Prepare the Jupyter console by importing the namespace and module."
  (interactive)
  (rdd-py/eval-python-namespace))

(defun rdd-py/pre-command ()
  "If running Jupyter with external kernel: evaluate the current Python ns."
  (when (eq this-command 'elpy-shell-send-region-or-buffer)
    (when (rdd-py/jupyter-kernel-shell?)
      (rdd-py/prepare-jupyter-console))))

(defun rdd-py/post-command ()
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
