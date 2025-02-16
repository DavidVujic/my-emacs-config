;;; setup-python-rdd.el --- Python

;;; Commentary:
;;  Python REPL Driven Development


;;; Code:

(defun rdd-py/command-hook ()
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
