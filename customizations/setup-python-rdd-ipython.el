;;; setup-python-rdd-ipython.el --- Python

;;; Commentary:
;;  Python REPL Driven Development using IPython


;;; Code:
(defun rdd-py/setup-ipython ()
  "IPython shell."
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  (setq python-shell-completion-native-enable nil))


;;; setup-python-rdd-ipython.el ends here
