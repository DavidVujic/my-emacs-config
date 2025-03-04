;;; setup-python-rdd-llm.el --- Python

;;; Commentary:
;;  REPL Driven Development customizations using LLM


;;; Code:

(defvar rdd-py/llm-instruction-generate-parameter-stubs
  "Analyze the following Python function and identify its input parameters and their types.

  Then, generate example values for these parameters based on their names and types.
  The example values should be representative of their type and have meaningful content
  depending on the parameter name. The values will be used to quickly test the function
  in a Python REPL.

  Don't generate anything else than the actual code.
  Don't wrap the code within markdown.
  It has to be code that can be evaluated in a REPL."
  "Instruction for an LLM, to generate example values for function parameters.")


(defun rdd-py/llm-insert-parameter-stubs (response _info)
  (with-current-buffer "*Python*"
    (goto-char (point-max))
    (insert response)
    (comint-send-input)))


(defun rdd-py/llm-generate-parameter-stubs ()
  "Ask the LLM to generate stubs (example values) for the input parameters."
  (interactive)
  (let* ((code (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (format "%s\n\n%s" rdd-py/llm-instruction-generate-parameter-stubs code)))
    (gptel-request prompt
      :callback #'rdd-py/llm-insert-parameter-stubs)))


;;; setup-python-rdd-llm.el ends here
