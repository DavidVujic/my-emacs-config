;;; setup-python-rdd-llm.el --- Python

;;; Commentary:
;;  REPL Driven Development customizations using LLM


;;; Code:

(defvar rdd-py/llm-prompt-generate-function-parameter-stubs
  "Generate one example value for each input parameter, based on the parameter name and type.
  The example value should be representative of the type and have meaningful content
  depending on the parameter name.

  Don't respond with anything else than the actual Python code.
  Don't wrap the code within markdown.
  Don't generate code comments.
  Don't add text that explains what you have done.
  It should be only Python code, nothing else.
  IMPORTANT: the format should be \"parameter = the generated value\".
  All generated code has to be code that can be evaluated in a REPL."
  "Instruction for an LLM, to generate example values for function parameters.")


(defun rdd-py/llm-insert-parameter-stubs (response _info)
  (rdd-py/output-overlay response t)
  (when (yes-or-no-p "Evaluate? ")
    (with-current-buffer rdd-py/python-buffer-name
      (goto-char (point-max))
      (insert response)
      (comint-send-input)))
  (rdd-py/remove-existing-overlay))


(defun rdd-py/llm-generate-parameter-stubs ()
  "Ask the LLM to generate stubs (example values) for the input parameters."
  (interactive)
  (let* ((code (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (format "%s\n\n%s" rdd-py/llm-prompt-generate-function-parameter-stubs code)))
    (gptel-request prompt
      :callback #'rdd-py/llm-insert-parameter-stubs)))


;;; setup-python-rdd-llm.el ends here
