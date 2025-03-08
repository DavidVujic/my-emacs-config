;;; setup-llm.el --- LLM

;;; Commentary:
;;  LLM customizations

;;; Code:
(require 'auth-source)
(require 'gptel)

(defvar setup-llm-system-message
  "You are a helpful assistant in Emacs.
  Respond concisely, to the top of your intelligence.
  Minimize exposition. Avoid caveats or disclaimers.
  Think from first principles, do not parrot common beliefs.
  Cite your sources."
  "Generic LLM system message.")

(defun get-llm-api-key (host)
  "Get the LLM API key for a specific HOST."
  (auth-source-pick-first-password :host host))

(defun setup-gptel-openai ()
  "Setup gptel using OpenAI."
  (interactive)
  (setq gptel-api-key (get-llm-api-key "api.openai.com"))
  (setq gptel-model :gpt-4o)
  (setq gptel--system-message setup-llm-system-message))

(defun setup-gptel-anthropic ()
  "Setup gptel using Anthropic."
  (interactive)
  (setq gptel-api-key (get-llm-api-key "api.anthropic.com"))
  (setq gptel-model :claude-3-sonnet-20240229)
  (setq gptel--system-message setup-llm-system-message)

  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t
          :key gptel-api-key)))


(provide 'setup-llm)

;;; setup-llm.el ends here
