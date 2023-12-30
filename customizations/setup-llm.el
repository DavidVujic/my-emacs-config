;;; setup-llm.el --- LLM

;;; Commentary:
;;  LLM customizations

;;; Code:
(require 'auth-source)
(require 'gptel)

(defun setup-gptel ()
  "Setup gptel."
  (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com"))
  (setq gptel-model "gpt-4-1106-preview")
  (setq gptel--system-message
        "You are a helpful assistant in Emacs.
       Respond concisely, to the top of your intelligence.
       Minimize exposition. Avoid caveats or disclaimers.
       Think from first principles, do not parrot common beliefs.
       Cite your sources."))

(add-hook 'gptel-mode-hook #'setup-gptel)

(provide 'setup-llm)

;;; setup-llm.el ends here
