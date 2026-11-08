;;; setup-eca-customizations.el --- elisp  -*- lexical-binding: t -*-
;;; Commentary:
;; Eca customizations, mainly for the Python Agent-REPL-MCP

;;; Code:

(defun my/repl-call? (content)
  "Return non-nil when CONTENT is a tool-call event for agent-python-repl-mcp__run."
  (and (equal (plist-get content :server) "agent-python-repl-mcp")
       (equal (plist-get content :name) "run")))

(defun my/tool-call-running? (content)
  "Return non-nil when CONTENT is any tool-call lifecycle event.
Including \"toolCalled\"/\"toolCallRejected\" so the python view is not
overwritten by the built-in render when the call finishes."
  (member (plist-get content :type)
          '("toolCallRun" "toolCallRunning" "toolCalled" "toolCallRejected")))

(defun my/repl-call-running? (content)
  "Return non-nil when CONTENT is an agent-python-repl-mcp__run lifecycle event."
  (and (my/repl-call? content)
       (my/tool-call-running? content)))

(defun my/repl-code (content)
  "Return the `data' code string of the agent-repl-mcp__run call in CONTENT."
  (plist-get (plist-get content :arguments) :data))

(defun my/repl-output (content)
  "Return the joined output text of a toolCalled CONTENT, or nil."
  (when-let* ((outputs (plist-get content :outputs)))
    (let ((text (mapconcat (lambda (o) (or (plist-get o :text) "")) outputs "\n")))
      (unless (string= text "") text))))

(defun my/repl-show-code (_session content)
  "Render the data argument as Python for agent-python-repl-mcp__run.
On \"toolCalled\" also keep the tool output, in a fenced block."
  (when (my/repl-call-running? content)
    (when-let* ((id   (plist-get content :id))
                (code (my/repl-code content)))
      (let ((rows `(("Tool"      . ,(plist-get content :name))
                    ("Server"    . ,(plist-get content :server))
                    ("Arguments" . ,(concat "\n```python\n" code "\n```\n")))))
        (when-let* ((out (my/repl-output content)))
          (setq rows (append rows
                             `(("Output" . ,(concat "\n```json\n" out "\n```\n"))))))
        (eca-chat--update-expandable-content
         id nil   ; nil label: keep the client's label (status icon etc.)
         (eca-chat--content-table rows)
         nil)))))  ; nil append: replace, matching the client's own per-event render

(with-eval-after-load 'eca
  (add-hook 'eca-chat-tool-call-functions #'my/repl-show-code))

;;; setup-eca-customizations.el ends here
