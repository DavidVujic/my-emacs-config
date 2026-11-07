;;; setup-eca-customizations.el --- elisp  -*- lexical-binding: t -*-
;;; Commentary:
;; Eca customizations, mainly for the Python Agent-REPL-MCP

;;; Code:

(defun my/repl-call? (content)
  "Return non-nil when CONTENT is a tool-call event for agent-repl-mcp__run."
  (and (equal (plist-get content :server) "agent-repl-mcp")
       (equal (plist-get content :name) "run")))

(defun my/tool-call-running? (content)
  "Return non-nil when CONTENT is a tool-call run or running event."
  (member (plist-get content :type) '("toolCallRun" "toolCallRunning")))

(defun my/repl-call-running? (content)
  "Return non-nil when CONTENT is an agent-repl-mcp__run call that is starting."
  (and (my/repl-call? content)
       (my/tool-call-running? content)))

(defun my/repl-code (content)
  "Return the `data' code string of the agent-repl-mcp__run call in CONTENT."
  (plist-get (plist-get content :arguments) :data))

(defun my/repl-show-code (_session content)
  "Render the data argument as Python when agent-repl-mcp__run is in CONTENT."
  (when (my/repl-call-running? content)
    (when-let* ((id   (plist-get content :id))
                (code (my/repl-code content)))
      (eca-chat--update-expandable-content
       id nil   ; nil label: keep the client's label (status icon etc.)
       (eca-chat--content-table
        `(("Tool"      . ,(plist-get content :name))
          ("Server"    . ,(plist-get content :server))
          ("Arguments" . ,(concat "\n```python\n" code "\n```\n"))))
       nil))))  ; nil append: replace, matching the client's own per-event render

(with-eval-after-load 'eca
  (add-hook 'eca-chat-tool-call-functions #'my/repl-show-code))

;;; setup-eca-customizations.el ends here
