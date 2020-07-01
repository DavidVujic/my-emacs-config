;;; shell-integration.el --- shell


;;; Commentary:
;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
 

;;; Code:

;; find the current node version
(setq exec-path (append exec-path '("~/.nvm/versions/node/v12.14.1/bin")))
(exec-path-from-shell-initialize)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-copy-envs
   '("PATH")))

(provide 'shell-integration)

;;; shell-integration.el ends here
