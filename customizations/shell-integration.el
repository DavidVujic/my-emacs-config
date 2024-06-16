;;; shell-integration.el --- shell


;;; Commentary:
;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell

;;; Code:
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)

  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-envs
     '("PATH"))))


(provide 'shell-integration)

;;; shell-integration.el ends here
