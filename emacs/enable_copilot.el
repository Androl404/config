;; First clone the Git repository https://github.com/copilot-emacs/copilot.el to your local machine.
;; Then evaluate copilot_balanced.el and copilot.el in Emacs to load the package.
;; Then evaluate the following lines.

;; Load the filecopilot_balanced.el
(load-file "/media/andrei/Data/dev/third_repo/copilot.el/copilot-balancer.el")
(load-file "/media/andrei/Data/dev/third_repo/copilot.el/copilot")

(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; Do M-x copilot-diagnose to check if everything is working correctly.
;; Then enable copilot mode with M-x copilot-mode.
