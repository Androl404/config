(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-indent-level 4)
 '(auth-source-save-behavior nil)
 '(column-number-mode t)
 '(doc-view-continuous t)
 '(markdown-command "/usr/bin/pandoc")
 '(package-selected-packages
   '(auctex-latexmk auto-complete-auctex catppuccin-theme company-auctex
                    company-box company-math doom-themes evil
                    exec-path-from-shell fixmee flycheck
                    gruber-darker-theme htmlize ido-completing-read+
                    lsp-mode lua-mode magit marginalia matlab-mode
                    move-text multiple-cursors org-bullets php-mode
                    rust-mode smex typescript-mode typst-ts-mode
                    vscode-dark-plus-theme))
 '(php-imenu-generic-expression 'php-imenu-generic-expression-simple)
 '(php-mode-coding-style 'psr2)
 '(php-mode-template-compatibility nil)
 '(warning-suppress-log-types '((comp) (comp)))
 '(whitespace-style
   '(face tabs spaces trailing space-before-tab newline indentation empty
          space-after-tab space-mark tab-mark)))


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :font "Source Sans Pro" :height 2.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :font "Source Sans Pro" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :font "Source Sans Pro" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :font "Source Sans Pro" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :font "Source Sans Pro" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :font "Source Sans Pro"))))
 '(org-level-6 ((t (:inherit default :weight bold :font "Source Sans Pro"))))
 '(org-level-7 ((t (:inherit default :weight bold :font "Source Sans Pro"))))
 '(org-level-8 ((t (:inherit default :weight bold :font "Source Sans Pro")))))
