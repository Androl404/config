(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-indent-level 4)
 '(auth-source-save-behavior nil)
 '(column-number-mode t)
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf"
     "bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af"
     "53a4efdca4c9fb870c3f92e4cfca0fbb638bb29b168a26a363298f9b1d9b9bcf"
     "01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd"
     default))
 '(doc-view-continuous t)
 '(ido-auto-merge-delay-time 0.7)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-cr+-replace-completely t)
 '(ido-enable-dot-prefix t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere nil)
 '(ido-mode 'both nil (ido))
 '(ido-show-dot-for-dired nil)
 '(markdown-command "/usr/bin/pandoc")
 '(package-selected-packages
   '(auctex-latexmk auto-complete-auctex company-auctex company-box
                    company-math doom-themes evil exec-path-from-shell
                    flycheck gruber-darker-theme htmlize
                    ido-completing-read+ lsp-mode lua-mode magit
                    marginalia matlab-mode move-text multiple-cursors
                    org-bullets php-mode rust-mode smex
                    typescript-mode vertico vscode-dark-plus-theme))
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
