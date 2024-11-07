;; To get the Gruber-Darker theme for Emacs 24+, actually to set a package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Load multiple functions
(load "~/.emacs.d/rc.el")
(rc/require 'gruber-darker-theme)

;; Don't show the splash screen
(setq inhibit-startup-message t)  ; Comment at end of line!

;; Turn off some unneeded UI elements
(menu-bar-mode -1)  ; Leave this one on if you're a beginner!
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

;; Display relative line numbers
(setq display-line-numbers-type 'relative) 

;; Set custom font
(set-frame-font "Iosevka Nerd Font 16" nil t)

;; To stop creating back-ups files
(setq make-backup-files nil) ; stop creating ~ files

;; To set up the 'ido-completing-read+ package
(rc/require 'smex 'ido-completing-read+)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(setq ido-use-filename-at-point 'guess)

;; To set up smex, in order to have a custom M-x menu, which the most use commands first
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; To set indetation as 4 spaces
(setq c-default-style "linux"
      c-basic-offset 4)

;; dired
(require 'dired-x)
(setq dired-listing-switches "-alh --group-directories-first")

;; For LSP (Is It Really Needed?)
(rc/require 'lsp-mode)
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

;; To enable and use the 'evil-mode'
; (require 'evil)
; (evil-mode 1)

;; To enable the Typescript mode
(rc/require 'typescript-mode)
(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; Use electric pair mode, to type the closing parenthesis (and others) when you type the first one
(electric-pair-mode t)

;; Multiple cursors mode
(rc/require 'multiple-cursors)
;; Do What I Mean
(global-set-key (kbd "C-M-j") 'mc/mark-all-dwim) ; both marked and unmarked region. multiple presses.
;; For continuous lines: Mark lines, then create cursors. Can be mid-line.
(global-set-key (kbd "C-M-c") 'mc/edit-lines)
;; Select region first, then create cursors.
(global-set-key (kbd "C-M-/") 'mc/mark-all-like-this) ; select text first. finds all occurrences.
(global-set-key (kbd "C-M-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-.") 'mc/mark-next-like-this)
;; Skip this match and move to next one. (Note YouTube won't allow angle brackets here.)
(global-set-key (kbd "C-M-<") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)

;; Key-binding to duplicate line
(global-set-key (kbd "C-,") 'duplicate-line)

;; Use `//' comments instead if `/*' comments in c-mode
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

;; For dired if I have multiple dired windows to move/copy file with default path of the other buffer
(setq dired-dwim-target t)

;; To customize the edition on LaTeX documents
(rc/require 'auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'flysppell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'pabbrev-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; For the company mode
(rc/require 'company-math 'company-box)
(require 'company)
(global-company-mode)
(add-hook 'tuareg-mode-hook
          (lambda ()
            (interactive)
            (company-mode 0)))

(rc/require 'treemacs 'php-mode 'typescript-mode 'magit 'rust-mode 'auto-complete-auctex 'flycheck 'eglot 'lua-mode)

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
   '("01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd" "e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" "e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7" default))
 '(doc-view-continuous t)
 '(evil-want-C-u-scroll t)
 '(ido-auto-merge-delay-time 0.7)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-cr+-replace-completely t)
 '(ido-enable-dot-prefix t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere nil)
 '(ido-mode 'both nil (ido))
 '(ido-show-dot-for-dired nil)
 '(package-selected-packages
   '(lua-mode ac-php treemacs lsp-ltex company-math pabbrev auto-complete-auctex lsp-latex auctex smex rust-mode multiple-cursors magit typescript-mode undo-fu undo-tree evil company-box projectile ## phpt-mode flycheck lsp-mode php-mode gruber-darker-theme ido-completing-read+))
 '(php-imenu-generic-expression 'php-imenu-generic-expression-simple)
 '(php-mode-coding-style 'psr2)
 '(php-mode-template-compatibility nil)
 '(warning-suppress-log-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
