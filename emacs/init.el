;; To get the Gruber-Darker theme for Emacs 24+, actually to set a package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Set custom file for Emacs auto configuration
(setq custom-file "~/.emacs.d/custom.el")

;; Load multiple functions
(load "~/.emacs.d/rc.el")
(rc/require 'gruber-darker-theme)

(setq-default inhibit-splash-screen t               ;; Don't show the splash screen
    make-backup-files nil                           ;; To stop creating back-ups files
    tab-width 4                                     ;; To make tab width as 4 spaces
    indent-tabs-mode nil                            ;; Tabs will indent the line instead of adding tabs
    compilation-scroll-output t)                    ;; Output in compilation mode scrolls as it appears

;; Turn off some unneeded UI elements
(menu-bar-mode -1)  ; Leave this one on if you're a beginner!
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

;; Display relative line numbers
(setq display-line-numbers-type 'relative) 

;; Set custom font
(set-frame-font "Iosevka Nerd Font 18" nil t)

;; To set up the 'ido-completing-read+ package
(rc/require 'smex 'ido-completing-read+)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(setq ido-use-filename-at-point 'guess)

;; To set up smex, in order to have a custom M-x menu, which the most use commands first
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; To set indetation as 4 spaces for C (Is it really needed?)
(setq c-default-style "linux"
      c-basic-offset 4)

;; dired
(require 'dired-x)
(setq dired-listing-switches "-alh --group-directories-first")
;; For dired if I have multiple dired windows to move/copy file with default path of the other buffer
(setq dired-dwim-target t)

;; To enable and use the 'evil-mode'
; (rc/require 'evil)
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

;; To customize the edition on LaTeX documents
(rc/require 'auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq LaTeX-electric-left-right-brace t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
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

;; To move Text
(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(rc/require 
    'treemacs 
    'php-mode 
    'typescript-mode 
    'magit 
    'rust-mode 
    'auto-complete-auctex 
    'flycheck 
    'eglot 
    'lua-mode 
    'company-auctex 
    'auctex-latexmk
)

;; To laod the custom-file
(load custom-file)
(put 'upcase-region 'disabled nil)
