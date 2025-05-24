;; To get the Gruber-Darker theme for Emacs 24+, actually to set a package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Set custom file for Emacs auto configuration
(setq custom-file "~/.emacs.d/custom.el")

(setq user-full-name "Andrei Zeucianu"
      user-mail-address "andrei.zeucianu@gmail.com")

;; Load multiple functions
(load "~/.emacs.d/rc.el")
(rc/require 'gruber-darker-theme)

(setq-default inhibit-splash-screen t               ;; Don't show the splash screen
    make-backup-files nil                           ;; To stop creating back-ups files
    tab-width 4                                     ;; To make tab width as 4 spaces
    indent-tabs-mode nil                            ;; For using spaces instead of tabs
    compilation-scroll-output t)                    ;; Output in compilation mode scrolls as it appears

;; To set set split threshold for horizontal split
(setq window-combination-resize t
      split-width-threshold 300)

;; Turn off some unneeded UI elements
(menu-bar-mode -1)  ; Leave this one on if you're a beginner!
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

;; Display relative line numbers
(setq display-line-numbers-type 'relative) 

;; Set custom font
(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font-17")) ; For compatibility with emacsclient
; (set-frame-font "Iosevka Nerd Font 18" nil t)

;; To set up the 'ido-completing-read+ package
(rc/require 'smex 'ido-completing-read+)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(setq ido-use-filename-at-point 'guess)

;; To set up smex, in order to have a custom M-x menu, which the most use commands first
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; To set indetation as 4 spaces for C (Is it really needed?)
(setq-default
    ;; c-default-style "linux" ;; It prefers tabs over spaces
    c-basic-offset 4)

;; Use `//' comments instead if `/*' comments in c-mode
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

;; Use whitespace mode with whitespace handling on certain mode
(add-hook 'csharp-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'c++-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'c-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'simpc-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'rust-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'LaTeX-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'python-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'php-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'go-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook 'rc/set-up-whitespace-handling)

;; dired
(require 'dired-x)
(setq dired-listing-switches "-alh --group-directories-first")
;; For dired if I have multiple dired windows to move/copy file with default path of the other buffer
(setq dired-dwim-target t)

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
;; Skip this match and move to next one.
(global-set-key (kbd "C-M-<") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)

;; Key-binding to duplicate line
(global-set-key (kbd "C-M-,") 'duplicate-line)

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

;; To customize Markdown document
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)

;; For the company mode
(rc/require 'company-math 'company-box)
(require 'company)
(global-company-mode)

;; To move Text
(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; Remplace useless list directory binding
(global-set-key (kbd "C-x C-d") 'ido-dired)

(rc/require 
    'php-mode 
    'typescript-mode 
    'markdown-mode 
    'magit 
    'rust-mode 
    'auto-complete-auctex 
    'flycheck 
    'eglot 
    'lsp-mode 
    'lua-mode 
    'company-auctex 
    'auctex-latexmk
    'evil
    'editorconfig
    'jsonrpc
    'matlab-mode
    'exec-path-from-shell
)

;; To use the PATH from shell (use the .bashrc file)
(exec-path-from-shell-initialize)

;; To load the org configuration
(load "~/.emacs.d/org.el")

;; To load the custom-file
(load custom-file)
