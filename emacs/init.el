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

;; Load the Gruber Darker Theme from Mr. Zozin
(use-package gruber-darker-theme
  :ensure t
  :init
  (setq custom-safe-themes t)
  :config
  (load-theme 'gruber-darker))

(setq-default inhibit-splash-screen t               ;; Don't show the splash screen
              make-backup-files nil                 ;; To stop creating back-ups files
              tab-width 4                           ;; To make tab width as 4 spaces
              indent-tabs-mode nil                  ;; For using spaces instead of tabs
              compilation-scroll-output t           ;; Output in compilation mode scrolls as it appears
              grep-scroll-output t)                 ;; Output in grep mode scrolls as it appears

;; To set set split threshold for horizontal split
(setq window-combination-resize t
      split-width-threshold 300)

;; Set default grep command to be recursive
(setq grep-command "grep --color=auto -rnH --null -e ")
(setq grep-use-null-device nil)

;; For the display column indicator mode
(setq display-fill-column-indicator-column t
      fill-column 80)

;; Turn off some unneeded UI elements
(menu-bar-mode -1)  ; Leave this one on if you're a beginner!
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; To disable the bell
;; (setq visible-bell t) ;; Switch to a visual bell
(setq ring-bell-function 'ignore) ;; Disable the bell without graphical replacement

;; Use short answer yes when quickly reverting buffers
(setq revert-buffer-quick-short-answers t)

;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

;; Display relative line numbers
(setq display-line-numbers-type 'relative)

;; Set custom font
(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font-16")) ; For compatibility with emacsclient
;; (set-frame-font "Iosevka Nerd Font 16" nil t)

;; See number of count when searching
(setq isearch-lazy-count t)

;; Minibuffer packages
(use-package vertico
  :ensure t
  :init
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)   ;; enter directories naturally
              ("DEL" . vertico-directory-delete-char) ;; deletes directory components like ido
              ("C-j" . vertico-directory-enter)
              ("C-l" . vertico-directory-up)))

(use-package vertico-directory
  :after vertico
  :ensure nil   ;; because it’s included with vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  (minibuffer-setup . vertico-repeat-save))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

(use-package consult
  :ensure t
  :config
  (setq consult--read-config
        '((consult-buffer :sort t)))
  :bind (;; Search the current buffer
         ("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer))) ; To overwrite dumb buffer list

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

;; Save minibuffer history (works with Vertico, Consult, etc.)
(use-package savehist
  :ensure t
  :init
  (savehist-mode 1)
  :config
  (setq savehist-additional-variables
        '(command-history
          kill-ring
          register-alist
          mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring)))

;; For the Git gutter
(use-package git-gutter-fringe
  :ensure t
  :config
  ;; optionally tweak the bitmaps to improve appearance
  (global-git-gutter-mode 1)
  (setq git-gutter:update-interval 0.02)
  (define-fringe-bitmap 'git-gutter-fr:added   [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  (set-face-foreground 'git-gutter-fr:modified "darkorange")
  (set-face-foreground 'git-gutter-fr:added    "darkgreen")
  (set-face-foreground 'git-gutter-fr:deleted  "darkred"))

;; To set indetation as 4 spaces for C (Is it really needed?)
(setq-default c-basic-offset 4)
;; c-default-style "linux" ;; It prefers tabs over spaces

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
(use-package typescript-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))

;; Use electric pair mode, to type the closing parenthesis (and others) when you type the first one
(electric-pair-mode t)

;; Multiple cursors mode
(use-package multiple-cursors
  :ensure t
  :bind
  ;; Do What I Mean
  ("C-M-j" . mc/mark-all-dwim) ; both marked and unmarked region. multiple presses.
  ;; For continuous lines: Mark lines, then create cursors. Can be mid-line.
  ("C-M-c" . mc/edit-lines)
  ;; Select region first, then create cursors.
  ("C-M-/" . mc/mark-all-like-this) ; select text first. finds all occurrences.
  ("C-M-," . mc/mark-previous-like-this)
  ("C-M-." . mc/mark-next-like-this)
  ;; Skip this match and move to next one.
  ("C-M-<" . mc/skip-to-previous-like-this)
  ("C-M->" . mc/skip-to-next-like-this))

;; Key-binding to duplicate line
(global-set-key (kbd "C-M-,") 'duplicate-line)

;; To customize the edition on LaTeX documents
(use-package auctex
  :ensure t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq LaTeX-electric-left-right-brace t)
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)
  :hook ((LaTeX-mode . flyspell-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . visual-line-mode)
         (LaTeX-mode . turn-on-reftex)))
;; (use-package auto-complete-auctex :ensure t)
(use-package auctex-latexmk :ensure t)

;; To customize Markdown document
(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . flyspell-mode)
         (markdown-mode . visual-line-mode)))

;; For the company mode
(use-package company-math :ensure t)
(use-package company-box :ensure t)
(use-package company-auctex :ensure t)
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.0))

;; To move Text
(use-package move-text
  :ensure t
  :bind
  ("M-p" . move-text-up)
  ("M-n" . move-text-down))

;; For TypeScript
(use-package typst-ts-mode
  :ensure t
  :config
  (add-to-list 'treesit-language-source-alist
               '(typst "https://github.com/uben0/tree-sitter-typst"))
  (treesit-install-language-grammar 'typst)
  :hook ((typst-ts-mode . flyspell-mode)
         (typst-ts-mode . visual-line-mode)))

;; For the cursor to shine when to move it
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;; To save where I was in files
(save-place-mode t)

;; Major modes
(use-package php-mode :ensure t)
(use-package web-mode :ensure t)
(use-package matlab-mode :ensure t)
(use-package rust-mode :ensure t)
(use-package lua-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package csv-mode :ensure t)
(use-package lsp-mode :ensure t)
(use-package cmake-mode :ensure t)

;; Other packages
(use-package magit :ensure t)
(use-package flycheck :ensure t)
(use-package eglot :ensure t) ; Build in since 30.1
(use-package editorconfig :ensure t)
(use-package jsonrpc :ensure t)

;; To use the PATH from shell (use the .bashrc file)
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (eq system-type 'gnu/linux)
    (exec-path-from-shell-initialize)))

;; To load the org configuration
(load "~/.emacs.d/org.el")

;; Load the tricks to make Magit faster on MS Windows
;; Load Windows32 brower to open files from Emacs
(when (eq system-type 'windows-nt)
  (load "~/.emacs.d/magit-windows.el")
  (load "~/.emacs.d/w32-browser.el")
  (define-key dired-mode-map (kbd "M-RET") #'dired-w32-browser))

;; Set variable for my Windows environnement
(when (eq system-type 'windows-nt)
  (setq python-shell-interpeter "C:\\Users\\azeucianu\\AppData\\Local\\Programs\\Python\\Launcher\\py.exe")
  (setq ispell-dictionary "français")
  (setq delete-by-moving-to-trash t)
  (setq ispell-program-name "C:\\Program Files\\hunspell\\bin\\hunspell.exe")
  (set-language-environment "UTF-8"))

;; To load the custom-file
(load custom-file)
