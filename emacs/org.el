;; To install some packages from Melpa
(use-package org-bullets :ensure t)
(use-package htmlize :ensure t)

;; To use special bullets for lists
(add-hook 'org-mode-hook 'org-bullets-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
;; (add-hook 'org-mode-hook 'variable-pitch-mode)

;; Pour masquer les caractères de mise en forme, comme les `*', `/' ou `_'
;; (setq org-hide-emphasis-markers t)

;; To use little bullets for lists
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun set-org-fonts ()
  ;;  Pour paramétrer des polices
  ;; (message-box "On passe par cette fonction !") ;; Pour le débuggage
  (let* ((variable-tuple
          (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (headline           `(:inherit default :weight bold)))
    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil)))))))

(add-hook 'server-after-make-frame-hook 'set-org-fonts)

;; To enable different backend export options
(require 'ox-md)
(require 'ox-beamer)

;; To allow syntax highlighting in HTML
(require 'ox-html)
(setq org-html-htmlize-output-type 'css)

;; To remove validate button at the end of HTML exports
(setq org-html-validation-link nil)

;; Function to include the CSS style into the HTML when exporting from Org-Mode
(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle "~/.emacs.d/org-style.css" path))) ;; <- set your own style file path
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))
(add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)

;; To allow syntax highlighting in LaTeX
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-packages-alist '("" "xcolor"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
