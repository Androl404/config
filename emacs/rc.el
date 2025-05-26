;; Credit to Mr A-Who, Mr Zozin : `https://github.com/rexim/dotfiles'
(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once ()
  (when (not rc/package-contents-refreshed)
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-one-package (package)
  (when (not (package-installed-p package))
    (rc/package-refresh-contents-once)
    (package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (rc/require-one-package package)))

(defun rc/require-theme (theme)
  (let ((theme-package (->> theme
                            (symbol-name)
                            (funcall (-flip #'concat) "-theme")
                            (intern))))
    (rc/require theme-package)
    (load-theme theme t)))

;; Splits between horizontally and vertically splited windows
(defun rc/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; (global-set-key (kbd "C-x |") 'toggle-window-split)

;; Enable whitespace-mode with special configuration (custom.el)
(defun rc/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

;; To clear the kill-ring
(defun rc/clear-kill-ring ()
  (interactive)
  (setq kill-ring nil))

;; To run a command on all marked files in dired
(defun rc/dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

;; Variable to save the marking of s-expressions backward
(defvar rc/mark-backward-sexp-anchor nil
  "Anchor point for repeated backward sexp marking.")

;; Function to mark s-expressions backward
(defun rc/mark-backward-sexp ()
  "Incrementally mark s-expressions backward without moving point forward.
If no more s-expressions can be marked, keep the current region and show a message."
  (interactive)
  (let ((inhibit-quit t)
        (original-point (point))
        (original-mark (mark)))
    (condition-case err
        (if (not (region-active-p))
            ;; First invocation: set anchor and mark
            (progn
              (setq rc/mark-backward-sexp-anchor (point))
              (backward-sexp)
              (set-mark (point))
              (goto-char rc/mark-backward-sexp-anchor))
          ;; Subsequent invocations: extend region backward
          (let ((current-point (point)))
            (goto-char (mark))
            (backward-sexp)
            (set-mark (point))
            (goto-char current-point)))
      (scan-error
       ;; Restore original region and show message
       (set-mark original-mark)
       (goto-char original-point)
       (message "No more s-expressions to mark backward.")))
    (setq deactivate-mark nil)))

;; Hook to reset the variable storing the position of the mark
(add-hook 'deactivate-mark-hook
          (lambda () (setq rc/mark-backward-sexp-anchor nil)))

(define-key global-map (kbd "C-M-S-SPC") #'rc/mark-backward-sexp)
;; (global-set-key (kbd "C-M-!") 'rc/mark-backward-sexp)
