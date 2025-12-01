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

;; If text is marked (mark activated) and search is launched, use the marked text as search string.
(defun rc/isearch-forward-from-region ()
  "If region is active, use its content as the initial search string."
  (interactive)
  (let ((search-text (when (use-region-p)
                       (buffer-substring-no-properties (region-beginning) (region-end)))))
    (deactivate-mark)
    (isearch-mode t nil nil nil)
    (when search-text
      (isearch-yank-string search-text))))

(defun rc/isearch-backward-from-region ()
  "If region is active, use its content as the initial reverse search string."
  (interactive)
  (let ((search-text (when (use-region-p)
                       (buffer-substring-no-properties (region-beginning) (region-end)))))
    (deactivate-mark)
    (isearch-mode nil nil nil nil)
    (when search-text
      (isearch-yank-string search-text))))

(global-set-key (kbd "C-s") #'rc/isearch-forward-from-region)
(global-set-key (kbd "C-r") #'rc/isearch-backward-from-region)

;; To show and copy (kill) the absolute path of the current buffer.
(defun rc/show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (buffer-file-name)))

(global-set-key [C-f1] 'rc/show-file-name)

;; To switch between frames when all frames have only one window
(defun rc/other-window-or-frame ()
  "Switch to next frame if all frames have only one window; otherwise switch window."
  (interactive)
  (if (cl-every (lambda (f) (= (length (window-list f)) 1))
                (frame-list))
      (other-frame 1)
    (other-window 1)))

(global-set-key (kbd "C-x o") #'rc/other-window-or-frame)

;; Execute exactly what I type when pressing C-j
(defun rc/vertico-accept-input ()
  "Accept minibuffer input exactly as typed, ignoring the current candidate."
  (interactive)
  (let ((input (minibuffer-contents)))
    (delete-minibuffer-contents)
    (insert input)
    (exit-minibuffer)))
