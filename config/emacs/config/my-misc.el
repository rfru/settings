(setq indent-line-function 'insert-tab)
(setq large-file-warning-threshold 100000000)
(electric-indent-mode 1)

; Save minibuffer history.
(savehist-mode 1)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 0.7)
(auto-save-buffers-enhanced t)

(require 'saveplace)
(setq-default save-place t)

(setq lazy-highlight-initial-delay 0)

; Spell check.
;(add-hook 'text-mode-hook 'flyspell-mode)
;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

; Refresh all buffers periodically.
(setq revert-without-query '(".*"))
(global-auto-revert-mode t)

; Only backup locally
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
; Don't use builtin autosave.
(setq auto-save-default nil)

(require 'tramp)
(setq tramp-verbose 3)
;(setq vc-handled-backends '(Git))
(setq vc-handled-backends nil)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd ".") 'eval-last-sexp)
            (define-key evil-visual-state-local-map (kbd ".")
              '(lambda ()
                 (interactive)
                 (call-interactively 'eval-region)
                 (evil-normal-state)))
            ))


(defun my-py-send (start end)
  "Send the region delimited by START and END to inferior Python process."
  (interactive "r")
  (python-shell-send-string
   (buffer-substring start end)
   ;; No need to send blank lines in ipython? 2013-12-22
   ;; (concat
   ;;  (let ((line-num (line-number-at-pos start)))
   ;;    ;; When sending a region, add blank lines for non sent code so
   ;;    ;; backtraces remain correct.
   ;;    (make-string (1- line-num) ?\n))
   ;;  (buffer-substring start end))
      nil t))
(defun python-eval-region-or-line ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (my-py-send beg end)))
(add-hook 'python-mode-hook
          (lambda ()
            (define-key evil-visual-state-local-map (kbd ".") 'python-eval-region-or-line)
            (define-key evil-normal-state-local-map (kbd ".") 'python-eval-region-or-line)
            ))

(setq vc-follow-symlinks 't)

; Persistent undo!
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo")))

; Tail the compilation buffer.
(setq compilation-scroll-output t)

(provide 'my-misc)
