(setq indent-line-function 'insert-tab)
(setq large-file-warning-threshold 100000000)
(electric-indent-mode 1)

(setq debug-on-quit nil)

(defun my-paste () 
  (interactive)
  (if (string-match "^\*terminal" (buffer-name))
      (term-paste)
      (yank)))
(define-key evil-normal-state-map (kbd "s-v") 'my-paste)
(define-key evil-insert-state-map (kbd "s-v") 'my-paste)

(defun my-yes-or-mumble-p (prompt)
    "PROMPT user with a yes-or-no question, but only test for yes."
    (if (string= "y"
                 (downcase
                  (read-from-minibuffer
                   (concat prompt "(y or n) "))))
        t
      nil))
(defalias 'yes-or-no-p 'my-yes-or-mumble-p)

; Save minibuffer history and other variables.
(setq savehist-additional-variables 
  '(compile-history) 
  savehist-file "~/.emacs.d/savehist") 
savehist-file
(savehist-mode 1)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 0.7)
(setq auto-save-buffers-enhanced-quiet-save-p nil)
(auto-save-buffers-enhanced t)

(require 'saveplace)
(setq-default save-place t)

(setq lazy-highlight-initial-delay 0)

; Refresh all buffers periodically.
(require 'autorevert)
(setq revert-without-query '(".*"))
(setq auto-revert-use-notify nil)
(global-auto-revert-mode t)

; Only backup locally
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
; Don't use builtin autosave.
(setq auto-save-default nil)

(require 'tramp)
(setq tramp-verbose 3)
;; (setq tramp-verbose 6)
; Tramp might be more stable if we don't use the same connection as terminal SSH.
(setq tramp-ssh-controlmaster-options
                 (concat
                   "-o ControlPath=/tmp/ssh-%%r@%%h:%%p "
                   "-o ControlMaster=auto -o ControlPersist=yes"))
(setq vc-handled-backends nil)
(setq tramp-remote-path '(tramp-own-remote-path))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd ".") 'eval-last-sexp)
            (define-key evil-visual-state-local-map (kbd ".")
              '(lambda ()
                 (interactive)
                 (call-interactively 'eval-region)
                 (evil-normal-state)))))

(defun my-ess-eval ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
          (call-interactively 'ess-eval-line-and-step)))
(add-hook 'ess-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd ".") 'my-ess-eval)
            (define-key evil-visual-state-local-map (kbd ".") 'my-ess-eval)
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
      nil))
(defun python-eval-region-or-line ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (my-py-send beg end)
    (if (region-active-p)
      (deactivate-mark)
      (next-line 1))))
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

; Console output scrolling.
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

(require 'midnight)
(midnight-delay-set 'midnight-delay "4:30am")
(setq clean-buffer-list-delay-general 2)
; Cleanup every hour.
(setq midnight-period (* 1 60 60))

(require 'multi-term)
(setq multi-term-program "/bin/bash")

(provide 'my-misc)
