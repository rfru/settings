; Save minibuffer history.
(savehist-mode 1)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 0.8)
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

(add-hook 'emacs-lisp-mode-hook (lambda ()
                            (define-key evil-normal-state-local-map (kbd ".") 'eval-last-sexp)
                            (define-key evil-visual-state-local-map (kbd ".") '(lambda ()
                                                                          (interactive)
                                                                          (call-interactively 'eval-region)
                                                                          (evil-normal-state)))
                            ))
(add-hook 'python-mode-hook (lambda ()
                            (define-key evil-visual-state-local-map (kbd ".") '(lambda ()
                                                                           (interactive)
                                                                           (call-interactively 'python-shell-send-region)))
                            ))

(setq vc-follow-symlinks 't)

; Persistent undo!
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo")))

; Tail the compilation buffer.
(setq compilation-scroll-output t)

(provide 'my-misc)
