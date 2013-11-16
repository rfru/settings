; Save minibuffer history.
(savehist-mode 1)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 1.5)
(auto-save-buffers-enhanced t)

(require 'saveplace)
(setq-default save-place t)

; Spell check.
;(add-hook 'text-mode-hook 'flyspell-mode)
;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

; Refresh all buffers periodically.
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
                            (define-key evil-normal-state-map (kbd ".") 'eval-last-sexp)))

(setq vc-follow-symlinks 't)

; Persistent undo!
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo")))

; Tail the compilation buffer.
(setq compilation-scroll-output t)

(provide 'my-misc)