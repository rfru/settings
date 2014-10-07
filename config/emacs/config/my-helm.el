(require 'helm-config)
(require 'helm)
(require 'helm-buffers)
(require 'helm-files)

(setq helm-mp-matching-method 'multi3p)
(setq helm-mp-highlight-delay 0.1)
(setq helm-M-x-always-save-history t)
(setq helm-split-window-default-side 'other)
(setq helm-quick-update t)
(setq helm-buffer-details-flag nil)
(define-key evil-motion-state-map (kbd "'") 'helm-M-x)
(define-key evil-visual-state-map (kbd "'") 'helm-M-x)

(require 'recentf)
(setq recentf-exclude '("\\.recentf" "^/tmp/" "/.git/" "/.emacs.d/elpa/"))
(setq recentf-max-saved-items 100)
(setq recentf-auto-cleanup 'never)
(setq recentf-save-file (expand-file-name "~/.emacs.d/.recentf" user-emacs-directory))
(setq recentf-auto-save-timer
      (run-with-idle-timer 15 t 'recentf-save-list))
(recentf-mode 1)

(setq helm-for-files-preferred-list
      '(helm-source-buffers-list
        helm-source-recentf))

(defun my-find-files ()
  (interactive)
  (helm-find-files-1 (expand-file-name (helm-current-directory)) nil))

(require 'helm-swoop)
(setq helm-swoop-split-direction 'split-window-horizontally)
(define-key evil-motion-state-map (kbd ",") 'helm-for-files)
(define-key evil-normal-state-map (kbd ",") 'helm-for-files)
(define-key evil-motion-state-map (kbd "z") 'my-find-files)
(define-key evil-normal-state-map (kbd "z") 'my-find-files)
(define-key evil-visual-state-map (kbd "f") 'helm-swoop)
(define-key evil-normal-state-map (kbd "f") (lambda () (interactive) (helm-swoop :$query "")))
(define-key evil-normal-state-map (kbd "/") 'ace-jump-mode)

(defun search()
  (interactive)
  (if (file-remote-p default-directory)
      (helm-ag)
    (helm-do-ag)))
(define-key evil-motion-state-map (kbd "F") 'search)
(define-key evil-normal-state-map (kbd "F") 'search)

(define-key helm-map [escape] 'helm-keyboard-quit)

(setq helm-candidate-number-limit 25)
(setq helm-buffer-max-length 75)
(helm-mode 1)

(provide 'my-helm)
