(require 'helm-config)
(require 'helm)
(require 'helm-buffers)
(require 'helm-files)

(setq helm-mp-matching-method 'multi3p)
(setq helm-mp-highlight-delay 0.1)
(setq helm-M-x-always-save-history t)
(setq helm-split-window-default-side 'other)
(setq helm-quick-update t)
(define-key evil-motion-state-map (kbd "t") 'helm-M-x)
(define-key evil-visual-state-map (kbd "t") 'helm-M-x)

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
        helm-source-recentf)
      )

(defun swoop-blank-query()
  (interactive)
  (helm-swoop :$query ""))
(define-key evil-motion-state-map (kbd ",") 'helm-for-files)
(define-key evil-motion-state-map (kbd "z") 'find-file)
(define-key evil-motion-state-map (kbd "/") 'swoop-blank-query)
(define-key evil-normal-state-map (kbd ",") 'helm-for-files)
(define-key evil-normal-state-map (kbd "z") 'find-file)
(define-key evil-normal-state-map (kbd "/") 'swoop-blank-query)

(setq helm-candidate-number-limit 25)
(setq helm-buffer-max-length nil)
(helm-mode 1)

(provide 'my-helm)
