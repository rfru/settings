(require 'flycheck)
(setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
(global-flycheck-mode)

(require 'eclim)
;(setq eclim-print-debug-messages t)
(setq eclim-problems-refresh-delay 3)
;; (add-hook 'java-mode-hook (lambda ()
;;                             (when (and buffer-file-name
;;                                      (eclim--accepted-p buffer-file-name)
;;                                      (eclim--project-dir buffer-file-name))
;;                               (eclim-mode 1))))
(add-to-list 'eclim--file-coding-system-mapping '("no-conversion" . "utf8"))
(setq eclim-executable "/Users/mtlin/eclipse/eclim")
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
(defun eclim--project-dir (&optional filename)
   "/usr/local/google/users/mtlin/magicjar/home-citc/magicjar/eclipse")
(global-eclim-mode)

(defun eclim-complete ()
  (interactive)
  (auto-complete '(ac-source-emacs-eclim)))
(define-key evil-insert-state-map (kbd "C-SPC") 'eclim-complete)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'smartparens-config)
(show-smartparens-global-mode nil)
(smartparens-global-mode t)

(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)

(provide 'my-prog)
