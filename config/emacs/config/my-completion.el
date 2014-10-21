; setup autocompletion
(require 'auto-complete)
(require 'auto-complete-config)
(require 'go-autocomplete)
(setq-default ac-sources '(ac-source-words-in-same-mode-buffers ac-source-dictionary))
(add-to-list 'ac-modes 'shell-mode)
(add-hook 'shell-mode-hook
          (lambda ()
            (make-local-variable 'ac-auto-start)
            (setq ac-auto-start 3)
            (setq ac-sources '(ac-source-files-in-current-dir))))
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(add-hook 'haskell-mode-hook (lambda()
                               (setq ac-sources (append '(ac-source-ghc-mod) ac-sources))))

; Avoid pasting issues.
(setq ac-delay 0.05)

(setq ac-candidate-limit 100)
(setq web-mode-ac-sources-alist
  '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-same-mode-buffers))))
(add-to-list 'ac-modes 'html-mode)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'protobuf-mode)

;; (require 'auto-complete-clang)
;; (defun my-ac-cc-mode-setup ()
;;   (setq ac-sources (append '(ac-source-clang) ac-sources)))
;; (add-hook 'c++-mode-hook 'my-ac-cc-mode-setup)

(setq ac-disable-faces nil)
;; (setq ac-auto-show-menu 0.05)
;; (setq ac-quick-help-delay 0.5)
(global-auto-complete-mode t)

(provide 'my-completion)
