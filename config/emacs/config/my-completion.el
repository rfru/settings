; Enables tooltip help
(require 'pos-tip)

; setup autocompletion
(require 'auto-complete-config)
(require 'go-autocomplete)
(require 'ac-emacs-eclim-source)
(setq-default ac-sources '(ac-source-dictionary ac-source-words-in-same-mode-buffers ac-source-filename))
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(setq ac-auto-start 0)
(setq ac-auto-show-menu t)
(setq ac-quick-help-delay 0.5)
(setq ac-candidate-limit 100)
(add-to-list 'ac-modes 'html-mode)
(add-to-list 'ac-modes 'web-mode)
(setq ac-disable-faces nil)
(global-auto-complete-mode t)

(provide 'my-completion)
