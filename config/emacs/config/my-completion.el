; setup autocompletion
(require 'auto-complete-config)
(require 'go-autocomplete)
(require 'ac-emacs-eclim-source)
(setq-default ac-sources '(ac-source-words-in-same-mode-buffers))
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(add-hook 'haskell-mode-hook (lambda()
                               (setq ac-sources (append '(ac-source-ghc-mod) ac-sources))))
(setq ac-auto-show-menu t)

; Don't use quick help in console.
(setq ac-use-quick-help nil)
;; (when (display-graphic-p)
;;   (setq ac-use-quick-help t))
; Avoid pasting issues.
(setq ac-delay 0.05)
(setq ac-quick-help-delay 0.5)
(setq ac-candidate-limit 100)
(setq web-mode-ac-sources-alist
  '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-same-mode-buffers))))
(add-to-list 'ac-modes 'html-mode)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'protobuf-mode)

(setq ac-disable-faces nil)

(setq ac-auto-show-menu nil)
(global-auto-complete-mode t)

;; (setq-default company-frontends '(
;;                                   company-pseudo-tooltip-frontend
;;                           company-preview-if-just-one-frontend))
;; (global-company-mode t)

(provide 'my-completion)
