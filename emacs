(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar starter-kit-packages (list 'evil
				   'auto-complete
                                   'color-theme-solarized
				   'solarized-theme
				   'ace-jump-mode
                                   'multiple-cursors
                                   ;'flycheck
				   'haskell-mode
				   'autopair
                                   'expand-region
				   'js2-mode
				   'flymake-cursor
				   'undo-tree ; Automatically loaded by evil.
				   'mmm-mode
				   'rainbow-delimiters
				   'exec-path-from-shell
				   'helm)
  "Libraries that should be installed by default.")

(defun starter-kit-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package starter-kit-packages)
    (unless (or (member package package-activated-list)
		(functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))


;; On your first run, this should pull in all the base packages.
(when (not package-archive-contents) (package-refresh-contents))
(starter-kit-elpa-install)

(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

(setq-default indent-tabs-mode nil)
; Refresh all buffers periodically.
(global-auto-revert-mode t)

; setup evil
(require 'evil)
(evil-mode 1)

(add-to-list 'auto-mode-alist '("\\.tpl\\'" . html-mode))

(semantic-mode 1)

; setup autocompletion
(require 'auto-complete-config)

; Example
;(defun ac-css-mode-setup ()
;    (setq ac-sources (append '(ac-source-css-property) ac-sources)))
(setq-default ac-sources '(ac-source-semantic ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(global-auto-complete-mode t)

; setup autopair
(autoload 'autopair-global-mode "autopair" nil t)
(autopair-global-mode)
; don't activate in lisp
(add-hook 'lisp-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))

(require 'helm-config)
(require 'helm)
(require 'helm-buffers)
(require 'helm-files)

(defvar helm-source-g4-opened
    '((name . "g4-opened")
      ;(init . (lambda() (helm-attrset 'delayed g4-delay)))
      (candidates-process
       . (lambda () (start-process "g4-opened-process" nil "g4opened.py" helm-pattern)))
      (type . file)
      (requires-pattern . 3)
      (delayed))
      "Source for currently opened g4 files.")

(defun my-helm ()
  (interactive)
  (helm-other-buffer
   '(helm-c-source-buffers-list
     helm-c-source-recentf
     helm-source-g4-opened)
   " *my-helm*"))

(define-key evil-normal-state-map (kbd ",") 'my-helm)
(setq helm-buffer-max-length 100)
(setq helm-input-idle-delay 0.1)
(setq helm-idle-delay 0.1)
(helm-mode 1)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

; setup ghc-mod (cabal install ghc-mod, hlint)
(setq exec-path (cons "~/.cabal/bin/" exec-path))
; need to copy emacs scripts to here
(add-to-list 'load-path "~/.emacs.d/ghc-mod/")
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
; Reparse buffer for new modes
(setq mmm-parse-when-idle t)
(mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-js)
(mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-css)
(set-face-background 'mmm-default-submode-face "gray95")

(eval-after-load 'flymake '(require 'flymake-cursor))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; Defer to built-in js-mode, but keep syntax highlighting.
(add-hook 'js-mode-hook 'js2-minor-mode)

(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(define-key evil-normal-state-map (kbd "f") 'ace-jump-mode)

(require 'expand-region)
(define-key evil-normal-state-map (kbd "]") 'er/expand-region)
(define-key evil-normal-state-map (kbd "[") 'er/contract-region)
(define-key evil-visual-state-map (kbd "]") 'er/expand-region)
(define-key evil-visual-state-map (kbd "[") 'er/contract-region)

(require 'multiple-cursors)
(define-key evil-visual-state-map (kbd "m") 'mc/mark-next-like-this)
(define-key evil-normal-state-map (kbd "m") 'mc/mark-next-like-this)

; Tail the compilation buffer.
(setq compilation-scroll-output t)

;; Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
    (interactive)
      "Return true if system is darwin-based (Mac OS X)"
        (string-equal system-type "darwin")
          )

;; Check if system is GNU/Linux
(defun system-type-is-gnu ()
    (interactive)
      "Return true if system is GNU/Linux-based"
        (string-equal system-type "gnu/linux")
          )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))

; setup colors
(require 'color-theme)
(require 'color-theme-solarized)
(setq solarized-broken-srgb t)
(load-theme 'solarized-light t)

(tool-bar-mode -1)
(menu-bar-mode 0)

;(add-hook 'after-init-hook #'global-flycheck-mode)
(load-file "/usr/share/emacs/site-lisp/google/google.el")
(require 'google)
(require 'google-flymake)
(require 'google3-build)
(setq google-build-system "blaze")
