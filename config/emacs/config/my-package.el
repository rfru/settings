(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar starter-kit-packages
  (list 'evil
        'pcre2el
        'smooth-scrolling
        'protobuf-mode
        'ess
        'helm-swoop
        'beacon
        'goto-last-change
        'diminish
        'async
        'multi-term
        's
        'dash
        'f
        'expand-region
        'helm-ag
        'evil-matchit ; Enhance % operator for tags and many other structures
        'magit
        'scss-mode
        'groovy-mode
        'emacs-eclim
        'markdown-mode
        'yaml-mode
        'web-mode
        'flycheck
        'visual-regexp
        'lua-mode
        'ace-jump-mode
        'visual-regexp-steroids
        'evil-leader
        'company-jedi
        'highlight-indentation
        'company
        'company-go
        'multiple-cursors
        'buffer-move
        'pretty-symbols
        'volatile-highlights
        'typescript-mode
        'go-mode
        'haskell-mode
        'smartparens
        'js2-mode
        'persistent-scratch
        'undo-tree ; Automatically loaded by evil.
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

; GUI Emacs needs to be similar to shell init.
(require 'exec-path-from-shell)
(add-to-list 'exec-path-from-shell-variables "GOPATH")
(when (memq window-system '(mac ns))
   (exec-path-from-shell-initialize))

(provide 'my-package)
