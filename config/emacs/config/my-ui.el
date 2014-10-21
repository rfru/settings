(when (display-graphic-p)
  (tool-bar-mode -1)
  (when (equal system-type 'darwin)
    (set-default-font "Tisa Pro 14")
    (setq-default line-spacing 0)
    (add-to-list 'default-frame-alist '(width  . 100))
    (add-to-list 'default-frame-alist '(height . 35))
    (set-fringe-mode 0)
    (scroll-bar-mode -1)
    ))

(defun reset-ui ()
  (menu-bar-mode -1)

  (setq column-number-mode t)

  (add-hook 'window-configuration-change-hook
            (lambda ()
              (global-hl-line-mode t)
              (set-frame-parameter nil 'internal-border-width 4)
              (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 1 1)))
                                        ; Highlight current line


  (custom-set-variables
   '(inhibit-startup-screen t)))

;; (add-hook 'after-make-frame-functions (lambda (frame)
;;                                         (reset-ui)))
(reset-ui)

(require 'pretty-symbols)
(defvar my-pretty-symbols
  (let ((c-like '(c-mode c++-mode go-mode java-mode js2-mode
                        perl-mode cperl-mode ruby-mode
                        python-mode inferior-python-mode)))

  `((?ƒ mine "\\<defun\\>" (emacs-lisp-mode))
    (?ƒ mine "\\<func\\>" (go-mode))
    (?ƒ mine "\\<function\\>" (js2-mode))
    (?ƒ mine "\\<def\\>" (python-mode))
    (?Ṫ mine "\\<true\\>" (java-mode js2-mode go-mode))
    (?Ṫ mine "\\<True\\>" (python-mode))
    (?Ḟ mine "\\<false\\>" (java-mode js2-mode go-mode))
    (?Ḟ mine "\\<False\\>" (python-mode))
    (?Ø mine "\\<void\\>" (java-mode))
    (?Ø mine "\\<null\\>" (java-mode js2-mode))
    (?Ø mine "\\<nil\\>" (emacs-lisp-mode go-mode))
    (?Ø mine "\\<None\\>" (python-mode))
    (?Ǝ mine "\\<defvar\\>" (emacs-lisp-mode))
    (?Ǝ mine "\\<var\\>" (js2-mode go-mode))
    (?← mine "\\<return\\>" (java-mode js2-mode go-mode python-mode))
    (?→ mine "\\<require\\>" (emacs-lisp-mode))
    (?→ mine "\\<import\\>" (java-mode go-mode python-mode))
    ;(?¬ logical ,(rxt-pcre-to-elisp "\\((!)") (java-mode js-mode go-mode))
    )))

(setq pretty-symbol-patterns (append pretty-symbol-patterns my-pretty-symbols))
(setq pretty-symbol-categories '(lambda relational mine))
(add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode)
(add-hook 'java-mode-hook 'pretty-symbols-mode)
(add-hook 'js2-mode-hook 'pretty-symbols-mode)
(add-hook 'python-mode-hook 'pretty-symbols-mode)
(add-hook 'go-mode-hook 'pretty-symbols-mode)

; Word wrapping.
(diminish 'undo-tree-mode)
(diminish 'global-whitespace-mode)
(diminish 'flycheck-mode)
(diminish 'helm-mode)
(diminish 'smartparens-mode)
(diminish 'auto-complete-mode)
(diminish 'whitespace-mode)
(diminish 'magit-auto-revert-mode)
(diminish 'pretty-symbols-mode)

(provide 'my-ui)

