(load-theme 'solarized-light t)
;(setq noctilux-broken-srgb nil)
;(load-theme 'noctilux t)

(set-face-attribute 'default nil :family "Consolas")
(set-face-attribute 'default nil :height 140)
(set-fontset-font "fontset-default"
                  'unicode
                  '("Consolas" . "iso10646-1"))
(setq-default line-spacing 6)
(add-to-list 'default-frame-alist '(width  . 125))
(add-to-list 'default-frame-alist '(height . 50))

(tool-bar-mode -1)
(menu-bar-mode -1)

(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq evil-shift-width 2)
(setq-default tab-width 2)
(setq c-basic-offset 2)
(setq css-indent-offset 2)

(require 'whitespace)
(setq-default whitespace-style '(face empty trailing))
(add-hook 'prog-mode-hook (lambda ()
                            (setq whitespace-line-column 80)
                            (setq whitespace-style '(face lines-tail empty trailing))
                            (whitespace-mode 1)
                            ))
(add-hook 'go-mode-hook (lambda ()
                          (whitespace-mode -1)
                          (setq whitespace-line-column 120)
                          (whitespace-mode 1)
                          ))
(add-hook 'web-mode-hook (lambda () (whitespace-mode -1)))


(defvar my-pretty-symbols
  (let ((c-like '(c-mode c++-mode go-mode java-mode js-mode
                        perl-mode cperl-mode ruby-mode
                        python-mode inferior-python-mode)))

  `((?ƒ mine "\\<defun\\>" (emacs-lisp-mode))
    (?ƒ mine "\\<func\\>" (go-mode))
    (?ƒ mine "\\<def\\>" (python-mode))
    (?Ṫ mine "\\<true\\>" (java-mode js-mode go-mode))
    (?Ṫ mine "\\<True\\>" (python-mode))
    (?Ḟ mine "\\<false\\>" (java-mode js-mode go-mode))
    (?Ḟ mine "\\<False\\>" (python-mode))
    (?Ø mine "\\<void\\>" (java-mode))
    (?Ø mine "\\<null\\>" (java-mode js-mode))
    (?Ø mine "\\<nil\\>" (emacs-lisp-mode go-mode))
    (?Ø mine "\\<None\\>" (python-mode))
    (?Ǝ mine "\\<defvar\\>" (emacs-lisp-mode))
    (?Ǝ mine "\\<var\\>" (js-mode go-mode))
    (?← mine "\\<return\\>" (java-mode js-mode go-mode python-mode))
    (?→ mine "\\<require\\>" (emacs-lisp-mode))
    (?→ mine "\\<import\\>" (java-mode go-mode python-mode))
    (?₀ mine "\\[0\\]" (,@c-like))
    (?₁ mine "\\[1\\]" (,@c-like))
    (?₂ mine "\\[2\\]" (,@c-like))
    (?₃ mine "\\[3\\]" (,@c-like))
    (?₄ mine "\\[4\\]" (,@c-like))
    (?₅ mine "\\[5\\]" (,@c-like))
    (?₆ mine "\\[6\\]" (,@c-like))
    (?₇ mine "\\[7\\]" (,@c-like))
    (?₈ mine "\\[8\\]" (,@c-like))
    (?₉ mine "\\[9\\]" (,@c-like))
    ;(?¬ logical ,(rxt-pcre-to-elisp "\\((!)") (java-mode js-mode go-mode))
    )))

(setq pretty-symbol-patterns (append pretty-symbol-patterns my-pretty-symbols))
(setq pretty-symbol-categories '(lambda relational mine))
(add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode)
(add-hook 'java-mode-hook 'pretty-symbols-mode)
(add-hook 'js-mode-hook 'pretty-symbols-mode)
(add-hook 'python-mode-hook 'pretty-symbols-mode)
(add-hook 'go-mode-hook 'pretty-symbols-mode)
; Highlight current line
(global-hl-line-mode 1)

(custom-set-variables
 '(inhibit-startup-screen t))

(provide 'my-theme)
