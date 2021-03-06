(setq console-font
      (if (eq system-type "darwin")
          "Menlo"
        "Consolas"))
(add-hook 'term-mode-hook (lambda ()
                            (face-remap-add-relative 'default `(:family ,console-font :height 120))
                            (setq line-spacing 3)))
(defun reset-ui ()
  (when (display-graphic-p)
    (tool-bar-mode -1)
    (set-frame-font "Tisa Pro 12" t t)
    (setq-default line-spacing 1)
    (add-to-list 'default-frame-alist '(width  . 150))
    (add-to-list 'default-frame-alist '(height . 35))
    (set-fringe-mode 0)
    (scroll-bar-mode -1))
    (menu-bar-mode -1)
    (setq column-number-mode t)

    (add-hook 'window-configuration-change-hook
              (lambda ()
                (set-frame-parameter nil 'internal-border-width 0)
                (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 1 1)))
                                        ; Highlight current line
    (custom-set-variables
     '(inhibit-startup-screen t)))
(add-hook 'after-make-frame-functions
          (lambda (frame) (reset-ui)))
(reset-ui)

(set-fontset-font "fontset-default"  '(#xe000 . #xf8ff) "Material Icons-12")

(require 'pretty-symbols)
(defvar my-pretty-symbols
  (let ((c-like '(c-mode c++-mode go-mode java-mode js2-mode
                        perl-mode cperl-mode ruby-mode
                        python-mode inferior-python-mode)))

  `((?𝒇 mine "\\<defun\\>" (emacs-lisp-mode))
    (?𝒇 mine "\\<func\\>" (go-mode))
    (?𝒇 mine "\\<function\\>" (js2-mode web-mode lua-mode typescript-mode))
    (?𝒇 mine "\\<def\\>" (python-mode))
    (?✓ mine "\\<true\\>" (java-mode js2-mode go-mode web-mode typescript-mode c++-mode))
    (?✓ mine "\\<True\\>" (python-mode))
    (?✓ mine "\\<YES\\>" (objc-mode))
    (?✗ mine "\\<false\\>" (java-mode js2-mode go-mode web-mode typescript-mode c++-mode))
    (?✗ mine "\\<False\\>" (python-mode))
    (?✗ mine "\\<NO\\>" (objc-mode))
    (?Ø mine "\\<void\\>" (java-mode objc-mode c++-mode))
    (?Ø mine "\\<nullptr\\>" (c++-mode))
    (?Ø mine "\\<NULL\\>" (objc-mode))
    (?Ø mine "\\<null\\>" (java-mode js2-mode web-mode typescript-mode))
    (?Ø mine "\\<nil\\>" (objc-mode emacs-lisp-mode go-mode))
    (?Ø mine "\\<None\\>" (python-mode))
    (?Ǝ mine "\\<new\\>" (java-mode js2-mode web-mode typescript-mode c++-mode))
    (?∈ mine "\\<in\\>" (python-mode js2-mode web-mode typescript-mode))
    (? mine "\\<defvar\\>" (emacs-lisp-mode))
    (? mine "\\<local\\>" (lua-mode))
    (? mine "\\<var\\>" (go-mode))
    (? mine "\\<auto\\>" (c++-mode))
    (? mine "\\<let\\>" (js2-mode web-mode typescript-mode))
    (? mine "\\<const\\>" (js2-mode typescript-mode c++-mode))
    (? mine "\\<public\\>" (java-mode))
    (? mine "\\<public:" (c++-mode))
    (? mine "\\<export\\>" (js2-mode typescript-mode))
    (? mine "\\<private\\>" (java-mode))
    (? mine "\\<private:" (c++-mode))
    (?← mine "\\<return\\>" (java-mode js2-mode go-mode python-mode web-mode objc-mode lua-mode typescript-mode c++-mode))
    (? mine "#import\\>" (objc-mode))
    (? mine "#include\\>" (c++-mode))
    (? mine "\\<require\\>" (emacs-lisp-mode))
    (? mine "\\<require\\>" (lua-mode))
    (? mine "\\<import\\>" (java-mode go-mode python-mode typescript-mode))
    ; ﹕•
    (?• mine "\\<self\\." (objc-mode python-mode))
    (?• mine "\\<this\\." (java-mode js2-mode web-mode typescript-mode))
    (?• mine "\\<this" (c++-mode))
    ;(?¬ logical ,(rxt-pcre-to-elisp "\\((!)") (java-mode js-mode go-mode))
    )))

(setq pretty-symbol-patterns (append pretty-symbol-patterns my-pretty-symbols))
(setq pretty-symbol-categories '(lambda relational mine))
(add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode)
(add-hook 'objc-mode-hook 'pretty-symbols-mode)
(add-hook 'lua-mode-hook 'pretty-symbols-mode)
(add-hook 'web-mode-hook 'pretty-symbols-mode)
(add-hook 'java-mode-hook 'pretty-symbols-mode)
(add-hook 'js2-mode-hook 'pretty-symbols-mode)
(add-hook 'typescript-mode-hook 'pretty-symbols-mode)
(add-hook 'python-mode-hook 'pretty-symbols-mode)
(add-hook 'go-mode-hook 'pretty-symbols-mode)
(add-hook 'c++-mode-hook 'pretty-symbols-mode)

(diminish 'undo-tree-mode)
(diminish 'volatile-highlights-mode)
(diminish 'global-whitespace-mode)
(diminish 'flycheck-mode)
(diminish 'helm-mode)
(diminish 'smartparens-mode)
(diminish 'company-mode)
(diminish 'whitespace-mode)
(diminish 'pretty-symbols-mode)

(provide 'my-ui)
