(require 'flycheck)
(setq-default flycheck-disabled-checkers '(html-tidy haskell-ghc))
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
;; (defun eclim--project-dir (&optional filename)
;;    "/usr/local/google/users/mtlin/magicjar/home-citc/magicjar/eclipse")
(global-eclim-mode)

(defun eclim-complete ()
  (interactive)
  (auto-complete '(ac-source-emacs-eclim)))
(define-key evil-insert-state-map (kbd "C-SPC") 'eclim-complete)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq web-mode-enable-current-element-highlight t)

(require 'smartparens-config)
(show-smartparens-global-mode nil)
(smartparens-global-mode t)

(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)

; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq c-basic-offset 2)
(setq css-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq python-indent-offset 2)
(setq-default evil-shift-width 2)

(require 'whitespace)
(setq whitespace-style '(face lines-tail empty trailing))
(defun my-set-whitespace (col)
  (whitespace-mode -1)
  (setq whitespace-line-column col)
  (whitespace-mode 1))
(defun my-set-whitespace-normal()
  (my-set-whitespace 120))
(defun my-set-whitespace-less ()
  (my-set-whitespace 100))
(defun my-whitespace-hook ()
  (let ((mode major-mode))
    (cond ((s-equals? mode "java-mode") (my-set-whitespace-less))
          ((s-equals? mode "js2-mode") (my-set-whitespace-less))
          (t (my-set-whitespace-normal)))))
(add-hook 'prog-mode-hook 'my-whitespace-hook)

(add-to-list 'load-path "~/.emacs.d/ghc")
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda ()
                               (turn-on-haskell-indentation)
                               (ghc-init)))
(add-hook 'haskell-mode-hook (lambda nil
                               (add-hook 'after-save-hook
                                         (lambda nil (ghc-check-syntax))
                                         nil 'local)))

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

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(setq sp-autoescape-string-quote nil)

(provide 'my-prog)
