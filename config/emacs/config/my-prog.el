(require 'expand-region)
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'js2-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-mode)

(require 'flycheck)
(setq-default flycheck-disabled-checkers
              '(html-tidy haskell-ghc emacs-lisp-checkdoc r-lintr))

(global-flycheck-mode)

; Make compile commands per buffer basis.
(make-variable-buffer-local 'compile-command)
(setq compilation-ask-about-save nil)

(require 'magit)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[6]?\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . web-mode))
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-css-colorization t)
; This is a temporary fix for web-mode-mark-and-expand, which doesn't work with expand region anymore.
(remove-hook 'web-mode-hook 'er/add-web-mode-expansions)

(require 'smartparens-config)
(show-smartparens-global-mode t)
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
;; (setq whitespace-style '(face lines-tail empty trailing))
;; (setq whitespace-line-column 100)
(setq whitespace-style '(face empty trailing))
;; (setq whitespace-global-modes '(java-mode python-mode js2-mode go-mode emacs-lisp-mode c++-mode web-mode protobuf-mode))
(global-whitespace-mode 1)

(add-to-list 'load-path "~/.emacs.d/ghc")
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda ()
                               (add-hook 'after-save-hook
                                         (lambda nil (ghc-check-syntax))
                                         nil 'local)
                               (turn-on-haskell-indentation)
                               (ghc-init)))

(setq-default compile-command nil)
(defun my-compile ()
  (interactive)
  (cond
   ((eq 'ess-mode major-mode)
    (call-interactively 'ess-eval-buffer))
   (t
    (when (not compile-command)
      (setq compile-command
            (cond
             ((and (file-remote-p default-directory) (s-equals? "mtl" (tramp-file-name-host (tramp-dissect-file-name default-directory))))
              (if (s-contains? "test" (buffer-name))
                  "blaze test :all"
                "blaze build :all"))
             ((eq 'go-mode major-mode)
              (if (s-contains? "_test" (buffer-name))
                  "go test"
                "go build"))
             ((eq 'sh-mode major-mode)
              (s-concat "sh " (buffer-name)))
             ((eq 'python-mode major-mode)
              (s-concat "python " (buffer-name)))
             )))
    (call-interactively 'compile))))

(defconst scss-font-lock-keywords
  ;; Variables
    '(("$[a-z_-][a-z-_0-9]*" . font-lock-constant-face)))
(define-derived-mode scss-mode css-mode "SCSS"
    "Major mode for editing SCSS files, http://sass-lang.com/
Special commands:
\\{scss-mode-map}"
    (font-lock-add-keywords nil scss-font-lock-keywords)
    ;; Add the single-line comment syntax ('//', ends with newline)
    ;; as comment style 'b' (see "Syntax Flags" in elisp manual)
    (modify-syntax-entry ?/ ". 124" css-mode-syntax-table)
    (modify-syntax-entry ?* ". 23b" css-mode-syntax-table)
      (modify-syntax-entry ?\n ">" css-mode-syntax-table))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; (setq
;;  python-shell-interpreter "ipython"
;;  python-shell-interpreter-args ""
;;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;  python-shell-completion-setup-code
;;    "from IPython.core.completerlib import module_completion"
;;  python-shell-completion-module-string-code
;;    "';'.join(module_completion('''%s'''))\n"
;;  python-shell-completion-string-code
;;    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."
  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))

(setq sp-autoescape-string-quote nil)

(load "ess-site")
; ESS will not print the evaluated comands, also speeds up the evaluation
(setq ess-eval-visibly nil)
(define-key inferior-ess-mode-map (kbd ",") nil)
(ess-toggle-underscore nil)
(setq ess-ask-for-ess-directory nil)

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string
     (if mark-active
         (concat (buffer-substring (region-beginning) (region-end)) " "
                 (cond
                  ((eq major-mode 'go-mode) "golang")
                  ((eq major-mode 'python-mode) "python")
                  ((eq major-mode 'emacs-lisp-mode) "elisp")
                  ((eq major-mode 'ruby-mode) "rails")))
       (read-string "Google: "))))))

(provide 'my-prog)
