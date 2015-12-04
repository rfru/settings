;; Define escapes before anything else.
(define-key mc/keymap [escape] 'mc/keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

; Window motions.
(define-key evil-motion-state-map "n" 'other-window)
(define-key evil-normal-state-map "n" 'other-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(define-key evil-motion-state-map (kbd "SPC") 'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "f") 'evil-scroll-page-up)
(define-key evil-normal-state-map (kbd "f") 'evil-scroll-page-up)
(define-key evil-motion-state-map (kbd "C-b") nil)
(define-key evil-motion-state-map (kbd "C-f") nil)

(define-key evil-normal-state-map (kbd "z") 'comment-or-uncomment-region-or-line)
(define-key evil-visual-state-map (kbd "z") 'comment-or-uncomment-region-or-line)

(define-key evil-normal-state-map (kbd "e") 'er/expand-region)
(define-key evil-visual-state-map (kbd "e") 'er/expand-region)
(define-key evil-normal-state-map (kbd "E") 'er/contract-region)
(define-key evil-visual-state-map (kbd "E") 'er/contract-region)

(define-key evil-normal-state-map (kbd "m") 'mc/mark-next-like-this)
(define-key evil-visual-state-map (kbd "m") 'mc/mark-next-like-this)
(define-key evil-normal-state-map (kbd "M") 'mc/unmark-next-like-this)
(define-key evil-visual-state-map (kbd "M") 'mc/unmark-next-like-this)

(define-key evil-normal-state-map (kbd "q") 'last-buffer)

(define-key evil-normal-state-map (kbd "<S-up>") 'buf-move-up)
(define-key evil-normal-state-map (kbd "<S-down>") 'buf-move-down)
(define-key evil-normal-state-map (kbd "<S-left>") 'buf-move-left)
(define-key evil-normal-state-map (kbd "<S-right>") 'buf-move-right)

; Use evil keys for compilation mode.
(define-key compilation-mode-map (kbd "g") nil)
(define-key compilation-mode-map (kbd "h") nil)
(define-key compilation-mode-map (kbd "z") nil)
(define-key compilation-mode-map (kbd "c") nil)

(evil-leader/set-key
  "q" 'delete-window
  "n" 'narrow-or-widen-dwim
  "d" 'my-delete-buffer
  "k" (lambda ()
        (interactive)
        (if (y-or-n-p "Kill buffers?")
            (my-kill-buffers)))
  "w" '(lambda ()
          (interactive)
          (evil-write-all nil))
  "c" 'my-compile
  "g" '(lambda ()
         (interactive)
         (evil-write-all nil)
         (magit-status))
  "v" 'hsplit-last-buffer
  "r" 'revert-buffer
  "s" 'vsplit-last-buffer
  "." 'open-sudo)

(require 'visual-regexp)
(require 'visual-regexp-steroids)
(define-key evil-visual-state-map (kbd "r") 'vr/replace)

(define-key evil-normal-state-map "U" 'redo)
(define-key evil-normal-state-map "\C-r" nil)
(define-key evil-normal-state-map (kbd "<tab>") 'goto-last-change)
(define-key evil-normal-state-map (kbd "S-<tab>") 'goto-last-change-reverse)

(define-key evil-normal-state-map "t" 'helm-show-kill-ring)

(define-key evil-motion-state-map "'" 'helm-M-x)
(define-key evil-visual-state-map "'" 'helm-M-x)

(define-key evil-normal-state-map "`" 'google)

(define-key evil-motion-state-map "c" 'helm-for-files)
(define-key evil-normal-state-map "c" 'helm-for-files)
(define-key evil-motion-state-map "." 'my-find-directories)
(define-key evil-normal-state-map "." 'my-find-directories)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key evil-visual-state-map "/"
  (lambda ()
    (interactive)
    (helm-swoop)))
(define-key evil-normal-state-map "/"
  (lambda ()
    (interactive)
    (if (>= (line-number-at-pos (point-max)) 2000)
        (isearch-forward)
      (helm-swoop :$query ""))))
(define-key evil-normal-state-map "s" 'ace-jump-word-mode)

(define-key evil-normal-state-map "?" 'search)

(evil-define-key 'insert comint-mode-map (kbd "<up>") 'comint-previous-input)
(evil-define-key 'insert comint-mode-map (kbd "<down>") 'comint-next-input)
(evil-define-key 'insert comint-mode-map (kbd "C-z") 'comint-stop-subjob)

(evil-define-key 'normal shell-mode-map (kbd "t") 'helm-comint-input-ring)
(evil-define-key 'normal shell-mode-map (kbd "RET")
  (lambda ()
    (interactive)
    (evil-goto-line)
    (evil-append 0)))
(evil-define-key 'normal term-raw-map (kbd "RET")
  (lambda ()
    (interactive)
    (term-send-return)
    (evil-append 0)))
(evil-define-key 'normal term-raw-map (kbd "<up>")
  (lambda ()
    (interactive)
    (term-send-up)
    (evil-append 0)
    ))
(evil-define-key 'normal term-raw-map (kbd "<down>")
  (lambda ()
    (interactive)
    (term-send-down)
    (evil-append 0)
    ))
(evil-define-key 'normal term-raw-map (kbd "p") 'term-paste)

(evil-define-key 'normal emacs-lisp-mode-map (kbd "RET") 'eval-last-sexp)
(evil-define-key 'normal lisp-interaction-mode-map (kbd "RET") 'eval-last-sexp)
(evil-define-key 'visual emacs-lisp-mode-map (kbd "RET") 'evil-eval-region)
(evil-define-key 'visual lisp-interaction-mode-map (kbd "RET") 'evil-eval-region)
(evil-define-key 'normal ess-mode-map (kbd "RET") 'my-ess-eval)
(evil-define-key 'visual ess-mode-map (kbd "RET") 'my-ess-eval)
(evil-define-key 'visual python-mode-map (kbd "RET") 'python-eval-region-or-line)
(evil-define-key 'normal python-mode-map (kbd "RET") 'python-eval-region-or-line)

(evil-set-initial-state 'compilation-mode 'normal)

(provide 'my-keys)
