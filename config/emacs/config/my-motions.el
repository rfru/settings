(setq indent-line-function 'insert-tab)
(global-set-key (kbd "C-x C-f") 'find-file-other-window)
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key "\C-xk" 'kill-this-buffer)

; setup evil
(require 'evil)
(evil-mode 1)
(define-key evil-motion-state-map ";" 'evil-ex)
(evil-ex-define-cmd "bd[elete]" 'kill-this-buffer)

;; (define-key evil-motion-state-map (kbd "<down>") 'evil-scroll-page-down)
;; (define-key evil-motion-state-map (kbd "<up>") 'evil-scroll-page-up)
;; ; Prevent normal usage.
;; (define-key evil-motion-state-map (kbd "\C-f") nil)
;; (define-key evil-motion-state-map (kbd "\C-b") nil)

(require 'expand-region)
(define-key evil-normal-state-map (kbd "e") 'er/expand-region)
(define-key evil-visual-state-map (kbd "e") 'er/expand-region)
(define-key evil-normal-state-map (kbd "E") 'er/contract-region)
(define-key evil-visual-state-map (kbd "E") 'er/contract-region)

(require 'multiple-cursors)
(define-key evil-normal-state-map (kbd "m") 'mc/mark-next-like-this)
(define-key evil-visual-state-map (kbd "m") 'mc/mark-next-like-this)
(define-key evil-normal-state-map (kbd "M") 'mc/unmark-next-like-this)
(define-key evil-visual-state-map (kbd "M") 'mc/unmark-next-like-this)

(require 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "f") 'ace-jump-mode)

(defun last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(define-key evil-normal-state-map (kbd "q") 'last-buffer)

;; Is this still necessary?
;; esc quits
;; Don't wait for any other keys after escape is pressed.
(setq evil-esc-delay 0)
;; (defun minibuffer-keyboard-quit ()
;;   "Abort recursive edit.
;; In Delete Selection mode, if the mark is active, just deactivate it;
;; then it takes a second \\[keyboard-quit] to abort the minibuffer."
;;   (interactive)
;;   (if (and delete-selection-mode transient-mark-mode mark-active)
;;       (setq deactivate-mark  t)
;;     (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
;;     (abort-recursive-edit)))
;; (define-key evil-normal-state-map (kbd "<f7>") 'keyboard-quit)
;; (define-key evil-visual-state-map (kbd "<f7>") 'keyboard-quit)
;; (define-key minibuffer-local-map (kbd "<f7>") 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-ns-map (kbd "<f7>") 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-completion-map (kbd "<f7>") 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-must-match-map (kbd "<f7>") 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-isearch-map (kbd "<f7>") 'minibuffer-keyboard-quit)

(provide 'my-motions)
