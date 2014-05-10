(global-set-key (kbd "C-x C-f") 'find-file-other-window)
(global-set-key "\C-xk" 'kill-this-buffer)

; setup evil
(require 'evil)
(evil-mode 1)
(define-key evil-motion-state-map ";" 'evil-ex)
(evil-ex-define-cmd "bd[elete]" 'kill-this-buffer)
; Automatically save when quitting.
(evil-ex-define-cmd "q[uit]" 'evil-save-modified-and-close)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(define-key evil-normal-state-map (kbd "c") 'comment-or-uncomment-region-or-line)
(define-key evil-visual-state-map (kbd "c") 'comment-or-uncomment-region-or-line)

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

(global-set-key (kbd "<f4>") (kbd "<escape>"))
;; Don't wait for any other keys after escape is pressed.
(setq evil-esc-delay 0)
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key minibuffer-local-map (kbd "<f4>") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map (kbd "<f4>") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map (kbd "<f4>") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map (kbd "<f4>") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map (kbd "<f4>") 'minibuffer-keyboard-quit)

(provide 'my-motions)
