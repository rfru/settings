(global-set-key (kbd "C-x C-f") 'find-file-other-window)

; setup evil
(require 'evil)
(defcustom evil-state-modes
  '(fundamental-mode
    text-mode
    prog-mode
    sws-mode
    dired-mode
    comint-mode
    log-edit-mode
    compilation-mode)
  "List of modes that should start up in Evil state."
  :type '(repeat (symbol)))

(defun my-enable-evil-mode ()
  (if (apply 'derived-mode-p evil-state-modes)
      (turn-on-evil-mode)))
(add-hook 'after-change-major-mode-hook 'my-enable-evil-mode)
(define-key evil-motion-state-map ";" 'evil-ex)
(evil-ex-define-cmd "bd[elete]" 'kill-this-buffer)

; Window motions.
(define-key evil-motion-state-map (kbd "<right>") 'windmove-right)
(define-key evil-motion-state-map (kbd "<left>") 'windmove-left)
(define-key evil-motion-state-map (kbd "<up>") 'windmove-up)
(define-key evil-motion-state-map (kbd "<down>") 'windmove-down)
(define-key evil-motion-state-map (kbd "<S-right>") 'buf-move-right)
(define-key evil-motion-state-map (kbd "<S-left>") 'buf-move-left)
(define-key evil-motion-state-map (kbd "<S-up>") 'buf-move-up)
(define-key evil-motion-state-map (kbd "<S-down>") 'buf-move-down)

(define-key evil-motion-state-map (kbd "SPC") 'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "<S-SPC>") 'evil-scroll-page-up)
(define-key evil-motion-state-map (kbd "C-b") nil)
(define-key evil-motion-state-map (kbd "C-f") nil)

; Automatically save when quitting.
(evil-define-command evil-my-save-quit (file &optional bang)
  "Saves the current buffer and closes the window."
  :repeat nil
  (interactive "<f><!>")
  (when (and (buffer-modified-p) (not (string-match "^\\*.+?\\*$" (buffer-name))))
    (evil-write nil nil nil file bang))
    (evil-quit))
(evil-ex-define-cmd "q[uit]" 'evil-my-save-quit)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (if (string= "web-mode" major-mode)
        (web-mode-comment-or-uncomment)
        (comment-or-uncomment-region beg end))))
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
(define-key evil-motion-state-map (kbd "f") 'ace-jump-mode)

(defun last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(define-key evil-normal-state-map (kbd "q") 'last-buffer)

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
(define-key mc/keymap [escape] 'mc/keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(evil-mode 1)

(provide 'my-motions)
