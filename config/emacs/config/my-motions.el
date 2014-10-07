(require 'evil)
(require 'evil-matchit)
(require 'multiple-cursors)
(global-evil-matchit-mode 1)
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
(evil-define-command evil-delete-buffer-keep-windows
  (buffer &optional bang)
  (interactive "<b><!>")
  (with-current-buffer (or buffer (current-buffer))
    (when bang
      (set-buffer-modified-p nil)
      (dolist (process (process-list))
        (when (eq (process-buffer process)
                  (current-buffer))
          (set-process-query-on-exit-flag process nil))))
    (if (and (fboundp 'server-edit)
             (boundp 'server-buffer-clients)
             server-buffer-clients)
        (server-edit)
		  (kill-buffer nil))))

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


(defun my-quit ()
  (interactive)
  (when (and (buffer-modified-p) (not (string-match "^\\*.+?\\*$" (buffer-name))))
    (evil-write nil nil nil buffer-file-name t))
  (evil-delete-buffer-keep-windows (current-buffer) t))
(define-key evil-motion-state-map "Q" 'my-quit)
(define-key evil-normal-state-map "Q" 'my-quit)

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

; Window motions.
(define-key evil-motion-state-map (kbd "N") (lambda () (interactive) (other-window -1)))
(define-key evil-motion-state-map (kbd "n") 'other-window)

(define-key evil-motion-state-map (kbd "SPC") 'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "<S-SPC>") 'evil-scroll-page-up)
(define-key evil-motion-state-map (kbd "C-b") nil)
(define-key evil-motion-state-map (kbd "C-f") nil)

; Don't move the cursor backwards after exiting insert mode.
(setq evil-move-cursor-back nil)

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

(define-key evil-normal-state-map (kbd "m") 'mc/mark-next-like-this)
(define-key evil-visual-state-map (kbd "m") 'mc/mark-next-like-this)
(define-key evil-normal-state-map (kbd "M") 'mc/unmark-next-like-this)
(define-key evil-visual-state-map (kbd "M") 'mc/unmark-next-like-this)

(defun last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(define-key evil-normal-state-map (kbd "q") 'last-buffer)

(require 'evil-leader)
(setq evil-leader/leader ";")
(evil-leader/set-key
  "q" 'delete-window
  "w" '(lambda ()
          (interactive)
          (evil-write-all nil))
  "c" 'compile
  "g" 'magit-status
  "v" (lambda () (interactive)(split-window-horizontally) (other-window 1))
  "s" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-evil-leader-mode)

(require 'visual-regexp)
(require 'visual-regexp-steroids)
(define-key evil-normal-state-map (kbd "?") 'vr/replace)
(define-key evil-visual-state-map (kbd "?") 'vr/replace)

(evil-mode 1)

(define-key evil-normal-state-map "U" 'undo-tree-redo)
(define-key evil-normal-state-map "\C-r" nil)
(define-key evil-normal-state-map (kbd "RET") '(lambda() (interactive) (evil-goto-mark ?`)))

(provide 'my-motions)
