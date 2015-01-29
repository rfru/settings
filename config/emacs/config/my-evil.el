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

;; Don't wait for any other keys after escape is pressed.
(setq evil-esc-delay 0)

; Don't move the cursor backwards after exiting insert mode.
(setq evil-move-cursor-back nil)

(require 'evil-leader)
(setq evil-leader/leader ";")
(global-evil-leader-mode)

;; Make HJKL keys work in special buffers
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)

(evil-mode 1)

(provide 'my-evil)
