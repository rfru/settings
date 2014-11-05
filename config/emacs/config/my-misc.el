(setq indent-line-function 'insert-tab)
(setq large-file-warning-threshold 100000000)
(global-set-key (kbd "RET") 'newline-and-indent)

(setq ring-bell-function 'ignore)

(setq debug-on-quit nil)

(defun my-yes-or-mumble-p (prompt)
    "PROMPT user with a yes-or-no question, but only test for yes."
    (if (string= "y"
                 (downcase
                  (read-from-minibuffer
                   (concat prompt "(y or n) "))))
        t
      nil))
(defalias 'yes-or-no-p 'my-yes-or-mumble-p)

; Save minibuffer history and other variables.
(setq savehist-additional-variables
  '(compile-history)
  savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

(require 'saveplace)
(setq-default save-place t)

(setq lazy-highlight-initial-delay 0)
(setq revert-without-query '(".*"))

; Only backup locally
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
; Don't use builtin autosave.
(setq auto-save-default nil)

(require 'smooth-scrolling)
(setq smooth-scroll-margin 10)

(require 'tramp)
(setq tramp-verbose 3)
(setq vc-handled-backends nil)
(setq vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))
(setq tramp-remote-path '(tramp-own-remote-path))

(defun evil-eval-region ()
     (interactive)
     (call-interactively 'eval-region)
     (evil-normal-state))

(defun my-ess-eval ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
          (call-interactively 'ess-eval-line-and-step)))

(defun my-py-send (start end)
  "Send the region delimited by START and END to inferior Python process."
  (interactive "r")
  (python-shell-send-string
   (buffer-substring start end)
   ;; No need to send blank lines in ipython? 2013-12-22
   ;; (concat
   ;;  (let ((line-num (line-number-at-pos start)))
   ;;    ;; When sending a region, add blank lines for non sent code so
   ;;    ;; backtraces remain correct.
   ;;    (make-string (1- line-num) ?\n))
   ;;  (buffer-substring start end))
      nil))
(defun python-eval-region-or-line ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (my-py-send beg end)
    (if (region-active-p)
      (deactivate-mark)
      (next-line 1))))

(setq vc-follow-symlinks 't)

; Persistent undo!
(setq undo-tree-auto-save-history t)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)
(setq undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo")))

  (require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

; Jump to first error
(setq compilation-scroll-output 'first-error)

; Console output scrolling.
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(defun my-dirtrack-mode ()
  "Add to shell-mode-hook to use dirtrack mode in my shell buffers."
    (setq ansi-color-for-comint-mode t)
    (shell-dirtrack-mode 0)
    (set-variable 'dirtrack-list '("^.+[^ ]+:\\(.+?\\) *\\$" 1 nil))
    (dirtrack-mode 1))
(add-hook 'shell-mode-hook 'my-dirtrack-mode)
(setq explicit-shell-file-name "/bin/bash")

(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(add-hook 'compilation-filter-hook 'comint-truncate-buffer)
(setq comint-buffer-maximum-size 2000)

(defun my-find-directory (dir)
  (with-temp-buffer
    (add-to-recentd dir)
    (cd dir)
    (shell)
    (rename-uniquely)))
(setq find-directory-functions '(my-find-directory))

(defun narrow-or-widen-dwim (p)
  (interactive "P")
  (declare (interactive-only))
  (cond
        ((region-active-p)
         (progn
           (narrow-to-region (region-beginning) (region-end))
           (evil-exit-visual-state)))
        (t (widen))))

(provide 'my-misc)
