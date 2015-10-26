(setq magit-last-seen-setup-instructions "1.4.0")

(setq indent-line-function 'insert-tab)
(setq large-file-warning-threshold 100000000)

(setq ring-bell-function 'ignore)

(setq debug-on-quit nil)

(fset 'yes-or-no-p 'y-or-n-p)

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
;; (global-auto-revert-mode)

; Only backup locally
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
; Don't use builtin autosave.
(setq auto-save-default nil)
(require 'persistent-scratch)
(setq persistent-scratch-autosave-interval 15)
(persistent-scratch-autosave-mode t)
(persistent-scratch-setup-default)

(require 'smooth-scrolling)
(setq smooth-scroll-margin 10)

(require 'tramp)
(setq tramp-verbose 3)
(setq vc-handled-backends nil)
(setq vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))
(setq tramp-default-method "ssh")
(setq tramp-remote-path '(tramp-own-remote-path))
(add-to-list 'tramp-default-proxies-alist
             '("\\`mtl\\'" "\\`root\\'" "/ssh:%h:")
             )
(setq tramp-ssh-controlmaster-options
      "-o ControlPath=/tmp/%%r@%%h:%%p -o ControlMaster=auto -o ControlPersist=no")
;; (setq debug-on-quit nil)

(defun open-sudo ()
  (interactive)
  (let ((new-dir
         (if (file-remote-p default-directory)
             (s-replace "/ssh" "/sudo" default-directory)
           (s-concat "/sudo:localhost:" default-directory))))
    (helm-find-files-1 new-dir)))

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
(setq explicit-shell-file-name "/bin/bash")

(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(add-hook 'compilation-filter-hook 'comint-truncate-buffer)
(setq comint-buffer-maximum-size 2000)
(setq comint-input-ring-size 5000)
(defun my-comint-preoutput-turn-buffer-read-only (text)
  (propertize text 'read-only t))

(add-hook 'comint-preoutput-filter-functions 'my-comint-preoutput-turn-buffer-read-only)

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
