(require 'helm-config)
(require 'helm)
(require 'helm-buffers)
(require 'helm-files)

(setq helm-mp-matching-method 'multi3p)
(setq helm-mp-highlight-delay 0.1)
(setq helm-M-x-always-save-history t)
(setq helm-split-window-default-side 'other)
(setq helm-quick-update t)
(setq helm-buffer-details-flag nil)
(define-key evil-motion-state-map (kbd "'") 'helm-M-x)
(define-key evil-visual-state-map (kbd "'") 'helm-M-x)

(require 'recentf)
(setq recentf-exclude '("\\.recentf" "^/tmp/" "/.git/" "/.emacs.d/elpa/"))
(setq recentf-max-saved-items 150)
(setq recentf-auto-cleanup 'never)
(setq recentf-save-file (expand-file-name "~/.emacs.d/.recentf"))
(setq recentf-auto-save-timer
      (run-with-idle-timer 10 t 'recentf-save-list))
(recentf-mode 1)

(setq helm-for-files-preferred-list
      '(helm-source-buffers-list
        helm-source-recentf))

(setq recentd-file (expand-file-name "~/.emacs.d/.recentd"))
(defun add-to-recentd (d)
  (setq recentd-list
        (helm-fast-remove-dups
         (append (list (file-name-as-directory (expand-file-name d))) recentd-list)
         :test 'equal)))
(defun recentd-save-list ()
  (dump-vars-to-file '(recentd-list) recentd-file))
(setq recentd-list '())
(when (file-exists-p recentd-file)
    (load-file recentd-file))
(setq recentd-auto-save-timer
      (run-with-idle-timer 10 t 'recentd-save-list))

(defun my-generate-dirs ()
  (append
   (list (expand-file-name default-directory))
   recentd-list
   (mapcar (lambda (f) (file-name-directory f)) recentf-list)))
(cl-defun my-history (&key (comp-read t))
  "The `helm-find-files' history.
Show the first `helm-ff-history-max-length' elements of
`helm-ff-history' in an `helm-comp-read'."
  (let* ((history
          (append
           (my-generate-dirs)
           (if (>= (length helm-ff-history) helm-ff-history-max-length)
               (cl-subseq helm-ff-history 0 helm-ff-history-max-length)
             helm-ff-history)))
         (abbrev-hist
          (mapcar (lambda (dir)
                    (if (string= dir default-directory)
                        "./"
                      (abbreviate-file-name dir)))
                     history))
         (pruned-history (helm-fast-remove-dups abbrev-hist
                                                :test 'equal)))
    (helm-comp-read
     "Switch to Directory: "
     pruned-history
     :name "Directory History")))
(defun my-find-directories ()
  (interactive)
  (let ((dir (my-history)))
        (when (file-exists-p dir)
          (add-to-recentd dir))
        (helm-find-files-1 (expand-file-name dir) nil)))

(require 'helm-swoop)
(setq helm-swoop-split-direction 'split-window-horizontally)
(define-key evil-motion-state-map (kbd ",") 'helm-for-files)
(define-key evil-normal-state-map (kbd ",") 'helm-for-files)
(define-key evil-motion-state-map (kbd "z") 'my-find-directories)
(define-key evil-normal-state-map (kbd "z") 'my-find-directories)
(define-key evil-visual-state-map (kbd "s") 'helm-swoop)
(define-key evil-normal-state-map (kbd "s") (lambda () (interactive) (helm-swoop :$query "")))
(define-key evil-normal-state-map (kbd "f") 'ace-jump-mode)

(defun search()
  (interactive)
  (if (file-remote-p default-directory)
      (helm-ag)
    (helm-do-ag)))
(define-key evil-motion-state-map (kbd "S") 'search)
(define-key evil-normal-state-map (kbd "S") 'search)

(define-key helm-map [escape] 'helm-keyboard-quit)

(setq helm-candidate-number-limit 25)
(setq helm-buffer-max-length 75)
(helm-mode 1)

(provide 'my-helm)
