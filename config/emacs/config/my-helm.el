(require 'helm-config)
(require 'helm)
(require 'helm-buffers)
(require 'helm-files)
(require 'recentf)
(require 'dash)
(require 's)
(require 'f)
(require 'helm-swoop)

(setq helm-mp-matching-method 'multi3p)
(setq helm-mp-highlight-delay 0.1)
(setq helm-M-x-always-save-history t)
(setq helm-split-window-default-side 'other)
(setq helm-quick-update t)

(setq recentf-exclude '("\\.recentf" "^/tmp/" "/.git/" "/.emacs.d/elpa/"))
(setq recentf-max-saved-items 250)
(setq recentf-auto-cleanup 'never)
(setq recentf-save-file (expand-file-name "~/.emacs.d/.recentf"))
(setq recentf-auto-save-timer
      (run-with-idle-timer 10 t 'recentf-save-list))
(recentf-mode 1)

(defun my-filter (candidates _source)
(let* ((expanded default-directory)
      (open-buffers (-non-nil (-map 'buffer-file-name (buffer-list))))
      (diff (-difference candidates open-buffers)))
  (-map
   (lambda (file)
     (s-chop-prefix expanded file))
   diff)))
(defvar my-source-recentf
  `((name . "Recent")
    (init . (lambda ()
              (recentf-mode 1)))
    (candidates . recentf-list)
    (match . helm-files-match-only-basename)
    (filtered-candidate-transformer . my-filter)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (action . ,(cdr (helm-get-actions-from-type
                     helm-source-locate))))
  "See (info \"(emacs)File Conveniences\").
Set `recentf-max-saved-items' to a bigger value if default is too small.")

(setq helm-for-files-preferred-list
      '(helm-source-buffers-list
        my-source-recentf))

(setq recentd-file (expand-file-name "~/.emacs.d/.recentd"))
(setq recentd-max 50)
(defun add-to-recentd (d)
  (let ((tmp (helm-fast-remove-dups
              (append (list (file-name-as-directory (expand-file-name d))) recentd-list)
              :test 'equal)))
    (setq recentd-list
          (if (>= (length tmp) recentd-max)
              (cl-subseq tmp 0 recentd-max)
            tmp))))
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
   (-map 'file-name-directory recentf-list)))
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
  (if (and (eq major-mode 'shell-mode) (not (get-buffer-process (current-buffer))))
      (shell)
      ; Find files.
      (let ((dir (my-history)))
        (when (file-exists-p dir)
          (add-to-recentd dir))
        (helm-find-files-1 (expand-file-name dir) nil))))

(setq helm-swoop-split-direction 'split-window-horizontally)

(defun search()
  (interactive)
  (if (file-remote-p default-directory)
      (helm-ag)
    (helm-do-ag)))

(define-key helm-map [escape] 'helm-keyboard-quit)

(setq helm-candidate-number-limit 25)
(setq helm-buffer-max-length 75)
(helm-mode 1)

(provide 'my-helm)
