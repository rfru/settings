(require 'helm-config)
(require 'helm)
(require 'helm-buffers)
(require 'helm-files)
(require 'recentf)
(require 'dash)
(require 's)
(require 'f)
(require 'helm-swoop)

(setq helm-mp-matching-method 'multi3)
(setq helm-mp-highlight-delay 0.1)
(setq helm-mp-highlight-threshold 1)
(setq helm-M-x-always-save-history t)
(setq helm-split-window-default-side 'other)
(setq helm-quick-update t)

(setq recentf-exclude '("\\.recentf" "\\.recentd" "^/tmp/" "/.git/" "/.emacs.d/elpa/"))
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
   diff))

(setq my-source-recentf
      (helm-build-sync-source "Recent"
        :init ((lambda ()
                 (recentf-mode 1)))
        :candidates recentf-list
        :candidate-number-limit 100
        :filtered-candidate-transformer 'my-filter
        :keymap helm-generic-files-map
        :action (helm-actions-from-type-file)))

(defun buffers-attrs (buffer)
  (let* ((buf (get-buffer buffer))
         (name (buffer-name buf))
         (file (buffer-file-name buf))
         (dir (with-current-buffer buffer default-directory))
         (proc (get-buffer-process buf)))
    (list
     (cond
      ((not file) (propertize name 'face 'italic))
      (t name)))))

(defun buffers-transformer (buffers _source)
      (cl-loop for i in buffers
        for (name) = (buffers-attrs i)
        collect (cons name i)))

(defun shell-buffers ()
  ; Hack to get the other non-helm buffer
  (let* ((this-name (buffer-name (nth 1 (buffer-list))))
         (shells
     (-filter (lambda (b)
                (and (not (s-equals? (buffer-name b) this-name))
                     (s-starts-with? "*shell"
                                     (buffer-name b)))) (buffer-list))))
    (-map (lambda (b)
            (cons (with-current-buffer b default-directory) b)) shells)))

(defun no-shells ()
  (-filter (lambda (b)
             (let ((name (buffer-name b)))
                (not (s-starts-with? "*shell" name)))) (buffer-list)))
;; (defun no-shells ()
;;   (-filter (lambda (b)
;;              (let ((name (buffer-name b)))
;;                (and
;;                 (s-starts-with? "*" name)
;;                 (not (s-starts-with? "*shell" name))))) (buffer-list)))

(setq my-source-shells
      (helm-build-sync-source "Shells"
        :candidates 'shell-buffers
        :candidate-number-limit 25
        :action 'switch-to-buffer))

(setq my-source-buffers
      (helm-build-sync-source "Buffers"
        :candidates (lambda ()  (-map 'buffer-name (no-shells)))
        :candidate-number-limit 15
        :filtered-candidate-transformer '(helm-skip-boring-buffers buffers-transformer)
        :action 'helm-buffers-list-persistent-action))

(setq helm-for-files-preferred-list
      '(my-source-shells
        my-source-buffers
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
         (pruned-history (helm-fast-remove-dups history
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
    (helm-do-ag default-directory)))

(define-key helm-map [escape] 'helm-keyboard-quit)

(setq helm-candidate-number-limit 25)
(setq kill-ring-max 500)

(helm-mode 1)

(provide 'my-helm)
