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

(setq recentf-exclude '("\\.recentf" "\\.recentd" "^/tmp/" "/.git/" "/.emacs.d/elpa/" "^/var/"))
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
        :candidates (lambda () recentf-list)
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
                     (s-matches? (rxt-elisp-to-pcre "^\*(terminal|shell)") (buffer-name b))))
              (buffer-list))))
    (-map (lambda (b)
            (cons (with-current-buffer b default-directory) b)) shells)))

(defun no-shells ()
  (-remove (lambda (b)
             (let ((name (buffer-name b)))
                (s-matches? (rxt-elisp-to-pcre "^\*(terminal|shell)") name)
                )) (buffer-list)))

(setq my-source-shells
      (helm-build-sync-source "Shells"
        :candidates 'shell-buffers
        :candidate-number-limit 25
        :action 'switch-to-buffer))

(add-to-list 'helm-boring-buffer-regexp-list "\\*tramp")
(add-to-list 'helm-boring-buffer-regexp-list "\\*Compile-Log")
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

(defun my-find-directory-shell (dir)
  (with-temp-buffer
    (add-to-recentd dir)
    (cd dir)
    (when (file-remote-p default-directory)
      (copy-auth))
    (shell)
    (rename-uniquely)))

(defun my-find-directory-term (dir)
  (progn
    (add-to-recentd dir)
    (multi-term-at-dir dir)))
(setq find-directory-functions '(my-find-directory-shell))

(defun my-find-directories ()
  (interactive)
  (if (and (eq major-mode 'shell-mode) (not (get-buffer-process (current-buffer))))
      (shell)
      ; Find files.
      (let ((dir (my-history)))
        (progn
          (add-to-recentd dir)
          (helm-find-files-1 dir nil)))))

(setq helm-swoop-split-direction 'split-window-horizontally)

(setq helm-source-comint-input-ring
  '((name . "Comint history")
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (-remove
                       (lambda(str)
                         (or
                          (s-starts-with? "exec env ENV=" str)
                          (s-starts-with? "test -" str)
                          (s-equals? "echo are you awake" str)
                          (s-equals? "exit" str)))
                       (-distinct (ring-elements comint-input-ring))))))
    (action . helm-comint-input-ring-action)))

(defun my-ag-search()
  (interactive)
  (if (file-remote-p default-directory)
      (helm-ag)
    (helm-do-ag default-directory)))

(setq helm-ag-command-option
      (s-join " "(-map '(lambda (x) (s-prepend "--ignore " x)) '("*.pb" "*.log" "*.hdf5" "*.min.js"))))

(defvar helm-source-comint-input-ring-async-blank
  (helm-build-dummy-source
   "Run"
   :action (helm-make-actions
            "Run"
            'helm-comint-input-ring-action-async-command)))

(defun helm-comint-input-ring-action-async-command (candidate)
  (switch-to-buffer (format "*%s*" candidate))
  (rename-uniquely)
  (async-shell-command candidate (current-buffer))
  (setq-local comint-buffer-maximum-size 20000)
  (setq-local comint-prompt-read-only nil)
  (setq-local my-shell-command candidate))

(defvar helm-source-comint-input-ring-async
  '((name . "Comint history")
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (ring-elements comint-input-ring))))
    (action . helm-comint-input-ring-action-async-command))
  "Source that provide helm completion against `comint-input-ring'.")

(defun helm-comint-input-ring-async-command ()
  (interactive)
  (when (derived-mode-p 'comint-mode)
    (helm :sources '(
                     helm-source-comint-input-ring-async
                     helm-source-comint-input-ring-async-blank)
          :input (buffer-substring-no-properties (comint-line-beginning-position)
                                                 (point-at-eol))
          :buffer "*helm comint history*")))

(define-key helm-map [escape] 'helm-keyboard-quit)

(setq helm-candidate-number-limit 25)
(setq kill-ring-max 500)

(setq helm-swoop-speed-or-color nil)


(helm-mode 1)

(provide 'my-helm)
