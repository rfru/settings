(require 'evil)

(defun my-kill-buffers ()
    "Kill all other buffers."
    (interactive)
    ;; (tramp-cleanup-all-connections)
    (mapc '(lambda (b)
             (evil-delete-buffer-keep-windows b t))
          (remove-if '(lambda (b)
                        (and
                         (not (string-match "\\\\*shell\\\\*.*" (buffer-name b)))
                         (not (string-match "\\\\*tramp.+\\\\*" (buffer-name b)))
                         (not (string= "*compilation*" (buffer-name b)))
                         (not (buffer-file-name b))))
                     (buffer-list)))
    (delete-other-windows)
    (switch-to-buffer "*scratch*"))

(defun dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (dump varlist buf)
      (save-buffer)
      (kill-buffer))))

(defun dump (varlist buffer)
  "insert into buffer the setq statement to recreate the variables in VARLIST"
  (loop for var in varlist do
        (print (list 'setq var (list 'quote (symbol-value var)))
               buffer)))

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

(defun my-quit ()
  (interactive)
  (when (and (not (eq 'shell-mode major-mode)) (buffer-modified-p) (not (string-match "^\\*.+?\\*$" (buffer-name))))
    (evil-write nil nil nil buffer-file-name t))
  (evil-delete-buffer-keep-windows (current-buffer) t))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )
(defun hsplit-last-buffer ()
  (interactive)
   (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(defun xah-syntax-color-hex ()
  "Syntax color hex color spec such as 「#ff1100」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

(provide 'my-util)
