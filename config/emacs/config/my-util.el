(require 'evil)

(defun my-kill-buffers ()
    "Kill all other buffers."
    (interactive)
    (tramp-cleanup-all-connections)
    (mapc '(lambda (b)
             (evil-delete-buffer-keep-windows b t))
          (remove-if '(lambda (b)
                        (and
                         (not (string-match "\\\\*shell-[0-9]+\\\\*" (buffer-name b)))
                         (not (string= "*compilation*" (buffer-name b)))
                         (not (buffer-file-name b)))) (buffer-list)))
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

(provide 'my-util)
