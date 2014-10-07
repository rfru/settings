(add-to-list 'load-path "~/google")

(when (require 'borg-mode nil 'noerror)
  (require 'google3-build-mode)
  (require 'google-coding-style)
 (add-to-list 'ac-modes 'borg-mode)
 (add-to-list 'ac-modes 'mendel-mode)
 (setq-default compile-command "blaze build :all")
  (setq auto-mode-alist
        (nconc
         (list
          (cons "/BUILD$" 'google3-build-mode)
          (cons "mendel/.*\\.gcl$" 'mendel-mode)
          (cons "gws.*\\.gcl$" 'mendel-mode)
          ;; Various Borg-related files
          (cons "\\.bcl$" 'borg-mode)
          (cons "\\.borg$" 'borg-mode)
          (cons "\\.btcfg$" 'borg-mode)
          (cons "\\.gcl$" 'borg-mode)
          (cons "\\.gclx$" 'borg-mode))
         auto-mode-alist))

(defun google-imports-jiwyu ()
  "Run JIWYU (http://go/jiwyu) on the current file.
We assume that the files and the build target are already
editable.  This also saves the current buffer, and will revert it
once JIWYU finishes.
  Caveat: does not revert the associated BUILD file."
  (interactive)
  (unless (eq major-mode 'java-mode)
    (error "JIWYU only works with Java."))
  (let* (
         (temp-originating-buffer-name (current-buffer))
         (original-directory default-directory)
         (jiwyu-buffer-name
          (apply 'make-comint-in-buffer "jiwyu" nil
                 "/google/data/ro/teams/jiwyu/jiwyu" nil
                 (list (file-relative-name buffer-file-name)))))

    (switch-to-buffer jiwyu-buffer-name)
    (setq-local originating-buffer-name temp-originating-buffer-name)
    (setq-local default-directory original-directory)

    (defun after-execution (process event)
      (when (not (eq (process-status process) 'run))
        (let ((cur-buf (current-buffer)))
          (switch-to-buffer originating-buffer-name)
          (revert-buffer nil t t)
          (kill-buffer cur-buf))))

    (set-process-sentinel (get-buffer-process jiwyu-buffer-name) 'after-execution))))

(provide 'my-google)
