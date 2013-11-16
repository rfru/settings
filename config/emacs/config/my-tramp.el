;; (require 'cl)
;; (add-to-list 'tramp-remote-path "/usr/lib/google-golang/bin")
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; (defun maybe-add-tramp (file)
;;   (if (tramp-tramp-file-p default-directory)
;;       (let ((vec (tramp-dissect-file-name default-directory)))
;;             (tramp-make-tramp-file-name
;;              (tramp-file-name-method vec)
;;              (tramp-file-name-user vec)
;;              (tramp-file-name-host vec)
;;              file))
;;     file))

;; (defun maybe-strip-tramp-regex (str)
;;   (replace-regexp-in-string "/scpc.?:.+?:" "" str))
;; (defun maybe-strip-tramp (file)
;;   (if (tramp-tramp-file-p file)
;;       (tramp-file-name-localname
;;        (tramp-dissect-file-name (replace-regexp-in-string "\\\\" "" file)))
;;       file))
;; (defadvice call-process
;;   (around tramp-advice-process-file activate)
;;   "Invoke `tramp-sh-handle-process-file' for Tramp files."
;;   (if (eq (tramp-find-foreign-file-name-handler default-directory)
;;           'tramp-sh-file-name-handler)
;;         (setq ad-return-value
;;               (apply 'process-file
;;                      (append (subseq (ad-get-args 0) 0 4)
;;                              (mapcar 'maybe-strip-tramp (ad-get-args 4)))))
;;     ad-do-it))
;; (defadvice call-process-region
;;   (around tramp-advice-call-process-region activate)
;;   "Invoke `tramp-sh-handle-call-process-region' for Tramp files."
;;   (if (eq (tramp-find-foreign-file-name-handler default-directory)
;;           'tramp-sh-file-name-handler)
;;       (setq ad-return-value
;;             (apply 'tramp-sh-handle-call-process-region
;;                    (append (subseq (ad-get-args 0) 0 6)
;;                            (mapcar 'maybe-strip-tramp (ad-get-args 6)))))
;;     ad-do-it))
;; (defadvice start-process
;;   (around tramp-advice-start-process activate)
;;   "Invoke `tramp-sh-handle-start-file-process' for Tramp files."
;;   (if (eq (tramp-find-foreign-file-name-handler default-directory)
;;           'tramp-sh-file-name-handler)
;;       (setq ad-return-value (apply 'start-file-process (ad-get-args 0)))
;;     ad-do-it))

;; (defadvice shell-command-to-string
;;   (around tramp-advice-shell-command-to-string activate)
;;   (if (eq (tramp-find-foreign-file-name-handler default-directory)
;;           'tramp-sh-file-name-handler)
;;       (progn
;;         (ad-set-arg 0 (maybe-strip-tramp-regex (ad-get-arg 0)))
;;         ad-do-it)
;;       ad-do-it))

;; (defadvice start-process-shell-command
;;   (around tramp-advice-start-process-shell-command activate)
;;   "Invoke `tramp-sh-handle-shell-command' for Tramp files."
;;   (if (eq (tramp-find-foreign-file-name-handler default-directory)
;;           'tramp-sh-file-name-handler)
;;       (with-parsed-tramp-file-name default-directory nil
;;         (let ((shell-file-name
;;                (tramp-get-connection-property v "remote-shell" "/bin/sh"))
;;               (shell-command-switch "-c"))
;;           ad-do-it))
;;     ad-do-it))


;; (defun glaze-hook ()
;;   (when (is-g4-project (maybe-strip-tramp buffer-file-name))
;;     (start-file-process "glaze" nil "glaze" (maybe-strip-tramp default-directory))))
;; (add-hook 'go-mode-hook
;;           (lambda ()
;;             ;(add-hook 'before-save-hook
;;              ;         (lambda ()
;;              ;           (let ((temporary-file-directory (concat (file-remote-p default-directory) "/tmp"))) (gofmt))) 'nil make-it-local)
;;             (add-hook 'after-save-hook 'glaze-hook nil 'make-it-local)
;;             (start-file-process "gocode libs" nil "gocode_set_lib" (maybe-strip-tramp default-directory))))
