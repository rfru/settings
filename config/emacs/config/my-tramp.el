(require 'tramp)
(require 'tramp-sh)

(load "server")

(setq server-use-tcp t
      server-port    9999)
(unless (server-running-p) (server-start))

; For client buffers (emacsclient) don't confirm
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(defun open-sudo ()
  (interactive)
  (let ((new-dir
         (if (file-remote-p default-directory)
             (let* ((ds (tramp-dissect-file-name default-directory))
                   (prefix (substring (tramp-make-tramp-file-name
                    "ssh"
                    (tramp-file-name-user ds)
                    (tramp-file-name-host ds) nil) 0 -1)))
               (progn
                 (message (s-concat prefix (s-replace "/ssh" "|sudo" default-directory)))
                 (s-concat prefix (s-replace "/ssh" "|sudo" default-directory))))
           (s-concat "/sudo:localhost:" default-directory))))
    (helm-find-files-1 new-dir)))

(setq tramp-verbose 0)
(setq vc-handled-backends nil)
(setq vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))
(setq tramp-default-method "ssh")
(add-hook 'shell-mode-hook  'with-editor-export-editor)
(setq tramp-remote-path '(tramp-own-remote-path))
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))
(setq tramp-ssh-controlmaster-options
      "-o ControlPath=/tmp/%%r@%%h:%%p -o ControlMaster=auto -o ControlPersist=no")

(provide 'my-tramp)
