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
  (let* ((candidate default-directory)
         (buf (helm-basename candidate))
         (host (file-remote-p candidate 'host))
         (remote-path (format "/%s:%s:%s"
                              helm-su-or-sudo
                              (or host "")
                              (expand-file-name
                               (if host
                                   (file-remote-p candidate 'localname)
                                 candidate))))
         non-essential)
    (if (buffer-live-p (get-buffer buf))
        (progn
          (set-buffer buf)
          (find-alternate-file remote-path))
      (helm-find-files-1 remote-path))))

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
