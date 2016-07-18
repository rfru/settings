(require 'tramp)
(require 'tramp-sh)

(load "server")

(setq server-use-tcp t
      server-port    9999)
(unless (server-running-p) (server-start))

; For client buffers (emacsclient) don't confirm
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(defun put-alist (key value alist)
  "Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST."
  (let ((elm (assoc key alist)))
    (if elm
        (progn
          (setcdr elm value)
          alist)
      (cons (cons key value) alist))))

(defun update-tramp-emacs-server-port-forward (method-name)
  "Update the specified TRAMP's method to forward the Emacs
 server port to the local host. This lets emacsclient on the
 remote host open files in the local Emacs server."
  (let* ((method (assoc method-name tramp-methods))
         (ssh-args (cadr (assoc 'tramp-login-args method))))
    (put-alist 'tramp-login-args
      (list (put-alist "-R" (let ((port
                                   (process-contact server-process :service)))
        ;; put-alist makes a dotted pair for the key/value, but tramp-methods
        ;; needs a normal list, so put the value inside a list so that the
        ;; second part of the dotted pair (ie the cdr) is a list, which
        ;; converts it from a dotted pair into a normal list.
                              (list (format "%s:127.0.0.1:%s" port port)))
                       ssh-args))
      method)))

(defun tramp-make-tramp-file-name-from-vec (vec file)
  "Convenience function for making a TRAMP path, since this
apparently didn't already exist."
  (tramp-make-tramp-file-name
    (tramp-file-name-method vec)
    (tramp-file-name-user vec)
    (tramp-file-name-host vec)
    file))

(setq auth-copied '())
(defun copy-auth ()
  (interactive)
  (let* ((dissected (tramp-dissect-file-name default-directory))
         (host (tramp-file-name-host dissected))
         (emacs-dir (tramp-make-tramp-file-name-from-vec dissected "~/.emacs.d/"))
         (file (tramp-make-tramp-file-name-from-vec dissected "~/.emacs.d/remote-server")))
    (when (not (-contains? auth-copied host))
      (add-to-list 'auth-copied host)
      (unless (file-exists-p emacs-dir)
        (make-directory emacs-dir))
      (copy-file "~/.emacs.d/server/server" file t))))

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
(update-tramp-emacs-server-port-forward tramp-default-method)
(setq tramp-remote-path '(tramp-own-remote-path))
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))
(setq tramp-ssh-controlmaster-options
      "-o ControlPath=/tmp/%%r@%%h:%%p -o ControlMaster=auto -o ControlPersist=no")

(provide 'my-tramp)
