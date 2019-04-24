(require 'multi-term)
(require 'tramp)

;;; derived from http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
;;; stolen from http://github.com/tavisrudd/emacs.d
(defun multi-term-ext-setup-tramp (host)
  "Setup ansi-term/tramp remote directory tracking
   NOTE:  this appears to have some sort of timing bug in it and doesn't always work"
  (term-send-raw-string
   (concat "
function eterm_set_variables {
    echo -e \"\\033AnSiTu\" $LOGNAME
    echo -e \"\\033AnSiTh\" " host "
    echo -e \"\\033AnSiTc\" $(pwd)
}
function eterm_tramp_init {
    for temp in cd pushd popd; do
        alias $temp=\"eterm_set_cwd $temp\"
    done
    # set hostname, user, and cwd now
    eterm_set_variables
}
function eterm_set_cwd {
    $@
    eterm_set_variables
}
eterm_tramp_init
export -f eterm_tramp_init
export -f eterm_set_variables
export -f eterm_set_cwd
clear
")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multi-term-remote (&optional user+host)
  "Creates a multi-term in a remote host. A user + host (e.g
user@host) value will be required to perform the connection."
  (interactive)
  (let* ((user+host (or user+host
                        (read-from-minibuffer "SSH address (e.g user@host): ")))
         (multi-term-program "ssh")
         (multi-term-program-switches user+host))
    (progn
      (multi-term)
      (multi-term-ext-setup-tramp user+host))))

(defun multi-term-at-dir (dir)
    (if (tramp-tramp-file-p dir)
        (let* ((d (tramp-dissect-file-name dir))
               (host (tramp-file-name-host d))
               (dir (tramp-file-name-localname d)))
          (progn
            (multi-term-remote host)
            (term-send-raw-string (concat "cd " dir "\nclear\n"))))
        (with-temp-buffer
          (cd (expand-file-name dir))
          (multi-term)
          )))

(defun my-dirtrack-mode ()
  "Add to shell-mode-hook to use dirtrack mode in my shell buffers."
    (setq ansi-color-for-comint-mode t)
    (shell-dirtrack-mode 0)
    (set-variable 'dirtrack-list '("^.+[^ ]+:\\(.+?\\) *\\$" 1 nil))
    (dirtrack-mode 1))
(add-hook 'shell-mode-hook 'my-dirtrack-mode)

(defun tramp-comint-read-input-ring ()
  "Read remote bash_history file into comint input ring."
  (when (file-remote-p default-directory)
    (setq-local comint-input-ring-file-name
                (format "%s%s/.bash_history"
                        (replace-regexp-in-string (rxt-pcre-to-elisp "^(.+:/).*?$") "\\1" default-directory)
                        (-last-item (s-lines (s-trim (shell-command-to-string "/bin/echo $HOME"))))))
    (comint-read-input-ring)))

(add-hook 'shell-mode-hook 'tramp-comint-read-input-ring)
; Tramp sets HISTFILE to /dev/null so bash history on remote shells does not work.
(add-to-list 'tramp-remote-process-environment "HISTFILE=")
(setq explicit-bash-args
      '("--noediting"
        "-i"
        "-c"
        "export PROMPT_COMMAND=\"history -a; $PROMPT_COMMAND\"; function e { if [ $# -eq 0 ] || [[ ! -f $1 ]]; then return; fi; eval \"$EDITOR\" \"$@\"; } ; export -f e; bash"))

(provide 'my-term)
