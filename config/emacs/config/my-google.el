(add-to-list 'load-path "/usr/share/emacs/site-lisp/google")
(when (require 'google nil 'noerror)
 (setq google-show-trailing-whitespace nil)
 (setq google-trailing-whitespace-modes nil)

 (add-to-list 'ac-modes 'protobuf-mode)
 (add-to-list 'ac-modes 'google3-build-mode)
 (add-to-list 'ac-modes 'borg-mode)
 (add-to-list 'ac-modes 'mendel-mode)

 (add-hook 'protobuf-mode 'my-set-whitespace-normal)
 (add-hook 'google3-build-mode 'my-set-whitespace-normal)
 (add-hook 'borg-mode 'my-set-whitespace-normal)
 (add-hook 'mendel-mode-hook 'my-set-whitespace-normal))

(provide 'my-google)
