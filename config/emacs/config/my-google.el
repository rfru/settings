(load-file "/usr/share/emacs/site-lisp/google/google.el")
(require 'google)
(add-to-list 'ac-modes 'protobuf-mode)
(add-to-list 'ac-modes 'google3-build-mode)
(add-to-list 'ac-modes 'borg-mode)
(add-to-list 'ac-modes 'mendel-mode)

(provide 'my-google)
