(require 'company-emacs-eclim)
(require 'company-dabbrev)
(require 'company-go)
(company-emacs-eclim-setup)
(add-to-list 'company-backends 'company-ghc)
(add-to-list 'company-backends 'company-jedi)
(add-to-list 'company-backends 'company-go)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-other-buffers t)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(delete 'company-files company-backends)

(setq company-global-modes
      '(not shell-mode comint-mode))

(provide 'my-completion)
