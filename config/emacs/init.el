(defvar my-config-dir
	(expand-file-name "config" (file-name-directory (file-truename load-file-name))))
(add-to-list 'load-path my-config-dir)

(require 'my-package)
(require 'my-theme)
(require 'my-motions)
(require 'my-helm)
(require 'my-prog)
(require 'my-completion)
(require 'my-misc)
(require 'my-google)

(load "server")
(unless (server-running-p) (server-start))
