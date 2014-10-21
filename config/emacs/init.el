(defvar my-config-dir
	(expand-file-name "config" (file-name-directory (file-truename load-file-name))))
(add-to-list 'load-path my-config-dir)

(require 'my-package)
(require 'my-util)
(require 'my-motions)
(require 'my-helm)
(require 'my-prog)
(require 'my-completion)
(require 'my-misc)
(require 'my-google)

(require 'my-ui)
(require 'my-theme)

(load "server")
(unless (server-running-p) (server-start))
