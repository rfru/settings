(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar starter-kit-packages
  (list 'evil
        'auto-complete
        'pos-tip
        'protobuf-mode
        'diminish
        'ace-jump-mode
        'expand-region
        'ag
        'pcre2el
        'magit
        'web-mode
        'flycheck
        'jedi
        'go-autocomplete
        'auto-save-buffers-enhanced
        's
        'multiple-cursors
        'dash
        'emacs-eclim
        'pretty-symbols
        'go-mode
        'haskell-mode
        'smartparens
        'js2-mode
        'undo-tree ; Automatically loaded by evil.
        'exec-path-from-shell
        'helm)
  "Libraries that should be installed by default.")

(defun starter-kit-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package starter-kit-packages)
    (unless (or (member package package-activated-list)
		(functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))


;; On your first run, this should pull in all the base packages.
(when (not package-archive-contents) (package-refresh-contents))
(starter-kit-elpa-install)

; GUI Emacs needs to be similar to shell init.
;;(when (memq window-system '(mac ns))
;;    (exec-path-from-shell-initialize))
;;(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;;(unless (require 'el-get nil 'noerror)
;;  (with-current-buffer
;;      (url-retrieve-synchronously
;;       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;    (goto-char (point-max))
;;    (eval-print-last-sexp)))

;; (setq el-get-sources
;;       '((:name my-theme
;;                :type git
;;                :url "git://github.com/rfru/solarized-emacs.git"
;;                :load "solarized-theme.el"
;;                :compile ("solarized-theme.el")
;;                :features solarized-theme)))

;; (setq my-el-get-packages
;;       (append
          ; list all regular recipe packages you want installed
;;        '()
;;        (mapcar 'el-get-source-name el-get-sources)))

;; (el-get 'sync my-el-get-packages)
;; (el-get 'sync)

(require 's)
(require 'dash)

(provide 'my-package)
