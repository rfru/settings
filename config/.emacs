(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar starter-kit-packages
  (list 'evil
        'auto-complete
        'pos-tip
        'ace-jump-mode
        'expand-region
        'pcre2el
        'magit
        'web-mode
        'flycheck
        'jedi
        'solarized-theme
        'go-autocomplete
        'auto-save-buffers-enhanced
        'helm-ls-git
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
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

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

(load-theme 'solarized-light t)
;(setq noctilux-broken-srgb nil)
;(load-theme 'noctilux t)

(set-face-attribute 'default nil :family "Consolas")
(set-face-attribute 'default nil :height 140)
(set-fontset-font "fontset-default"
                  'unicode
                  '("Consolas" . "iso10646-1"))
(setq-default line-spacing 6)
(add-to-list 'default-frame-alist '(width  . 125))
(add-to-list 'default-frame-alist '(height . 50))

(tool-bar-mode -1)
(menu-bar-mode -1)

(setq-default indent-tabs-mode nil)
(setq evil-shift-width 2)
(setq-default tab-width 2)
(setq c-basic-offset 2)
(setq css-indent-offset 2)
(setq indent-line-function 'insert-tab)
(savehist-mode 1)
(global-set-key (kbd "C-x C-f") 'find-file-other-window)
(setq column-number-mode t)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))
(global-set-key "\C-xk" 'kill-this-buffer)

; Enables tooltip help
(require 'pos-tip)

(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 1.5)
(auto-save-buffers-enhanced t)

(defvar my-pretty-symbols
  (let ((c-like '(c-mode c++-mode go-mode java-mode js-mode
                        perl-mode cperl-mode ruby-mode
                        python-mode inferior-python-mode)))

  `((?ƒ mine "\\<defun\\>" (emacs-lisp-mode))
    (?ƒ mine "\\<func\\>" (go-mode))
    (?ƒ mine "\\<def\\>" (python-mode))
    (?Ṫ mine "\\<true\\>" (java-mode js-mode))
    (?Ṫ mine "\\<True\\>" (python-mode))
    (?Ḟ mine "\\<false\\>" (java-mode js-mode))
    (?Ḟ mine "\\<False\\>" (python-mode))
    (?Ø mine "\\<void\\>" (java-mode))
    (?Ø mine "\\<null\\>" (java-mode js-mode))
    (?Ø mine "\\<nil\\>" (emacs-lisp-mode go-mode))
    (?Ø mine "\\<None\\>" (python-mode))
    (?Ǝ mine "\\<defvar\\>" (emacs-lisp-mode))
    (?Ǝ mine "\\<var\\>" (js-mode go-mode))
    (?← mine "\\<return\\>" (java-mode js-mode go-mode python-mode))
    (?→ mine "\\<require\\>" (emacs-lisp-mode))
    (?→ mine "\\<import\\>" (java-mode go-mode python-mode))
    (?₀ mine "\\[0\\]" (,@c-like))
    (?₁ mine "\\[1\\]" (,@c-like))
    (?₂ mine "\\[2\\]" (,@c-like))
    (?₃ mine "\\[3\\]" (,@c-like))
    (?₄ mine "\\[4\\]" (,@c-like))
    (?₅ mine "\\[5\\]" (,@c-like))
    (?₆ mine "\\[6\\]" (,@c-like))
    (?₇ mine "\\[7\\]" (,@c-like))
    (?₈ mine "\\[8\\]" (,@c-like))
    (?₉ mine "\\[9\\]" (,@c-like))
    ;(?¬ logical ,(rxt-pcre-to-elisp "\\((!)") (java-mode js-mode go-mode))
    )))

(setq pretty-symbol-patterns (append pretty-symbol-patterns my-pretty-symbols))
(setq pretty-symbol-categories '(lambda relational mine))
(add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode)
(add-hook 'java-mode-hook 'pretty-symbols-mode)
(add-hook 'js-mode-hook 'pretty-symbols-mode)
(add-hook 'python-mode-hook 'pretty-symbols-mode)
(add-hook 'go-mode-hook 'pretty-symbols-mode)
; Highlight current line
(global-hl-line-mode 1)
(define-key global-map (kbd "RET") 'newline-and-indent)

; setup evil
(require 'evil)
(evil-mode 1)
(define-key evil-motion-state-map ";" 'evil-ex)
(evil-ex-define-cmd "bd[elete]" 'kill-this-buffer)

(require 'multiple-cursors)
(define-key evil-normal-state-map (kbd "m") 'mc/mark-next-like-this)
(define-key evil-visual-state-map (kbd "m") 'mc/mark-next-like-this)
(define-key evil-normal-state-map (kbd "M") 'mc/unmark-next-like-this)
(define-key evil-visual-state-map (kbd "M") 'mc/unmark-next-like-this)

(defun last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(define-key evil-normal-state-map (kbd "<tab>") 'last-buffer)

;; esc quits
;; Don't wait for any other keys after escape is pressed.
(setq evil-esc-delay 0)
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(require 'saveplace)
(setq-default save-place t)

; Spell check.
;(add-hook 'text-mode-hook 'flyspell-mode)
;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(require 'expand-region)
(define-key evil-normal-state-map (kbd "e") 'er/expand-region)
(define-key evil-visual-state-map (kbd "e") 'er/expand-region)
(define-key evil-normal-state-map (kbd "E") 'er/contract-region)
(define-key evil-visual-state-map (kbd "E") 'er/contract-region)

; Refresh all buffers periodically.
(global-auto-revert-mode t)

(require 'flycheck)
(setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
(global-flycheck-mode)

(require 'eclim)
;(setq eclim-print-debug-messages t)
(setq eclim-problems-refresh-delay 3)
;; (add-hook 'java-mode-hook (lambda ()
;;                             (when (and buffer-file-name
;;                                      (eclim--accepted-p buffer-file-name)
;;                                      (eclim--project-dir buffer-file-name))
;;                               (eclim-mode 1))))
(add-to-list 'eclim--file-coding-system-mapping '("no-conversion" . "utf8"))
(setq eclim-executable "/Users/mtlin/eclipse/eclim")
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
(defun eclim--project-dir (&optional filename)
   "/usr/local/google/users/mtlin/magicjar/home-citc/magicjar/eclipse")
(global-eclim-mode)


; setup autocompletion
(require 'auto-complete-config)
(require 'go-autocomplete)
(require 'ac-emacs-eclim-source)
(setq-default ac-sources '(ac-source-dictionary ac-source-words-in-same-mode-buffers ac-source-filename))
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(setq ac-auto-start 0)
(setq ac-auto-show-menu t)
(setq ac-quick-help-delay 0.5)
(setq ac-candidate-limit 100)
(add-to-list 'ac-modes 'html-mode)
(add-to-list 'ac-modes 'web-mode)
(setq ac-disable-faces nil)
(global-auto-complete-mode t)

(defun eclim-complete ()
  (interactive)
  (auto-complete '(ac-source-emacs-eclim)))
(define-key evil-insert-state-map (kbd "C-SPC") 'eclim-complete)

; Only autosave/backup locally
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-default nil)

(require 'tramp)
(setq tramp-verbose 3)
;(setq vc-handled-backends '(Git))
(setq vc-handled-backends nil)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(require 'smartparens-config)
(show-smartparens-global-mode nil)
(smartparens-global-mode t)

(require 'helm-config)
(require 'helm)
(require 'helm-buffers)
(require 'helm-files)
(require 'helm-ls-git)
;; (custom-set-faces
;;  '(helm-header ((t (:inherit info-menu-header))))
;;  '(helm-source-header ((t (:inherit info-title-3))))
;;  '(helm-selection ((t (:inherit highlight)))))
(setq helm-M-x-always-save-history t)
(define-key evil-normal-state-map (kbd "t") 'helm-M-x)
(define-key evil-visual-state-map (kbd "t") 'helm-M-x)

(defvar g4-project-regex (rxt-pcre-to-elisp "/google/src/cloud/.+?/(.+?)/.+$"))
(defun is-g4-project (file) (string-match g4-project-regex file))
(defun g4filter (file proj)
  (let ((matches (string-match g4-project-regex file)))
    (if matches
        (not (equal proj (match-string 1 file)))
      nil)))

(defun myrecents ()
  (if (not (s-blank? myrecents-current-buffer))
      (let ((matched (string-match
                      g4-project-regex
                      myrecents-current-buffer)))
        (if matched
            (let ((proj (match-string 1 myrecents-current-buffer)))
              (remove-if (lambda (file)
                           (g4filter file proj))
                         recentf-list))
          recentf-list))
        recentf-list))

(require 'recentf)
(setq recentf-max-saved-items 100)
(setq recentf-auto-cleanup 'never)
(setq recentf-save-file (expand-file-name "~/.emacs.d/recentf" user-emacs-directory))
(recentf-mode 1)
(defvar myrecents-current-buffer nil)
(defvar helm-source-myrecent
  `((name . "Recentf")
    (init . (lambda ()
              (setq myrecents-current-buffer buffer-file-name)))
    (candidates . myrecents)
    (no-delay-on-input)
    (action . (("Find file" . find-file)))))
(defun my-helm ()
  (interactive)
  (helm-other-buffer
   '(
     ;helm-c-source-ls-git
     helm-source-myrecent
     helm-c-source-buffers-list)
   " *my-helm*"))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                            (define-key evil-normal-state-map (kbd ".") 'eval-last-sexp)))
(define-key evil-normal-state-map (kbd ",") 'my-helm)
(define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)
(helm-mode 1)

(setq vc-follow-symlinks 't)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(define-key evil-normal-state-map (kbd "f") 'ace-jump-mode)

; Persistent undo!
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo")))

(require 'whitespace)
(setq-default whitespace-style '(face empty trailing))
(add-hook 'prog-mode-hook (lambda ()
                            (setq whitespace-line-column 80)
                            (setq whitespace-style '(face lines-tail empty trailing))
                            (whitespace-mode 1)
                            ))
(add-hook 'go-mode-hook (lambda ()
                          (whitespace-mode -1)
                          (setq whitespace-line-column 120)
                          (whitespace-mode 1)
                          ))
(add-hook 'web-mode-hook (lambda () (whitespace-mode -1)))

; Tail the compilation buffer.
(setq compilation-scroll-output t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))

(load "server")
(unless (server-running-p) (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/google/")
(require 'borg-mode)
;(require 'google3-build-mode)
(require 'protobuf-mode)

(define-derived-mode piccolo-mode python-mode "Piccolo"
  "python-mode variant for Piccolo")

(add-to-list 'ac-modes 'protobuf-mode)
;(add-to-list 'ac-modes 'google3-build-mode)
(add-to-list 'ac-modes 'borg-mode)
(add-to-list 'ac-modes 'mendel-mode)
(setq auto-mode-alist
      (nconc
       (list
        ;; GSLB configuration files
        (cons "/gslb\\.part\\.cfg$" 'gslb-mode)
        ;; .py files belong in Python mode
        (cons "\\.py$" 'python-mode)
        (cons "\\.pi$" 'piccolo-mode)
        ;; Beads files piggy-back on python-mode.
        (cons "\\.cnf$" 'python-mode)
        ;; MPM package definition files are Python syntax.
        (cons "/pkgdef$" 'python-mode)
        (cons "/BUILD$" 'borg-mode)
        (cons "\\.proto$" 'protobuf-mode)
        (cons "\\.protodevel$" 'protobuf-mode)
        (cons "\\.ncl$" 'ncl-mode)
        ;; .blueprint and .itcnf (Guitar config) files are NCL files.
        (cons "\\.blueprint$" 'ncl-mode)
        (cons "\\.itcnf$" 'ncl-mode)
        ;; SWIG files are made mostly of C++.
        (cons "\\.swig$" 'c++-mode)
        ;; gxp files are just xml files with largely html tags. Some
        ;; people find html-mode annoying (e.g. typing < gives &lt),
        ;; so we map .gxp to whatever mode is used by .html files
        (cons "\\.gxp$"
              (or (cdr (assoc "\\.html$" auto-mode-alist))
                 'html-mode))
        ;; We give our makefiles names like Makefile.foo
        (cons "/Makefile" 'makefile-mode)
        ;; Edit XML files in XML mode (rather than the default SGML
        ;; mode)
        (cons "\\.xml$" 'xml-mode)
        ;; Edit CTemplate files with tpl-mode.
        (cons "\\.tpl$" 'tpl-mode)
        ;; Mendel files
        (cons "mendel/.*\\.gcl$" 'mendel-mode)
        (cons "gws.*\\.gcl$" 'mendel-mode)
        ;; Various Borg-related files
        (cons "\\.bcl$" 'borg-mode)
        (cons "\\.borg$" 'borg-mode)
        (cons "\\.btcfg$" 'borg-mode)
        (cons "\\.gcl$" 'borg-mode)
        (cons "\\.gclx$" 'borg-mode)
        (cons "\\.pp$" 'borg-patchpanel-mode)
        (cons "\\.cfg$" 'borgmon-mode)
        (cons "\\.rules$" 'borgmon-mode)
        (cons "\\.probe$" 'borgmon-mode)
        (cons "\\.alert$" 'borgmon-mode)
        ;; borgmon test just for files under testdata dir
        (cons "google3/production/monitoring/.*/testdata/.*\\.data$"
              'borgmon-test-mode)
        ;; Sawzall
        ;(cons "\\.szl$" 'sawzall-mode)
        ;; Scraper
        (cons "\\.scraper$" 'scraper-mode)
        ;; ActionScript
        (cons "\\.as$" 'actionscript-mode)
        ;; Go language
        (cons "\\.go$" 'go-mode)
        ;; Megastore Definition Language
        (cons "\\.mdl$" 'sql-mode)
        ;; GYP
        (cons "\\.gypi?$" 'gyp-mode)
        ;; Text protobuf data, traditionally called ASCII before Proto2,
        ;; including Bigtable schemas. (Not to be confused with protobuf-mode
        ;; for protocol buffer definitions.)
        (cons "\\.ascii[-_]?pb$" 'protobuffer-mode)
        (cons "\\.ascii[-_]?proto$" 'protobuffer-mode)
        (cons "\\.pb[-_]?ascii$" 'protobuffer-mode)
        (cons "\\.pb[-_]?te?xt$" 'protobuffer-mode)
        (cons "\\.proto[-_]?ascii$" 'protobuffer-mode)
        (cons "\\.proto[-_]?te?xt$" 'protobuffer-mode)
        (cons "\\.te?xt[-_]?pb$" 'protobuffer-mode)
        (cons "\\.te?xt[-_]?proto$" 'protobuffer-mode)
        (cons "\\.btschema$" 'protobuffer-mode)
        (cons "\\.repoconfig$" 'protobuffer-mode)
        (cons "/METADATA$" 'protobuffer-mode)
        ;; Rosy service definitions.
        (cons "\\.rosy$" 'protobuf-mode)
        ;; Dabba configurations
        (cons "\\.dabba$" 'dabba-mode)
        ;; Spanner SDL files
        (cons "\\.sdl$" 'spansdl-mode)
        ;; GFE urlmap files
        (cons "/urlmap\\.production.*" 'urlmap-mode)
        (cons "/urlmap\\.any.*" 'urlmap-mode)
        (cons "/urlmap\\.part.*" 'urlmap-mode)
        (cons "/urlmap\\.test.*" 'urlmap-mode)
        )
       auto-mode-alist))

;; (require 'cl)
;; (add-to-list 'tramp-remote-path "/usr/lib/google-golang/bin")
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; (defun maybe-add-tramp (file)
;;   (if (tramp-tramp-file-p default-directory)
;;       (let ((vec (tramp-dissect-file-name default-directory)))
;;             (tramp-make-tramp-file-name
;;              (tramp-file-name-method vec)
;;              (tramp-file-name-user vec)
;;              (tramp-file-name-host vec)
;;              file))
;;     file))

;; (defun maybe-strip-tramp-regex (str)
;;   (replace-regexp-in-string "/scpc.?:.+?:" "" str))
;; (defun maybe-strip-tramp (file)
;;   (if (tramp-tramp-file-p file)
;;       (tramp-file-name-localname
;;        (tramp-dissect-file-name (replace-regexp-in-string "\\\\" "" file)))
;;       file))
;; (defadvice call-process
;;   (around tramp-advice-process-file activate)
;;   "Invoke `tramp-sh-handle-process-file' for Tramp files."
;;   (if (eq (tramp-find-foreign-file-name-handler default-directory)
;;           'tramp-sh-file-name-handler)
;;         (setq ad-return-value
;;               (apply 'process-file
;;                      (append (subseq (ad-get-args 0) 0 4)
;;                              (mapcar 'maybe-strip-tramp (ad-get-args 4)))))
;;     ad-do-it))
;; (defadvice call-process-region
;;   (around tramp-advice-call-process-region activate)
;;   "Invoke `tramp-sh-handle-call-process-region' for Tramp files."
;;   (if (eq (tramp-find-foreign-file-name-handler default-directory)
;;           'tramp-sh-file-name-handler)
;;       (setq ad-return-value
;;             (apply 'tramp-sh-handle-call-process-region
;;                    (append (subseq (ad-get-args 0) 0 6)
;;                            (mapcar 'maybe-strip-tramp (ad-get-args 6)))))
;;     ad-do-it))
;; (defadvice start-process
;;   (around tramp-advice-start-process activate)
;;   "Invoke `tramp-sh-handle-start-file-process' for Tramp files."
;;   (if (eq (tramp-find-foreign-file-name-handler default-directory)
;;           'tramp-sh-file-name-handler)
;;       (setq ad-return-value (apply 'start-file-process (ad-get-args 0)))
;;     ad-do-it))

;; (defadvice shell-command-to-string
;;   (around tramp-advice-shell-command-to-string activate)
;;   (if (eq (tramp-find-foreign-file-name-handler default-directory)
;;           'tramp-sh-file-name-handler)
;;       (progn
;;         (ad-set-arg 0 (maybe-strip-tramp-regex (ad-get-arg 0)))
;;         ad-do-it)
;;       ad-do-it))

;; (defadvice start-process-shell-command
;;   (around tramp-advice-start-process-shell-command activate)
;;   "Invoke `tramp-sh-handle-shell-command' for Tramp files."
;;   (if (eq (tramp-find-foreign-file-name-handler default-directory)
;;           'tramp-sh-file-name-handler)
;;       (with-parsed-tramp-file-name default-directory nil
;;         (let ((shell-file-name
;;                (tramp-get-connection-property v "remote-shell" "/bin/sh"))
;;               (shell-command-switch "-c"))
;;           ad-do-it))
;;     ad-do-it))


;; (defun glaze-hook ()
;;   (when (is-g4-project (maybe-strip-tramp buffer-file-name))
;;     (start-file-process "glaze" nil "glaze" (maybe-strip-tramp default-directory))))
;; (add-hook 'go-mode-hook
;;           (lambda ()
;;             ;(add-hook 'before-save-hook
;;              ;         (lambda ()
;;              ;           (let ((temporary-file-directory (concat (file-remote-p default-directory) "/tmp"))) (gofmt))) 'nil make-it-local)
;;             (add-hook 'after-save-hook 'glaze-hook nil 'make-it-local)
;;             (start-file-process "gocode libs" nil "gocode_set_lib" (maybe-strip-tramp default-directory))))

;; (defun eclim/workspace-dir ()
;;   (concat "/dev:" (eclim--call-process "workspace_dir")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
