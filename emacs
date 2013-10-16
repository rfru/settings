(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar starter-kit-packages
  (list 'evil
        'auto-complete
        'solarized-theme
        'pos-tip
        'ace-jump-mode
        'expand-region
        'pcre2el
        'go-autocomplete
        'dash
        's
        'goto-chg
        'emacs-eclim
        'pretty-symbols-mode
        'go-mode
        'haskell-mode
        'smartparens
        'js2-mode
        'undo-tree ; Automatically loaded by evil.
        'mmm-mode
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

(require 's)
(require 'dash)

(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq c-basic-offset 2)
(setq css-indent-offset 2)
(setq indent-line-function 'insert-tab)
(savehist-mode 1)

(require 'pos-tip)

(defvar my-pretty-symbols
  `((?ƒ lambda "\\<defun\\>" (emacs-lisp-mode))
    (?ƒ lambda "\\<func\\>" (go-mode))
    (?∅ mine "\\<void\\>" (java-mode))
    (?∅ mine "\\<null\\>" (java-mode js-mode))
    (?∅ mine "\\<nil\\>" (emacs-lisp-mode go-mode))
    (?∃ mine "\\<defvar\\>" (emacs-lisp-mode))
    (?∃ mine "\\<var\\>" (js-mode go-mode))
    (?↩ mine "\\<return\\>" (java-mode js-mode go-mode))
    (?→ mine "\\<require\\>" (emacs-lisp-mode))
    (?→ mine "\\<import\\>" (java-mode go-mode))
    ;(?¬ logical ,(rxt-pcre-to-elisp "\\((!)") (java-mode js-mode go-mode))
    ))

(setq pretty-symbol-patterns (append pretty-symbol-patterns my-pretty-symbols))
(setq pretty-symbol-categories '(lambda relational logical mine))
(add-hook 'emacs-lisp-mode-hook 'pretty-symbols-mode)
(add-hook 'java-mode-hook 'pretty-symbols-mode)
(add-hook 'js-mode-hook 'pretty-symbols-mode)
(add-hook 'python-mode-hook 'pretty-symbols-mode)
(add-hook 'go-mode-hook 'pretty-symbols-mode)

; setup evil
(require 'evil)
(evil-mode 1)

; Highlight current line
(global-hl-line-mode 1)

(define-key global-map (kbd "RET") 'newline-and-indent)
(require 'goto-chg)
(define-key evil-normal-state-map (kbd "m") 'goto-last-change)

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

(defun maybe-add-tramp (file)
  (if (tramp-tramp-file-p default-directory)
      (let ((vec (tramp-dissect-file-name default-directory)))
            (tramp-make-tramp-file-name
             (tramp-file-name-method vec)
             (tramp-file-name-user vec)
             (tramp-file-name-host vec)
             file))
    file))

(defun maybe-strip-tramp-regex (str)
  (replace-regexp-in-string "/scpc.?:.+?:" "" str))
(defun maybe-strip-tramp (file)
  (if (tramp-tramp-file-p file)
      (tramp-file-name-localname
       (tramp-dissect-file-name (replace-regexp-in-string "\\\\" "" file)))
      file))

(define-key evil-normal-state-map (kbd "t") 'helm-M-x)
(define-key evil-visual-state-map (kbd "t") 'helm-M-x)

(require 'saveplace)
(setq-default save-place t)

(require 'expand-region)
(define-key evil-normal-state-map (kbd "e") 'er/expand-region)
(define-key evil-visual-state-map (kbd "e") 'er/expand-region)
(define-key evil-normal-state-map (kbd "E") 'er/contract-region)
(define-key evil-visual-state-map (kbd "E") 'er/contract-region)

; Refresh all buffers periodically.
(global-auto-revert-mode t)

(require 'eclim)
(setq eclim-print-debug-messages t)
(add-to-list 'eclim--file-coding-system-mapping '("no-conversion" . "utf8"))
(defun eclim--project-dir (&optional filename)
  "/usr/local/google/home/mtlin/workspace/bignav-mobile")
(defun eclim/workspace-dir ()
  (concat "/dev:" (eclim--call-process "workspace_dir")))
(setq eclim-executable "/usr/local/google/users/mtlin/eclipse43/stable/plugins/org.eclim_2.3.1/bin/eclim")
(add-hook 'java-mode-hook (lambda ()
                            (when (and buffer-file-name
                                       (eclim--accepted-p buffer-file-name)
                                       (eclim--project-dir buffer-file-name))
                              (eclim-mode 1))))
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

; setup autocompletion
(require 'auto-complete-config)
(require 'go-autocomplete)
(require 'ac-emacs-eclim-source)
(require 'tramp)
(add-to-list 'tramp-remote-path "/usr/lib/google-golang/bin")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

; Only autosave/backup locally
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

(setq tramp-verbose 3)
; Don't do any version control for remote stuff.
(setq vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))

(require 'cl)
(defadvice call-process
  (around tramp-advice-process-file activate)
  "Invoke `tramp-sh-handle-process-file' for Tramp files."
  (if (eq (tramp-find-foreign-file-name-handler default-directory)
          'tramp-sh-file-name-handler)
        (setq ad-return-value
              (apply 'process-file
                     (append (subseq (ad-get-args 0) 0 4)
                             (mapcar 'maybe-strip-tramp (ad-get-args 4)))))
    ad-do-it))
(defadvice call-process-region
  (around tramp-advice-call-process-region activate)
  "Invoke `tramp-sh-handle-call-process-region' for Tramp files."
  (if (eq (tramp-find-foreign-file-name-handler default-directory)
          'tramp-sh-file-name-handler)
      (setq ad-return-value
            (apply 'tramp-sh-handle-call-process-region
                   (append (subseq (ad-get-args 0) 0 6)
                           (mapcar 'maybe-strip-tramp (ad-get-args 6)))))
    ad-do-it))
(defadvice start-process
  (around tramp-advice-start-process activate)
  "Invoke `tramp-sh-handle-start-file-process' for Tramp files."
  (if (eq (tramp-find-foreign-file-name-handler default-directory)
          'tramp-sh-file-name-handler)
      (setq ad-return-value (apply 'start-file-process (ad-get-args 0)))
    ad-do-it))

(defadvice shell-command-to-string
  (around tramp-advice-shell-command-to-string activate)
  (if (eq (tramp-find-foreign-file-name-handler default-directory)
          'tramp-sh-file-name-handler)
      (progn
        (ad-set-arg 0 (maybe-strip-tramp-regex (ad-get-arg 0)))
        ad-do-it)
      ad-do-it))

(defadvice start-process-shell-command
  (around tramp-advice-start-process-shell-command activate)
  "Invoke `tramp-sh-handle-shell-command' for Tramp files."
  (if (eq (tramp-find-foreign-file-name-handler default-directory)
          'tramp-sh-file-name-handler)
      (with-parsed-tramp-file-name default-directory nil
        (let ((shell-file-name
               (tramp-get-connection-property v "remote-shell" "/bin/sh"))
              (shell-command-switch "-c"))
          ad-do-it))
    ad-do-it))

(setq-default ac-sources '(ac-source-dictionary ac-source-words-in-same-mode-buffers))
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(ac-emacs-eclim-config)
(setq ac-auto-start 0)
(setq ac-auto-show-menu t)
(setq ac-quick-help-delay 0.5)
(setq ac-candidate-limit 100)
(add-to-list 'ac-modes 'html-mode)
(global-auto-complete-mode t)

(require 'smartparens-config)
(smartparens-global-mode t)

(setq helm-M-x-always-save-history t)

(require 'helm-config)
(require 'helm)
(require 'helm-buffers)
(require 'helm-files)

(defun myfind (c)
  (let (non-essential)
    (helm-find-many-files c)))

(defun g4-candidates ()
  (if (tramp-tramp-file-p default-directory)
      (let ((result (shell-command-to-string "g4 opened | sed 's/#.*//' | g4 -x - where | awk '/^\\// {print $3}'")))
        (if (and (not (string-match "command not found" result)) (not (string-match "Perforce client error" result)))
            (mapcar 'maybe-add-tramp (split-string result))
          nil))))

(defvar helm-source-g4-opened
    '((name . "g4-opened")
      (candidates . g4-candidates)
      (no-delay-on-input)
      (action . (("Find file" . myfind))))
      "Source for currently opened g4 files.")

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

(defvar myrecents-current-buffer nil)
(defvar helm-source-myrecent
  `((name . "Recentf")
    (init . (lambda ()
              (setq myrecents-current-buffer buffer-file-name)
              (require 'recentf)
              (or recentf-mode (recentf-mode 1))))
    (candidates . myrecents)
    (no-delay-on-input)
    (action . (("Find file" . myfind)))))
(defun my-helm ()
  (interactive)
  (helm-other-buffer
   '(helm-source-myrecent
     helm-c-source-buffers-list
     helm-source-g4-opened)
   " *my-helm*"))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                            (define-key evil-normal-state-map (kbd ".") 'eval-last-sexp)))
(define-key evil-normal-state-map (kbd ",") 'my-helm)
(setq helm-buffer-max-length 100)
(setq helm-input-idle-delay 0.1)
(setq helm-idle-delay 0.1)
(define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)
(helm-mode 1)

(setq vc-follow-symlinks 't)

(defun glaze-hook ()
  (when (is-g4-project (maybe-strip-tramp buffer-file-name))
    (start-file-process "glaze" nil "glaze" (maybe-strip-tramp default-directory))))
(add-hook 'go-mode-hook
          (lambda ()
            ;(add-hook 'before-save-hook
             ;         (lambda ()
             ;           (let ((temporary-file-directory (concat (file-remote-p default-directory) "/tmp"))) (gofmt))) 'nil make-it-local)
            (add-hook 'after-save-hook 'glaze-hook nil 'make-it-local)
            (start-file-process "gocode libs" nil "gocode_set_lib" (maybe-strip-tramp default-directory))))

(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
; Reparse buffer for new modes
(setq mmm-parse-when-idle t)
(mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-js)
(mmm-add-mode-ext-class 'html-mode "\\.html\\'" 'html-css)
(set-face-background 'mmm-default-submode-face "gray98")

; Defer to built-in js-mode, but keep syntax highlighting.
(add-hook 'js-mode-hook 'js2-minor-mode)

(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(define-key evil-normal-state-map (kbd "f") 'ace-jump-mode)

; Persistent undo!
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo")))

(require 'whitespace)
(setq whitespace-style '(face empty trailing))
;(setq whitespace-style '(face empty lines-tail trailing))
(global-whitespace-mode t)

; Tail the compilation buffer.
(setq compilation-scroll-output t)

;; Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
    (interactive)
      "Return true if system is darwin-based (Mac OS X)"
        (string-equal system-type "darwin")
          )

;; Check if system is GNU/Linux
(defun system-type-is-gnu ()
    (interactive)
      "Return true if system is GNU/Linux-based"
        (string-equal system-type "gnu/linux")
          )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))

(set-face-attribute 'default nil :family "Menlo")
(set-face-attribute 'default nil :height 120)
(setq-default line-spacing 4)

; setup colors
(require 'solarized)
(load-theme 'solarized-light t)

(tool-bar-mode -1)
(menu-bar-mode -1)

;(add-hook 'after-init-hook #'global-flycheck-mode)
;(load-file "/usr/share/emacs/site-lisp/google/google.el")
;(require 'google)
;(require 'google-flymake)
;(require 'google3-build)
;(setq google-build-system "blaze")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
