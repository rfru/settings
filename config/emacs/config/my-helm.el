(require 'helm-config)
(require 'helm)
(require 'helm-buffers)
(require 'helm-files)

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
     helm-source-myrecent
     helm-c-source-buffers-list)
   " *my-helm*"))

(define-key evil-normal-state-map (kbd ",") 'my-helm)
(define-key helm-map (kbd "<f4>") 'helm-keyboard-quit)
(global-set-key (kbd "<f4>") (kbd "<escape>"))
(helm-mode 1)

(provide 'my-helm)
