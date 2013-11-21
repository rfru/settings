(if (equal system-type 'darwin)
    (progn
                                        ;(setq noctilux-broken-srgb nil)
                                        ;(load-theme 'noctilux t)

      (set-face-attribute 'default nil :family "Consolas")
      (set-face-attribute 'default nil :height 120)
      (set-fontset-font "fontset-default"
                        'unicode
                        '("Consolas" . "iso10646-1"))
      (setq-default line-spacing 4)
      (add-to-list 'default-frame-alist '(width  . 125))
      (add-to-list 'default-frame-alist '(height . 50))

      (tool-bar-mode -1)))

(defun reset-ui ()
  (menu-bar-mode -1)
  (load-theme 'solarized-light t)

  (setq column-number-mode t)

                                        ; Highlight current line
  (global-hl-line-mode 1)

  (custom-set-variables
   '(inhibit-startup-screen t))

  (custom-set-faces
                                        ; TODO don't hardcode this color
   '(mode-line ((t (:inverse-video nil))))
   '(mode-line-inactive ((t (:foreground "brightcyan" :background "brightwhite" :inverse-video nil))))
   '(font-lock-comment-face ((t (:foreground "brightcyan"))))
   '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground nil))))
   '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground nil))))
   '(font-lock-doc-string-face ((t (:inherit font-lock-comment-face :foreground nil))))
   '(show-paren-match ((t (:background nil :foreground nil :inverse-video t))))
   ;'(show-paren-mismatch ((t (:background "magenta" :foreground nil :inverse-video t))))
   '(sp-show-pair-match-face ((t (:foreground nil :background nil :inherit show-paren-match))))

   '(ac-completion-face ((t (:inherit shadow))))
   '(ac-emacs-eclim-candidate-face ((t (:inherit ac-candidate-face))))
   '(ac-emacs-eclim-selection-face ((t (:inherit ac-selection-face))))
   '(whitespace-line ((t (:inherit error :background nil))))
   '(vertical-border ((t (:foreground "white" :background "brightwhite"))))
   '(popup-tip-face ((t (:inherit secondary-selection))))
   '(match ((t (:background nil :inverse-video nil :weight bold :underline t :foreground "blue"))))
   '(header-line ((t (:weight bold :inverse-video nil :background nil))))
   '(helm-candidate-number ((t (:inherit mode-line))))
   '(helm-source-header ((t (:inherit default :weight bold :underline t))))
   '(helm-selection ((t (:inherit highlight)))))
  )

;; (add-hook 'after-make-frame-functions (lambda (frame)
;;                                         (reset-ui)))
(reset-ui)
(provide 'my-theme)
