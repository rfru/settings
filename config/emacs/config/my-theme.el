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
   '(mode-line-inactive ((t (:foreground "white" :background "brightwhite"))))

   '(ac-completion-face ((t (:inherit shadow))))
   '(ac-emacs-eclim-candidate-face ((t (:inherit ac-candidate-face))))
   '(ac-emacs-eclim-selection-face ((t (:inherit ac-selection-face))))
   '(popup-tip-face ((t (:inherit secondary-selection))))
   '(match ((t (:inherit lazy-highlight))))
   '(helm-source-header ((t (:inherit default))))
   '(helm-selection ((t (:background "gray")))))
  )

(add-hook 'after-make-frame-functions (lambda (frame)
                                        (reset-ui)))
(reset-ui)
(provide 'my-theme)
