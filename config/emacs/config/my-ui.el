(when (display-graphic-p)
  (tool-bar-mode -1)
  (when (equal system-type 'darwin)
    (set-face-attribute 'default nil :family "Consolas")
    (set-face-attribute 'default nil :height 120)
    (set-fontset-font "fontset-default"
                      'unicode
                      '("Consolas" . "iso10646-1"))
    (setq-default line-spacing 4)
    (add-to-list 'default-frame-alist '(width  . 125))
    (add-to-list 'default-frame-alist '(height . 50))
    )
  (when (equal system-type 'gnu/linux)
    )
  )

(defun reset-ui ()
  (menu-bar-mode -1)

  (setq column-number-mode t)

  (add-hook 'window-configuration-change-hook
            (lambda ()
              (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 1 1)))
                                        ; Highlight current line
  (global-hl-line-mode t)

  ; Word wrapping.
  ;(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  ;(global-visual-line-mode t)

  (custom-set-variables
   '(inhibit-startup-screen t))
)

;; (add-hook 'after-make-frame-functions (lambda (frame)
;;                                         (reset-ui)))
(reset-ui)
(provide 'my-ui)
