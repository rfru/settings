(deftheme light "My light theme.")
(let* ((black "black")
       (gray "gray")
       (cyan "cyan")
       (red "red")
       (green "yellow")
       (blue "blue")
       (lightgray "white")
       (lightgray2 "brightcyan")
       (pink "magenta")
       (darkgray "brightblue")
       (white "brightwhite"))
  (custom-theme-set-faces
   'light
   ;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Direct specification.
   ;;;;;;;;;;;;;;;;;;;;;;;
   `(default ((t (:foreground ,black ))))
   `(isearch ((t (:background ,white :foreground ,pink :inverse-video t))))
   `(lazy-highlight ((t (:foreground ,gray :background ,black :inverse-video t))))
   `(highlight ((t (:background ,lightgray))))
   `(font-lock-keyword-face ((t (:foreground ,cyan))))
   `(font-lock-builtin-face ((t (:foreground ,blue))))
   `(font-lock-constant-face ((t (:foreground ,pink))))
   `(font-lock-string-face ((t (:foreground ,black))))
   `(region ((t (:background ,lightgray))))

                                        ; other
   `(mode-line ((t (:inverse-video nil :background ,lightgray))))
   `(mode-line-inactive ((t (:foreground ,gray :background ,white :inverse-video nil))))
   `(vertical-border ((t (:foreground "white" :background ,white))))
   `(match ((t (:underline t :weight bold :foreground ,pink))))
   `(font-lock-comment-face ((t (:foreground ,lightgray2))))
   '(header-line ((t (:weight bold :inverse-video nil :background nil))))
   '(show-paren-match ((t (:background nil :foreground nil :inverse-video t))))
   '(helm-source-header ((t (:inherit default :weight bold :underline t))))

   ;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Overriden inheritance.
   ;;;;;;;;;;;;;;;;;;;;;;;;
   '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground nil))))
   '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground nil))))
   '(font-lock-doc-string-face ((t (:inherit font-lock-comment-face :foreground nil))))

                                        ;'(show-paren-mismatch ((t (:background "magenta" :foreground nil :inverse-video t))))
   '(sp-show-pair-match-face ((t (:foreground nil :background nil :inherit show-paren-match))))

   ; Autocomplete with pop-tip generates empty whitespace at end of buffer
   '(whitespace-empty ((t (:background nil :foreground nil))))
   '(whitespace-line ((t (:inherit error :background nil))))
   '(whitespace-trailing ((t (:inherit trailing-whitespace :background nil))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Pure inherited aliases.
   ;;;;;;;;;;;;;;;;;;;;;;;;;
   '(secondary-selection ((t (:inherit highlight))))
   '(web-mode-html-tag-face ((t (:inherit font-lock-variable-name-face))))
    '(web-mode-html-attr-name-face ((t (:inherit font-lock-keyword-face))))
    '(web-mode-param-name-face ((t (:inherit font-lock-variable-name-face))))
    '(web-mode-doctype-face ((t (:inherit web-mode-comment-face))))
    '(web-mode-symbol-face ((t (:inherit web-mode-comment-face))))

    '(ac-completion-face ((t (:inherit font-lock-comment-face))))
    '(ac-emacs-eclim-candidate-face ((t (:inherit ac-candidate-face))))
    '(ac-emacs-eclim-selection-face ((t (:inherit ac-selection-face))))
    '(popup-tip-face ((t (:inherit secondary-selection))))

    '(helm-candidate-number ((t (:inherit mode-line))))
      '(helm-selection ((t (:inherit highlight))))))

(provide 'my-theme)
