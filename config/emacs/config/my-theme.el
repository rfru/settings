  (deftheme light "My light theme.")
  (let* ((black "black")
         (gray "brightcyan")
         (cyan "cyan")
         (red "red")
         (orange "yellow")
         (blue "blue")
         (lightgray "white")
         (lightgray2 "brightcyan")
         (pink "magenta")
         (purple "brightmagenta")
         (darkgray "brightblue")
         (white "brightwhite"))
    (custom-theme-set-faces
     'light
     ;;;;;;;;;;;;;;;;;;;;;;;
                                          ; Direct specification.
     ;;;;;;;;;;;;;;;;;;;;;;;
     ;; `(default ((t (:foreground ,black ))))
     `(isearch ((t (:background ,white :foreground ,pink :inverse-video t))))
     `(lazy-highlight ((t (:foreground ,lightgray :background ,black :inverse-video t))))
     `(hl-line ((t :background ,lightgray)))
     `(highlight ((t (:background ,lightgray))))

     `(font-lock-keyword-face ((t (:foreground ,cyan))))
     `(font-lock-builtin-face ((t (:foreground ,pink))))
     `(font-lock-constant-face ((t (:inherit default))))
     `(font-lock-string-face ((t (:foreground ,cyan))))
     `(font-lock-function-name-face ((t (:foreground ,purple))))
     `(font-lock-comment-face ((t (:foreground ,gray))))
     `(font-lock-keyword-face ((t (:foreground ,black :weight bold))))
     `(font-lock-variable-name-face ((t (:inherit default))))
     `(font-lock-type-face ((t (:inherit default))))

     `(error ((t (:inverse-video t :foreground ,red))))
     `(region ((t (:inverse-video t :foreground ,black))))

     `(popup-face ((t (:background ,lightgray))))
                                          ; other
     `(minibuffer-prompt ((t (:inherit default :weight bold))))
     `(mode-line ((t (:inverse-video nil :background ,lightgray))))
     `(mode-line-inactive ((t (:foreground ,gray :background ,white :inverse-video nil))))
     `(vertical-border ((t (:foreground ,lightgray :background ,white))))
     `(match ((t (:underline t :weight bold :foreground ,black))))
     '(header-line ((t (:weight bold :inverse-video nil :background nil))))
     '(show-paren-match ((t (:background nil :foreground nil :inverse-video t))))
     `(helm-source-header ((t (:foreground ,darkgray :background ,lightgray :weight bold))))

     '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground nil))))
     '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground nil))))
     '(font-lock-doc-string-face ((t (:inherit font-lock-comment-face :foreground nil))))

                                          ;'(show-paren-mismatch ((t (:background "magenta" :foreground nil :inverse-video t))))
     '(sp-show-pair-match-face ((t (:foreground nil :background nil :inherit show-paren-match))))
     '(js2-external-variable ((t (:inherit default))))
     '(js2-jsdoc-value ((t (:inherit font-lock-comment-face))))
     '(js2-jsdoc-tag ((t (:inherit font-lock-comment-face))))
     '(js2-jsdoc-type ((t (:inherit font-lock-comment-face))))
     '(js2-function-param ((t (:inherit font-lock-variable-name-face))))

     ; Autocomplete with pop-tip generates empty whitespace at end of buffer
     '(whitespace-empty ((t (:background nil :foreground nil))))
     `(whitespace-line ((t (:foreground ,red))))
     '(whitespace-trailing ((t (:inherit trailing-whitespace :background nil))))

     '(secondary-selection ((t (:inherit highlight))))
     '(web-mode-html-tag-bracket-face ((t (:inherit default))))
     '(web-mode-html-tag-face ((t (:inherit default :weight bold))))
     '(web-mode-css-property-name-face ((t (:inherit default))))
     '(web-mode-css-selector-face ((t (:inherit default))))
     '(web-mode-html-attr-name-face ((t (:inherit default))))
     '(web-mode-param-name-face ((t (:inherit font-lock-variable-name-face))))
     '(web-mode-doctype-face ((t (:inherit web-mode-comment-face))))
     '(web-mode-symbol-face ((t (:inherit web-mode-comment-face))))
     '(web-mode-current-element-highlight-face ((t (:inherit highlight))))

     '(ac-completion-face ((t (:inherit font-lock-comment-face))))
     '(ac-emacs-eclim-candidate-face ((t (:inherit ac-candidate-face))))
     '(ac-emacs-eclim-selection-face ((t (:inherit ac-selection-face))))
     '(popup-tip-face ((t (:inherit secondary-selection))))
     '(ghc-face-error ((t (:inherit error))))

     `(helm-match ((t (:foreground ,white :background ,pink :inverse-video nil))))
      `(helm-candidate-number ((t (:inherit mode-line))))
     `(helm-selection ((t (:weight bold :underline t))))))

(provide 'my-theme)
