(deftheme light "My light theme.")
(let* ((black "black"))
  (custom-theme-set-faces
   'light
   ;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Direct specification.
   ;;;;;;;;;;;;;;;;;;;;;;;
   `(default ((t (:foreground ,black ))))
   ;; `(shadow ((,class (:foreground ,solarized-comments))))
   ;; `(match ((,class (:background ,solarized-hl :foreground ,solarized-emph :weight bold))))
   ;; `(cursor ((,class (:foreground ,solarized-bg :background ,solarized-fg
   ;;                                :inverse-video t))))
   ;; `(mouse ((,class (:foreground ,solarized-bg :background ,solarized-fg
   ;;                               :inverse-video t))))
   ;; `(escape-glyph-face ((,class (:foreground ,red))))
   ;; `(fringe ((,class (:foreground ,solarized-fg :background ,s-fringe-bg))))
   ;; `(highlight ((,class (:background ,solarized-hl))))
   ;; `(link ((,class (:foreground ,yellow :underline t :weight bold))))
   ;; `(link-visited ((,class (:foreground ,yellow :underline t :weight normal))))
   ;; `(success ((,class (:foreground ,green ))))
   ;; `(warning ((,class (:foreground ,yellow ))))
   ;; `(error ((,class (:foreground ,orange))))
   ;; `(lazy-highlight ((,class (:foreground ,solarized-bg :background ,yellow
   ;;                                        :weight normal))))
   ;; `(escape-glyph ((,class (:foreground ,violet))))

   ;; ;; font lock
   ;; `(font-lock-builtin-face ((,class (:foreground ,solarized-fg :weight bold))))
   ;; `(font-lock-comment-delimiter-face
   ;;   ((,class (:foreground ,solarized-comments))))
   ;; `(font-lock-comment-face ((,class (:foreground ,solarized-comments))))
   ;; `(font-lock-constant-face ((,class (:foreground ,blue :weight bold))))
   ;; `(font-lock-doc-face ((,class (:foreground ,cyan))))
   ;; `(font-lock-function-name-face ((,class (:foreground ,blue))))
   ;; `(font-lock-keyword-face ((,class (:foreground ,green :weight bold))))
   ;; `(font-lock-negation-char-face ((,class (:foreground ,yellow :weight bold))))
   ;; `(font-lock-preprocessor-face ((,class (:foreground ,blue))))
   ;; `(font-lock-regexp-grouping-construct ((,class (:foreground ,yellow :weight bold))))
   ;; `(font-lock-regexp-grouping-backslash ((,class (:foreground ,green :weight bold))))
   ;; `(font-lock-string-face ((,class (:foreground ,cyan))))
   ;; `(font-lock-type-face ((,class (:foreground ,yellow))))
   ;; `(font-lock-variable-name-face ((,class (:foreground ,blue))))
   ;; `(font-lock-warning-face ((,class (:foreground ,orange :weight bold :underline t))))

                                        ; other
   '(mode-line ((t (:inverse-video nil))))
   '(mode-line-inactive ((t (:foreground "brightcyan" :background "brightwhite" :inverse-video nil))))
   '(vertical-border ((t (:foreground "white" :background "brightwhite"))))
   '(match ((t (:background nil :inverse-video nil :weight bold :underline t :foreground "blue"))))
   '(font-lock-comment-face ((t (:foreground "magenta"))))
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

   '(whitespace-line ((t (:inherit error :background nil))))
   '(whitespace-trailing ((t (:inherit trailing-whitespace :background nil))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Pure inherited aliases.
   ;;;;;;;;;;;;;;;;;;;;;;;;;
   '(web-mode-html-tag-face ((t (:inherit font-lock-variable-name-face))))
   '(web-mode-html-attr-name-face ((t (:inherit font-lock-keyword-face))))
   '(web-mode-param-name-face ((t (:inherit font-lock-variable-name-face))))
   '(web-mode-doctype-face ((t (:inherit web-mode-comment-face))))
   '(web-mode-symbol-face ((t (:inherit web-mode-comment-face))))

   '(ac-completion-face ((t (:inherit font-lock-special-keyword-face))))
   '(ac-emacs-eclim-candidate-face ((t (:inherit ac-candidate-face))))
   '(ac-emacs-eclim-selection-face ((t (:inherit ac-selection-face))))
   '(popup-tip-face ((t (:inherit secondary-selection))))

   '(helm-candidate-number ((t (:inherit mode-line))))
   '(helm-selection ((t (:inherit highlight))))))

(provide 'my-theme)
