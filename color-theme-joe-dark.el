;; My first color theme, a conversion of my manually huge dark color theme
;; Based on the blog from here:
;;   http://emacs-fu.blogspot.com/2009/03/color-theming.html
;;   http://emacs-fu.blogspot.com/2010/04/zenburn-color-theme.html

(require 'color-theme)
(defun color-theme-joe-dark ()
  "Joseph Lisee's dark theme (converted from a borrowed emacs colors) Jun 2011"
  (interactive)
  (color-theme-install
    '(color-theme-joe-dark
       ((foreground-color . "gray85")
         (background-color . "gray25") 
         (background-mode . dark))
       ;;(bold ((t (:bold t))))
       ;;(bold-italic ((t (:italic t :bold t))))
       ;;(default ((t (nil))))
       
       (font-lock-builtin-face ((t (:bold t :foreground "cyan"))))
       (font-lock-comment-face ((t (:foreground "LightPink"))))
       ;;(font-lock-comment-delimiter-face ((t (:foreground "#666666"))))
       (font-lock-constant-face ((t (:foreground "SpringGreen"))))
       (font-lock-doc-string-face ((t (:foreground "SpringGreen"))))
       ;;(font-lock-doc-face ((t (:foreground "gray"))))
       (font-lock-function-name-face ((t (:foreground "wheat3"))))
       (font-lock-reference-face ((t (:foreground "yellow"))))
       (font-lock-keyword-face ((t (:bold t :foreground "SkyBlue"))))
       (font-lock-preprocessor-face ((t (:foreground "SkyBlue"))))
       (font-lock-string-face ((t (:foreground "SpringGreen"))))
       (font-lock-type-face ((t (:foreground "orange1"))))
       (font-lock-variable-name-face ((t (:foreground "yellow"))))
       (font-lock-warning-face ((t (:foreground "DarkOrange"))))

       ;;(font-lock-warning-face ((t (:bold t :italic nil :underline nil 
       ;;                              :foreground "yellow"))))
       ;;(hl-line ((t (:background "#112233"))))
       ;;(mode-line ((t (:foreground "#ffffff" :background "#333333"))))
       ;;(region ((t (:foreground nil :background "#555555"))))
       ;;(show-paren-match-face ((t (:bold t :foreground "#ffffff" 
       ;;                             :background "#050505"))))
       )))