;;-----------------------------------------------------------------------------
;; .emacs configuration file
;; author: Joseph Lisee
;; tested on: GNU Emacs 23.1.1
;;
;; packages supported:
;;   time, cl, cc-mode, font-lock, ede, eieio, elib, jde, func-menu, 
;;   html-mode, semantic, speedbar, workshop, xml-mode
;;
;; last mod: 2011-05-26
;;-----------------------------------------------------------------------------


;; TODO:
;;  -Make emacs sub directory setable up here
;;  -Fix flyspell C++ issue
;;  -Get compilation and error jumping working

;; ----------------------------------------------------------------------- ;;
;; Standard Settings
;; ----------------------------------------------------------------------- ;;

(setq-default indent-tabs-mode nil)
(setq delete-key-deletes-forward t)
(setq mouse-yank-at-point t)
(line-number-mode t) ;; Show line numbers
(column-number-mode t) ;; Show column numbers
(show-paren-mode t) ;; Match parens


;; ----------------------------------------------------------------------- ;;
;; Global Key Bindings
;; ----------------------------------------------------------------------- ;;

;; (define-key global-map "\C-xw" 'what-line)
;; (define-key global-map "\C-z" 'undo)
;; (define-key global-map [delete] 'delete-char)
;; (define-key global-map [backspace] 'delete-backward-char)
;; (define-key global-map [f1] 'help-command)
;; (define-key global-map [f2] 'undo)
;; (define-key global-map [f3] 'isearch-forward)
;; (define-key global-map [f4] 'other-window)
;; (define-key global-map [f12] 'revert-buffer)
;; (define-key global-map [button4] 'previous-line)
;; (define-key global-map [button5] 'next-line)


;; ----------------------------------------------------------------------- ;;
;; Custom Color Scheme (Dark background, light text)
;; ----------------------------------------------------------------------- ;;

(defconst color-scheme 'dark)
(defconst foreground-color "gray85")
(defconst background-color "gray25")
(defconst cursor-color "red3")
(defconst pointer-color "white")

(progn 
  (add-to-list 'default-frame-alist '(foreground-color . "gray85"))
  (add-to-list 'default-frame-alist '(background-color . "gray25"))
  (add-to-list 'default-frame-alist '(cursor-color . "red3"))
  (add-to-list 'default-frame-alist '(background-mode . dark))
  (set-cursor-color cursor-color)
  (set-mouse-color pointer-color))

;; Setup font-lock syntax coloring package
;; This is broken for some reason, It gives me the dark bacground but the
;; light forecolors
(defconst lconfig-font-lock-faces  
  (list '(font-lock-builtin-face 
          ((((class color) (background dark)) (:foreground "cyan" :bold t)) 
           (((class color)) (:foreground "cyan" :bold t)))) 
        '(font-lock-comment-face 
          ((((class color) (background dark)) (:foreground "LightPink")) 
           (((class color)) (:foreground "LightPink")))) 
        '(font-lock-constant-face
          ((((class color) (background dark)) (:foreground "SpringGreen")) 
           (((class color)) (:foreground "SpringGreen")))) 
        '(font-lock-doc-string-face
          ((((class color) (background dark)) (:foreground "SpringGreen")) 
           (((class color)) (:foreground "#8080FE")))) 
        '(font-lock-function-name-face
          ((((class color) (background dark)) (:foreground "wheat3")) 
           (((class color)) (:foreground "wheat3")))) 
        '(font-lock-keyword-face
          ((((class color) (background dark)) (:foreground "SkyBlue" :bold t))
           (((class color)) (:foreground "SkyBlue" :bold t)))) 
        '(font-lock-preprocessor-face
          ((((class color) (background dark)) (:foreground "SkyBlue")) 
           (((class color)) (:foreground "SkyBlue")))) 
        '(font-lock-reference-face
          ((((class color) (background dark)) (:foreground "yellow")) 
           (((class color)) (:foreground "yellow")))) 
        '(font-lock-string-face
          ((((class color) (background dark)) (:foreground "SpringGreen")) 
           (((class color)) (:foreground "SpringGreen")))) 
        '(font-lock-type-face
         ((((class color) (background dark)) (:foreground "orange1")) 
           (((class color)) (:foreground "orange1")))) 
        '(font-lock-variable-name-face
          ((((class color) (background dark)) (:foreground "yellow")) 
           (((class color)) (:foreground "yellow")))) 
        '(font-lock-warning-name-face
          ((((class color) (background dark)) (:foreground "DarkOrange")) 
           (((class color)) (:foreground "DarkOrange"))))))

;; If possible set up a custom color scheme, otherwise turn colors off
(autoload 'custom-set-faces "font-lock" "Set the color scheme" t)
(autoload 'font-lock-fontify-buffer "font-lock" "Fontify Buffer" t)
(condition-case err
    (progn (apply 'custom-set-faces lconfig-font-lock-faces)
           (add-hook 'c-mode-common-hook 'font-lock-fontify-buffer)
           (add-hook 'emacs-lisp-mode-hook 'font-lock-fontify-buffer)
           )
  (error (progn
           (message "Could not customize colors, disabling colored fonts.")
           (setq-default font-lock-auto-fontify t))))


;; ----------------------------------------------------------------------- ;;
;; Associate Extensions with common modes (python, c++, etc)
;; ----------------------------------------------------------------------- ;;

(add-to-list 'auto-mode-alist '("SConstruct$" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript$" . python-mode))
(add-to-list 'auto-mode-alist '("wscript_build$" . python-mode))
(add-to-list 'auto-mode-alist '("wscript$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.xdr$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))


;; ----------------------------------------------------------------------- ;;
;; Misc Libraries and Custom modes
;; ----------------------------------------------------------------------- ;;

;; Load all the sub directories in my local emacs to the path
(let ((default-directory "~/files/emacs"))
      (normal-top-level-add-subdirs-to-load-path))

;; Smart tabing - tab based smart indenting useful for editing code that uses 
;; tabs, turn on with M-x smart-tab-mode
(require 'smart-tab)

;; Full matlab integration
(load-library "matlab-load")


;; ----------------------------------------------------------------------- ;;
;; Setup C mode
;; ----------------------------------------------------------------------- ;;

;; Make sure we autoload
(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode" t)
(autoload 'c-mode-common-hook "cc-mode" "C Mode Hooks" t)
(autoload 'c-add-style "cc-mode" "Add coding style" t)

;; Create my personal style custom C coding style
(defconst my-c-style
  '((c-auto-newline                 . nil)
    (c-basic-offset                 . 4)
    (c-comment-only-line-offset     . 0)
    (c-echo-syntactic-information-p . nil)
    (c-hungry-delete-key            . 0)
    (c-tab-always-indent            . 0) ;; Maybe have this at "t"
    (c-toggle-hungry-state          . t)
    (c-hanging-braces-alist         . ((substatement-open after)
                                       (brace-list-open)))
    (c-offsets-alist                . ((arglist-close . c-lineup-arglist)
                                       (case-label . 4)
                                       (substatement-open . 0)
                                       (block-open . 0) ; no space before {
                                       (knr-argdecl-intro . -)))
    (c-hanging-colons-alist         . ((member-init-intro before)
                                       (inher-intro)
                                       (case-label after)
                                       (label after)
                                       (access-label after)))
    (c-cleanup-list                 . (scope-operator
                                       empty-defun-braces
                                       defun-close-semi)))

;;    (c-echo-syntactic-information-p . t))
  "Joe's C/C++ Programming Style")
(c-add-style "PERSONAL" my-c-style)

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; set my personal style for the current buffer
  (c-set-style "PERSONAL")
  ;; other customizations
  (setq tab-width 4
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; TODO: Consider these
  ;; (progn (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  ;;        (define-key c-mode-base-map "\C-z" 'undo)
  ;;        (define-key c-mode-base-map [f4] 'speedbar-get-focus)
  ;;        (define-key c-mode-base-map [f5] 'next-error)
  ;;        (define-key c-mode-base-map [f6] 'run-program)
  ;;        (define-key c-mode-base-map [f7] 'compile)
  ;;        (define-key c-mode-base-map [f8] 'set-mark-command)
  ;;        (define-key c-mode-base-map [f9] 'insert-breakpoint)
  ;;        (define-key c-mode-base-map [f10] 'step-over)
  ;;        (define-key c-mode-base-map [f11] 'step-into)
  ;;        (c-add-style "Joe's Coding Style" jlisee-c-style t)))


;; ----------------------------------------------------------------------- ;;
;; Time Mode (AM/PM Clock in lower bar)
;; ----------------------------------------------------------------------- ;;

(autoload 'display-time "time" "Display Time" t)
(condition-case err
    (display-time)
  (error (message "Unable to load Time package.")))
(setq display-time-24hr-format nil)
(setq display-time-day-and-date t)


;; ----------------------------------------------------------------------- ;;
;; YAML Mode
;; ----------------------------------------------------------------------- ;;

(require 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
  '(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(setq yaml-indent-offset 4)


;; ----------------------------------------------------------------------- ;;
;; Flyspell Mode (Spell Checker)
;; ----------------------------------------------------------------------- ;;

;; Audo load flyspell (as you type spell checker) and turn it for coding
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;; Turn on for text editing
(add-hook 'text-mode-hook 'turn-on-flyspell)
;; NOTE: The two versions of the hook should work but they don't
;;(add-hook 'LaTeX-mode-hook '(flyspell-mode t))
;;(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
(add-hook 'latex-mode-hook (lambda () (flyspell-mode +1)))
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)

;; Turn on for programming
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook'flyspell-prog-mode)

;; Hook helper function
(defun turn-on-flyspell ()
   "Force flyspell-mode on using a positive arg.  For use in hooks."
   (interactive)
   (flyspell-mode 1))

;; TODO: 
;; To fix the C++ include check issue by doing the following:
;; Create a wrapper for flyspell-generic-progmode-verify, which is prog-mode
;; implementation of flyspell-generic-check-word-p, this should return false
;; if we are in an include statement (ie. string face on an include line)
;; It would also be could do not spell check strings without spaces, those
;; as most likly to settings as strings
;; NOTE: these functions use the emacs "point" concept to search based on
;; where the cursor is with regexs, etc.
;; Docs: see here for info on using emacs lisp:
;;   http://www.emacswiki.org/emacs/EmacsLisp

;; ----------------------------------------------------------------------- ;;
;; iswitch (better buffer switching)
;; ----------------------------------------------------------------------- ;;

;; TODO: consider 'Icicles' package instead

(iswitchb-mode 1)
;; These ignore all *...* buffers
(setq iswitchb-buffer-ignore '("^*"))


;; ----------------------------------------------------------------------- ;;
;; Latex
;; ----------------------------------------------------------------------- ;;

(add-hook 'latex-mode-hook (lambda () (longlines-mode +1)))


;; ----------------------------------------------------------------------- ;;
;; Custom Set Variables
;; ----------------------------------------------------------------------- ;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(matlab-functions-have-end t)
 '(matlab-shell-command "/opt/matlab/bin/matlab")
 '(matlab-verify-on-save-flag t)
 '(mlint-programs (quote ("mlint" "glnx86/mlint" "/opt/matlab/bin/glnx86/mlint"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )