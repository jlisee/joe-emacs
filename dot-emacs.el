;;-----------------------------------------------------------------------------
;; .emacs configuration file
;; author: Joseph Lisee
;; tested on:  GNU Emacs 22.3.1
;;             GNU Emacs 23.1.1
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

;; TAB SIZE (can't get this to work)
;;(defun jl-tab-size() 2)

;; Saving place in files
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
;;(setq-default save-place t)                 ;; activate it for all buffers
(require 'saveplace)                          ;; get the package


(if (not window-system)
    nil
  ;; Width and height
  (set-screen-width 80)
  (set-screen-height 72)

  ;; Remove Menu bar (tool bar disabled with tool-bar+ below)
  (menu-bar-mode -99)
)

;; ----------------------------------------------------------------------- ;;
;; Global Key Bindings
;; ----------------------------------------------------------------------- ;;

;; Change goto line
(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key "\C-c\C-g" 'goto-line)
(global-set-key (kbd "C-x m") 'menu-bar-mode) ; menu hide/show
(global-set-key (kbd "C-x t") 'show-tool-bar-for-one-command) ; toolbar show
(global-set-key "\C-c\C-a" 'mark-whole-buffer) ; rebind select all
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key "\C-ca" 'ack)

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
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))


;; ----------------------------------------------------------------------- ;;
;; Misc Libraries and Custom modes
;; ----------------------------------------------------------------------- ;;

;; Load all the sub directories holding this .emacs. This assumes the .emacs
;; is really a symlink to a subdirectory containing all the needed emacs files
(let ((default-directory (file-name-directory (file-truename load-file-name))))
      (normal-top-level-add-subdirs-to-load-path))

;; Smart tabing - tab based smart indenting useful for editing code that uses 
;; tabs, turn on with M-x smart-tab-mode
(require 'smart-tab)

;; Full matlab integration
(load-library "matlab-load")

;; Put in the 80 character gutter line
(require 'fill-column-indicator)
(setq fci-style 'rule) ;; Draw a thing line
(setq fci-rule-width 1) ;; Make the line only 1 pixel wide
(setq fci-rule-color "gray35")


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
    (c-basic-offset                 . 2)
    (c-comment-only-line-offset     . 0)
    (c-echo-syntactic-information-p . nil)
    (c-hungry-delete-key            . 0)
    (c-tab-always-indent            . 0) ;; Maybe have this at "t"
    (c-toggle-hungry-state          . t)
    (c-hanging-braces-alist         . ((substatement-open after)
                                       (brace-list-open)))
    (c-offsets-alist                . ((arglist-close . c-lineup-arglist)
                                       (case-label . 2)
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

;; Comment out "#if 0 comment sections"
;; TODO: update this to handle 1
(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; set my personal style for the current buffer
  (c-set-style "PERSONAL")
  ;; other customizations
  (setq tab-width 2
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  (font-lock-add-keywords
   nil
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end)
  (setq-default fill-column 80) ;; Fill column at 80
  (if (not window-system)
      nil
    (fci-mode)))
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
;; Compilation Mode Customizations
;; ----------------------------------------------------------------------- ;;

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


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
(setq yaml-indent-offset 2)


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
(setq iswitchb-buffer-ignore '("^ " "^\\*.*$"))

;; Use arrow keys to navigate iswitch buffer (along with C-s C-r)
(require 'edmacro)
(defun iswitchb-local-keys ()
  (mapc (lambda (K) 
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; Handle uniqify buffer renaming
(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
    set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (iswitchb-rescan))
(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))


;; ----------------------------------------------------------------------- ;;
;; uniqify (better duplicate buffer naming)
;; ----------------------------------------------------------------------- ;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse) ; place info after buffer name
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


;; ----------------------------------------------------------------------- ;;
;; Tool-Bar+ (easy hiding and showing of toolbar)
;; ----------------------------------------------------------------------- ;;

(require 'tool-bar+)
(tool-bar-pop-up-mode 1)

;; ----------------------------------------------------------------------- ;;
;; full-ack mode (use ack source code grep tool within emacs)
;; ----------------------------------------------------------------------- ;;

;; These auto-load the ack file whenever we run the command
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; ----------------------------------------------------------------------- ;;
;; Latex
;; ----------------------------------------------------------------------- ;;

(add-hook 'latex-mode-hook (lambda () (longlines-mode +1)))


;; ----------------------------------------------------------------------- ;;
;; Specials for X Window System
;; ----------------------------------------------------------------------- ;;
(if (not window-system)
      ;; ispell
      ;; ----------------------
      ;; (A few changes on ispell)
      (setq ispell-highlight-face 'underline);)

  ;;
  ;; Some dialog
  ;; ------------------
  (setq use-dialog-box t)
  ;;
  ;; less dialog
  ;; -----------
  ;(menu-prompting nil)
  ;; 
  ;; Set X synchrone
  ;; ---------------
  ;; Speed up
  (setq mouse-scroll-delay 0)
  (setq x-selection-timeout 0)

  ;; Have cut and paste work properly
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
)

;; ----------------------------------------------------------------------- ;;
;; Custom Set Variables
;; ----------------------------------------------------------------------- ;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(inhibit-startup-screen t)
 '(matlab-functions-have-end t)
 '(matlab-shell-command "/opt/matlab/bin/matlab")
 '(matlab-verify-on-save-flag t)
 '(mlint-programs (quote ("mlint" "glnx86/mlint" "/opt/matlab/bin/glnx86/mlint")))
 '(safe-local-variable-values (quote ((save-place . t))))
 '(scroll-bar-mode (quote right)))