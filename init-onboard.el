;;; init-onboard.el --- Portable Emacs starter kit without 3rd-party packages


;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; URL: https://github.com/monkeyjunglejuice/emacs.onboard
;; Version: 0.1.0


;;; Commentary:


;; The goal of ONBOARD is to offer a clean slate to build your own Emacs config.
;; It stays as close as possible to vanilla Emacs, but offers some convenience
;; and a better user experience, while only relying on built-in packages.
;;
;; Tested with:
;;  [X] Emacs 27.1 on Ubuntu 20.04 LTS
;;  [X] Emacs 26.1 on Debian 10.9.0


;; KEYBINDINGS
;;
;; "F12"  Toggle between dark and light theme
;;
;; "M-x"  Show all commands
;;        – hold down the "Meta key" and press <x>
;;        – the "Meta key" is usually <Alt> on Linux/Windows and <Option> on Mac
;;
;; "C-g"  Get out! Press <Ctrl>-<g> to cancel whatever happens – or hit 3x <ESC>
;;
;;
;; Examples:
;;
;; "M-x goto-user-init-file" Visit main configuration file ('.emacs', 'init.el')
;; "M-x check-parens"        Check if all parens match (in Emacs Lisp code)
;; "M-x help"                Reach the ultimate help menu
;;
;; "C-h o" Place the cursor behind a keyword, function, variable or other symbol
;;         to issue the command `describe-symbol' via keybinding and
;;         view the symbol's documentation
;;
;; "M-;"   Comment/uncomment a selected piece of text or code


;;; Code:


;;; EARLY INIT ________________________________________________________________


;; Code that should run as early as possible and would normall reside
;; within '~/.emacs.d/early-init.el' for Emacs 27 and higher


;; Tune garbage collection

;; Show a message when garbage collection happens? Useful while tuning the GC
(setq garbage-collection-messages nil)

;; Temporary set a high value of 256 MB to trigger less garbage collections
;; during initialization. The Emacs default is a threshold of 800 KB
(setq gc-cons-threshold (* 256 1000000))

;; Then lower the threshold to 4 MB during normal operation to prevent longer
;; GC pauses, but still have it at a higher value than the default to
;; experience less mini-interruptions – eg. while scrolling larger buffers.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 4 1000000))))


;;; PACKAGE MANAGEMENT ________________________________________________________


;; Browse, select and install 3rd-party packages: "M-x list-packages"


(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))

  (when no-ssl (warn "Your version of Emacs does not support SSL connections!"))

  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))

;; `package-initialize' seems not needed any more with Emacs 27?
(when (< emacs-major-version 27)
  (package-initialize))


;; GNU TLS connection issue workaround
(require 'gnutls)
(if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))


;; Highlight lines in the package manager
(add-hook 'package-menu-mode-hook
          (lambda ()
            (hl-line-mode 1)))


;;; SYSTEM ____________________________________________________________________


;; Prevent stale elisp bytecode from shadowing more up-to-date source files
(setq load-prefer-newer t)


;; Increase warning threshold
(setq large-file-warning-threshold (* 64 1000000))


;; Set undo limit to 64 MB
(setq undo-outer-limit (* 64 1000000))


;; Increase the amount of data which Emacs reads from subprocesses
(setq read-process-output-max (* 1024 1024)) ; 1 MB


;; Emacs knows where your init file is (open and edit '.emacs' or 'init.el')
(defun goto-user-init-file ()
  "Visit the init file."
  (interactive)
  (find-file user-init-file))

;; Open to the '~/.emacs.d' directory in the Dired file manager
(defun goto-user-emacs-directory ()
  "Open the Emacs directory in Dired."
  (interactive)
  (dired user-emacs-directory))


;; Diagnostics
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s with %d garbage collections."
                     (format "%.3f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


;;; SERVER ____________________________________________________________________


;; Running Emacs as a daemon: "M-x info-emacs-manual" <s> server <RET>


(require 'server)


;; Display the name of the Emacs server process in the frame title
;; to see easily to which server process a client is connected to
;; Further information:
;; https://monkeyjunglejuice.github.io/blog/emacs-server-name-frame-title.howto.html

(defun my-frame-title ()
  "Set a custom frame title."
  (setq frame-title-format
        (concat "%b (%f)"
                (when (server-running-p)
                  (concat " " server-name)))))

(add-hook 'emacs-startup-hook
          (lambda ()
            "Run functions after loading init files"
            (my-frame-title)))

(add-hook 'server-mode-hook
          (lambda ()
            "Run functions after entering or leaving 'server-mode'."
            (my-frame-title)))


;; Shutdown Emacs server process

(defun server-stop ()
  "Save buffers, quit and shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))


;;; FONTS _____________________________________________________________________


;; The height value is in 1/10 pt, so 120 will give 12 pt

;; Set the default monospaced font
(set-face-attribute 'default nil
                    ;; :family "IBM Plex Mono" ; set the font name
                    :slant  'normal
                    :weight 'normal
                    :width  'normal
                    :height 120)

;; Set the proportional font (toggle by "M-x variable-pitch-mode")
(set-face-attribute 'variable-pitch nil
                    ;; :family "Crimson Pro" ; set the font name
                    :slant  'normal
                    :weight 'normal
                    :width  'normal
                    :height 160)


;; Set the modeline fonts. This function must be called to have any effect:
;; Uncomment `load-after-theme-light-hook' and `load-after-theme-light-hook'
;; further down under "Theme configuration".

(defun my-modeline ()
  "Custom modeline styling."
  (set-face-attribute 'mode-line nil
                      ;; :family "IBM Plex Mono" ; inherited from default
                      :width  'normal
                      :height 100)
  (set-face-attribute 'mode-line-inactive nil
                      ;; :family "IBM Plex Mono" ; inherited from default
                      :width  'normal
                      :height 100))


;;; TOGGLE THEME ______________________________________________________________


;; Default/fallback definitions – don't change them here,
;; but scroll further down to "Theme configuration"

(defgroup toggle-theme nil
  "Toggle between light and dark theme with a single key press."
  :group 'convenience)

(defcustom light-theme-name 'leuven
  "Name of the light theme."
  :group 'toggle-theme
  :type 'symbol)

(defcustom dark-theme-name 'wombat
  "Name of the dark theme."
  :group 'toggle-theme
  :type 'symbol)

(defcustom default-theme-variant 'dark
  "Load either the 'light or the 'dark theme at startup?"
  :group 'toggle-theme
  :type 'symbol)

(defvar active-theme-variant nil
  "Holds the information about the currently active theme variant.")

(defcustom load-before-theme-light-hook nil
  "Run before loading the light theme."
  :group 'toggle-theme
  :type 'hook)

(defcustom load-after-theme-light-hook nil
  "Run after loading the light theme."
  :group 'toggle-theme
  :type 'hook)

(defcustom load-before-theme-dark-hook nil
  "Run before loading the dark theme."
  :group 'toggle-theme
  :type 'hook)

(defcustom load-after-theme-dark-hook nil
  "Run after loading the dark theme."
  :group 'toggle-theme
  :type 'hook)

(defun load-theme-light ()
  "Load the light theme and apply some modifications."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (run-hooks 'load-before-theme-light-hook)
  (load-theme light-theme-name t)
  (setq active-theme-variant 'light)
  (run-hooks 'load-after-theme-light-hook))

(defun load-theme-dark ()
  "Load the dark theme and apply some modifications."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (run-hooks 'load-before-theme-dark-hook)
  (load-theme dark-theme-name t)
  (setq active-theme-variant 'dark)
  (run-hooks 'load-after-theme-dark-hook))

(defun toggle-theme ()
  "Toggle between light and dark theme."
  (interactive)
  (cond ((equal active-theme-variant 'light) (load-theme-dark))
        ((equal active-theme-variant 'dark) (load-theme-light))
        (t (mapc #'disable-theme custom-enabled-themes))))

(defun load-theme-default ()
  "Load the default theme."
  (cond
   ((equal default-theme-variant 'light) (load-theme-light))
   ((equal default-theme-variant 'dark) (load-theme-dark))
   (t (message
       "Toggle theme: DEFAULT-THEME-VARIANT must be either 'light or 'dark"))))


;; Theme configuration ********************************************************

;; Set the light theme here:
(setq light-theme-name 'leuven)

;; Set the dark theme here:
(setq dark-theme-name 'wombat)

;; Set the default variant here, either 'light or 'dark:
(setq default-theme-variant 'dark)

;; Set the keybinding to toggle between light and dark:
(global-set-key (kbd "<f12>") #'toggle-theme)

;; The hooks can be used to run additional functions before or after loading
;; the selected light or dark theme. Useful to set variables that otherwise
;; get overwritten by the themes; for instance the font size of the
;; modeline, which is often explicitly set by the themes themselves.
;; The hooks can also be configured via "M-x customize-group RET toggle-theme"

;; (add-hook 'load-after-theme-light-hook
;;           (lambda ()
;;             (my-modeline)))

;; (add-hook 'load-after-theme-dark-hook
;;           (lambda ()
;;             (set-background-color "#000000") ; example
;;             (set-cursor-color "red3") ; example
;;             (my-modeline)))

;; ****************************************************************************


;; Load the theme eventually
(load-theme-default)


;;; USER INTERFACE ____________________________________________________________


;; Default frame settings
(setq default-frame-alist '())

;; Don't set frame size or position and start Emacs maximized instead
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set the default width of the Emacs frame in characters
(add-to-list 'default-frame-alist '(width . 80))

;; Set the default height of the Emacs frame in characters
(add-to-list 'default-frame-alist '(height . 24))

;; Set the distance from the left screen edge – x-axis
(add-to-list 'default-frame-alist '(left . 0))

;; Set the distance from the top screen edge – y-axis
(add-to-list 'default-frame-alist '(top . 0))

;; Set the cursor type
;; To learn about available cursors, place your cursor behind 'cursor-type' in the
;; code below and hit "C-h o" or "M-x describe-symbol" <RET> cursor-type <RET>
(add-to-list 'default-frame-alist '(cursor-type . bar))

;; Enable/disable Cursor blinking?
(blink-cursor-mode 1) ; -1 means 'off'; 1 means 'on'

;; Cursor blinking interval in seconds
(setq blink-cursor-interval 0.4)

;; Make the cursor stand out?
(setq visible-cursor nil)

;; Display/hide menu bar by default?
(menu-bar-mode 1)

;; Toggle menu bar visibility by keybinding
(global-set-key (kbd "C-c b") #'menu-bar-mode)

;; Display/hide the tool bar?
(require 'tool-bar)
(tool-bar-mode -1)

;; Display/hide the scroll bar?
(require 'scroll-bar)
(scroll-bar-mode -1)

;; Enable/disable tooltips?
(require 'tooltip)
(tooltip-mode -1)

;; No startup screen
(setq inhibit-startup-screen t)

;; Turn off alarms completely
(setq ring-bell-function 'ignore)


;; Smooth scrolling
(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))
              mouse-wheel-progressive-speed nil
              mouse-wheel-follow-mouse t
              scroll-conservatively 10000
              scroll-step 1
              scroll-margin 1
              scroll-preserve-screen-position 1)


;; Pinentry
(require 'epg-config)
(setq epg-pinentry-mode 'loopback)


;;; MODELINE / ECHO AREA / MINIBUFFER _________________________________________


;; Use the minibuffer instead of dialog boxes
(setq use-dialog-box nil)

;; Change all yes/no style questions to y/n style
(fset 'yes-or-no-p 'y-or-n-p)

;; Show the buffer size in the modeline
(size-indication-mode 1)

;; Show column number along with line number in modeline
(column-number-mode 1)


;;; MINIBUFFER COMPLETION _____________________________________________________


(require 'icomplete)

(setq icomplete-in-buffer t
      icomplete-compute-delay 0
      icomplete-delay-completions-threshold 1000
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil)


;; Display completions vertically, using the newline separator '\n'
;; There's a better way than the newline-separator:
;; --> recommended 3rd-party package 'icomplete-vertical'
(setq icomplete-separator "\n")

;; Provide some intuitive keybindings and make the display area higher
;; when vertical completion is set
(if (string= icomplete-separator "\n")
    (progn
      (setq icomplete-prospects-height 10)
      (define-key icomplete-minibuffer-map
        (kbd "<down>") #'icomplete-forward-completions)
      (define-key icomplete-minibuffer-map
        (kbd "<up>") #'icomplete-backward-completions)
      (define-key icomplete-minibuffer-map
        (kbd "C-n") #'icomplete-forward-completions)
      (define-key icomplete-minibuffer-map
        (kbd "C-p") #'icomplete-backward-completions)))

;; Enable autocompletion
(icomplete-mode 1)

;; Make Icomplete better with Fido-mode (Emacs version 27 and higher)
(when (>= emacs-major-version 27)
  (fido-mode 1))


;; Enhance M-x to allow easier execution of commands
;; --> recommended 3rd-party package 'amx'
;; (global-set-key (kbd "M-x") #'amx)
;; (global-set-key (kbd "M-X") #'amx-major-mode-commands)


;;; WINDOW MANAGEMENT _________________________________________________________


;; Fix the default window splitting and placement
;; If this is a step too far, then replace
;; `display-buffer-same-window' with `display-buffer-pop-up-window',
;; or comment out the following expression and restart Emacs
;; to experience the default window behavior

(setq display-buffer-alist
      '((".*" (display-buffer-reuse-window display-buffer-same-window))))


;; Default window navigation – simply switch to the next window in order
;; Added for convenience; default key binding is "C-x o"
(global-set-key (kbd "M-o") #'other-window)


;; Navigate windows by direction
;; (require 'windmove)
;; (setq windmove-wrap-around nil)
;; (global-set-key (kbd "s-j") #'windmove-down)
;; (global-set-key (kbd "s-k") #'windmove-up)
;; (global-set-key (kbd "s-h") #'windmove-left)
;; (global-set-key (kbd "s-l") #'windmove-right)


;; Display-buffer: avoid resizing
(setq even-window-sizes nil)


;; Focus follows mouse
(setq mouse-autoselect-window nil
      focus-follows-mouse nil)


;; Undo/redo window configurations
(require 'winner)
(winner-mode 1)
(define-key winner-mode-map (kbd "C-x 4 u") #'winner-undo)
(define-key winner-mode-map (kbd "C-x 4 r") #'winner-redo)


;;; BUFFERS ___________________________________________________________________


;; Switch to another buffer (those are like tabs – when multiple files are open)
;; Added for convenience; default keybinding is "C-x b"
(global-set-key (kbd "M-SPC") #'switch-to-buffer)


;; Uniquify buffer names for identically-named files
(setq uniquify-buffer-name-style 'forward)


;; Ibuffer – show all buffers in a list and do stuff with them
(require 'ibuf-ext)
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)))

(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Kill the current buffer immediately instead of presenting a selection
(global-set-key (kbd "C-x k") #'kill-current-buffer)


;; Kill all buffers at once

(defun kill-all-buffers ()
  "Close all buffers at once."
  (interactive)
  (save-some-buffers)
  (let ((kill-buffer-query-functions '()))
    (mapc #'kill-buffer (buffer-list))))

(global-set-key (kbd "C-x K") 'kill-all-buffers)


;;; SCRATCH BUFFER ____________________________________________________________


;; Empty *scratch* buffer content at startup
(setq initial-scratch-message "")

;; Set the initial mode of the *scratch* buffer
(setq initial-major-mode #'org-mode)

;; Quickly jump to *scratch* buffer
(defun scratch ()
  "Jump to the *scratch* buffer. If it does not yet exist, create it."
  (interactive)
  (switch-to-buffer "*scratch*"))

(global-set-key (kbd "C-c s c") #'scratch)


;;; CLIPBOARD, COPY & PASTE ___________________________________________________


(require 'select)
(setq
 ;; Use clipboard
 select-enable-clipboard t
 ;; Use primary selection: mark=copy / middle-click=paste
 select-enable-primary t
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t
 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)


;; Allow Emacs to copy to and paste from the GUI clipboard
;; when running in a text terminal
;; --> recommended 3rd-party package 'xclip'
;; (xclip-mode 1)


;; Copy the file name (path) of the current file

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer's file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard."
               filename))))


;; Simple alternative for 'yank-pop'

(defun insert-kill-ring-item ()
  "Select and insert an item from the 'kill-ring'."
  (interactive)
  (insert (completing-read "Yank: " kill-ring nil t)))

(global-set-key (kbd "M-y") #'insert-kill-ring-item)


;;; BACKUP ____________________________________________________________________


;; Make backups of all edited files before saving them

(setq backup-by-copying t
      kept-new-versions 5
      kept-old-versions 1
      delete-old-versions t
      version-control t)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "var/backup/"))))


;;; LOCKFILES _________________________________________________________________


;; Let Emacs keeping track of files currently open
(setq create-lockfiles t)


;;; AUTO-SAVE _________________________________________________________________


(setq auto-save-default t
      auto-save-interval 0)

(setq auto-save-list-file-prefix
      (concat user-emacs-directory "var/auto-save/sessions/"))


;;; HELP ______________________________________________________________________


;; Show all options when running 'apropos' (fulltext search) "C-h a"
(require 'apropos)
(setq apropos-do-all t)


;;; SEARCH ____________________________________________________________________


;; Switch search functions to make regex-search the default

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'replace-regexp)


;;; RECENT FILES ______________________________________________________________


(require 'recentf)


(setq recentf-max-menu-items 10
      recentf-max-saved-items 100)

(setq recentf-save-file
      (concat user-emacs-directory "var/recentf-save.el"))


;; Turn on recent file mode to visit recently edited files
(recentf-mode 1)


;; Ignore some recently visitet files

(add-to-list 'recentf-exclude
             (concat user-emacs-directory "elpa/"))

(add-to-list 'recentf-exclude
             (concat user-emacs-directory "var/"))


;; Use 'completing-read' to choose between recent files

(defun find-recentf ()
  "Find recent file via completion in the minibuffer."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list nil t) nil))

(global-set-key (kbd "C-x f") #'find-recentf)


;;; DIRED _____________________________________________________________________


(require 'dired)


;; Use the system trash when deleting files

(defun trash-on ()
  "Delete files by moving to the system trash."
  (interactive)
  (setq delete-by-moving-to-trash t)
  (setq dired-recursive-deletes 'always) ; don't ask when directory not empty
  (message "Trash on: Deleted files will go to system trash."))

(defun trash-off ()
  "Delete files immediately."
  (interactive)
  (setq delete-by-moving-to-trash nil)
  (setq dired-recursive-deletes 'top) ; ask when directory not empty
  (message "Trash off: Files will be deleted immediately!"))

(trash-on) ; make it the default


;; Copying files/directories
(setq dired-recursive-copies 'always)


;; Auto refresh dired when contents of a directory change
(require 'autorevert)
(add-hook 'dired-mode-hook #'auto-revert-mode)
(setq auto-revert-verbose nil)


;; Short file listing by default; toggle detailled listing via "s-("
(defun dired-hide-details ()
   "Hide details for file listings per default."
   (dired-hide-details-mode 1))

(add-hook 'dired-mode-hook #'dired-hide-details)


;; Mimic dual-pane file managers
(setq dired-dwim-target t)


;; Image
(require 'image-dired)
(setq image-dired-thumb-margin 0
      image-dired-thumb-relief 0)


;; Directory listing columns; Switch arguments with "C-u s"
;; Show all files: -lhFA and hide backups with an additional -B

(defun dired-hide-dotfiles ()
  "Hide dotfiles in Dired."
  (interactive)
  (setq-default dired-listing-switches "--group-directories-first -lhF"))

(defun dired-show-dotfiles ()
  "Show dotfiles in Dired."
  (interactive)
  (setq-default dired-listing-switches "--group-directories-first -lhFA"))

(dired-show-dotfiles) ; set the default


;; Linux/Unix only
(defun dired-xdg-open ()
  "Open files and folders with the default desktop app."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(define-key dired-mode-map (kbd "M-RET") #'dired-xdg-open)


;; Goto home directory
(defun dired-home ()
  "Show the home directory in Dired."
  (interactive)
  (dired "~/"))


;; Set new keybinding resembling "C-x C-f" for visiting files
;; Added for convenience; default key binding is "C-x d"
(global-set-key (kbd "C-x C-d") #'dired)


;;; ESHELL ____________________________________________________________________


;; Eshell is *not* a terminal emulator, but a *shell* equivalent to Bash or Fish
;; that runs within Emacs. It is independent from the OS. Eshell looks like
;; a Posix shell superficially, but is also a REPL for Emacs Lisp expressions.


(require 'esh-mode)

;; Get rid of the Eshell startup message
(require 'em-banner)
(setq eshell-banner-message "")

;; List directory content after changing into it
(require 'em-dirs)
(setq  eshell-list-files-after-cd t)

;; To open more than one eshell buffer: "C-u C-c e e"
(global-set-key (kbd "C-c e e") #'eshell)


;;; SHELL _____________________________________________________________________


;; Issue shell commands and display their output


;; Set another shell than the default one
;; (setq shell-file-name "/usr/bin/bash")

;; To open more than one shell buffer: "C-u C-c s s"
(global-set-key (kbd "C-c s s") #'shell)


;;; PROCED ____________________________________________________________________


;; Show and manage OS processes


(require 'proced)
(setq proced-auto-update-interval 1)
(setq-default proced-auto-update-flag t
              proced-descend t)


;; BUILT-IN WEB BROWSER _______________________________________________________


;; Eww – Emacs Web Wowser


;; Pretend to be an iPhone
;; (setq url-user-agent
;;       "Mozilla/5.0 (iPhone; CPU iPhone OS 13_6_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/13.1.2 Mobile/15E148 Safari/604.1")

;; Or pretend to be the W3m text-mode browser
(setq url-user-agent "w3m/0.5.3+git20190105")

(setq url-privacy-level '(email lastloc os cookies emacs))
(url-setup-privacy-info)


;;; STANDARD WEB BROWSER ______________________________________________________


;; This can be the system-wide graphical web browser, but also a web browser
;; within Emacs. Other browse functions exist besides `browse-url'

;; Browse URL with standard web browser
(global-set-key (kbd "C-c w w") #'browse-url)


;;; EMAIL HANDLING ____________________________________________________________


;; TODO: Send emails directly from Emacs using SMTP – example template

;; Should be defined first
;; (setq user-mail-address "mail@example.org")

;; To avoid typing in the password for each email, specify SMTP account(s)
;; in '~/.authinfo.gpg'. Here's a content template for authinfo.gpg:
;; machine mail.example.org port 587 login myuser password mypassword


;; Emacs email variables

(require 'smtpmail)
(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "localhost"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 1025 ; default port: 587
      smtpmail-queue-dir "~/.mail/queued-mail/"
      smtpmail-smtp-user user-mail-address
      smtpmail-debug-info nil)


(require 'message)
(setq message-kill-buffer-on-exit t)


;;; ORG-MODE __________________________________________________________________


(require 'org)


;; Set the Org paths
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))


(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))


;; Capture: put newer entries on top
(setq org-reverse-note-order t)
(global-set-key (kbd "C-c o c") #'org-capture)


;; Agenda
(setq org-agenda-files (list org-directory))
(global-set-key (kbd "C-c o a") #'org-agenda)


;; Links
(global-set-key (kbd "C-c o l") #'org-store-link)
(define-key org-mode-map (kbd "C-c l") #'org-toggle-link-display)


;; Literate programming – activate code blocks via Babel languages
(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)))


(defun goto-org-directory ()
  "Show the Org directory in Dired."
  (interactive)
  (dired org-directory))

(global-set-key (kbd "C-c o d") #'goto-org-directory)


(defun find-org-file ()
  "Find Org file via completion in the minibuffer."
  (interactive)
  (find-file org-directory))

(global-set-key (kbd "C-c o f") #'find-org-file)


(defun goto-org-notes ()
  "Visit the Org main file."
  (interactive)
  (find-file (concat org-directory "notes.org")))

(global-set-key (kbd "C-c o o") #'goto-org-notes)


;;; EDITING ___________________________________________________________________


;; UTF-8
(prefer-coding-system 'utf-8)


;; General text settings
(add-hook 'text-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)
            (visual-line-mode 1)
            (hl-line-mode 1)))


;; Set desired line width
(setq-default fill-column 80)


;; Final new line
(setq require-final-newline t)


;; Sentences end with a single space
(setq sentence-end-double-space nil)


;; Better than the default 'just-one-space', which was M-SPC before
(global-set-key (kbd "S-SPC") #'cycle-spacing)


;; Count lines, words and chars (buffer or region)
(global-set-key (kbd "C-x l") #'count-words)


;; When re-visiting a file, the cursor goes
;; to the last place where it was before
(require 'saveplace)
(save-place-mode 1)
(setq save-place-file (concat user-emacs-directory "var/save-place.el"))


;; More useful than the default
(global-set-key (kbd "M-z") 'zap-up-to-char)


;; Typing a character while a text selection is active,
;; deletes the selection and replaces it with the typed character,
;; which is mostly common when working with text
;; (delete-selection-mode 1)


;;; PROGRAMMING _______________________________________________________________


;; General programming settings
(add-hook 'prog-mode-hook
          (lambda ()
            ;; (linum-mode 1) ; line numbers on=1 or off=-1 / "M-x linum-mode"
            ;; (electric-pair-local-mode 1) ; auto-close parens and brackets
	          (setq show-trailing-whitespace t)))


;; Indentation
(setq-default indent-tabs-mode nil ; don't use tabs for indentation
              tab-width 2)         ; set display width for tab characters


;; Parenthesis settings
(require 'paren)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
;; (setq show-paren-delay 0)


;; Backspace deletes the whole indentation instead of one-by-one
;; Possibly shadowed by 3rd-party packages like 'smartparens-mode'
(setq backward-delete-char-untabify-method 'hungry)


;; Additional keybinding to resemble other S-expression related keybindings
;; who begin usually with C-M
(global-set-key (kbd "<C-M-backspace>") #'backward-kill-sexp)


;;; ELISP _____________________________________________________________________


;; Essential Emacs Lisp setup
;; --> recommended 3rd-party packages:
;; 'rainbow-delimiters', 'company', 'flycheck', 'paren-face', 'paredit'

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (rainbow-delimiters-mode-enable)
;;             (company-mode 1)
;;             (flycheck-mode 1)
;;             (paren-face-mode 1)
;;             (paredit-mode 1)))

;; (add-hook 'lisp-interaction-mode-hook
;;           (lambda ()
;;             (rainbow-delimiters-mode-enable)
;;             (company-mode 1)
;;             (flycheck-mode -1)
;;             (paren-face-mode 1)
;;             (paredit-mode 1)))

;; (add-hook 'ielm-mode-hook
;;           (lambda ()
;;             (rainbow-delimiters-mode-enable)
;;             (company-mode 1)
;;             (paren-face-mode 1)
;;             (paredit-mode 1)))


;;; HTML/CSS __________________________________________________________________


;; CSS
(require 'css-mode)
(setq css-indent-offset 2)


;;; ___________________________________________________________________________
(provide 'init-onboard)
;;; init-onboard.el ends here
