;;; init-onboard.el --- Emacs ONBOARD Starter Kit  -*- lexical-binding: t; -*-

;; Copyright (C) 2021–2022 Dan Dee

;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; URL: https://github.com/monkeyjunglejuice/emacs.onboard
;; Version: 0.3.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience
;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:
;; The goal of Emacs ONBOARD is to offer a clean slate to build your personal
;; Emacs config. It stays as close as possible to vanilla Emacs, but offers some
;; convenience and a better user experience, while only relying
;; on built-in packages.

;;; Tested with:
;;  [X] Emacs 28.1 from Guix
;;  [X] Emacs 27.2 on Ubuntu 20.04 LTS
;;  [X] Emacs 27.1 on Ubuntu 20.04 LTS
;;  [X] Emacs 26.1 on Debian 10.9.0

;;; Keybindings:
;;
;; "M-x"  Show all commands
;;        – hold down the "Meta key" and press <x>
;;        – the "Meta key" is usually <Alt> on Linux/Windows and <Option> on Mac
;;
;; "C-g"  Get out! Press <Ctrl>+<g> to cancel whatever happens – or hit 3x <ESC>
;;
;; "F12"  Toggle between dark and light theme

;;; Examples:
;;
;; "M-x onb-"                Show all commands defined in this file
;; "M-x onb-user-init-file"  Visit main configuration file – .emacs or init.el
;; "M-x check-parens"        Check if all parens match in Emacs Lisp code
;; "M-x help"                Reach the ultimate help menu
;;
;; "C-h o" Place the cursor behind a keyword, function, variable or other symbol
;;         to issue the command `describe-symbol' via keybinding
;;         and view the symbol's documentation
;;
;; "M-;"   Comment/uncomment a selected piece of text or code

;;; Code:


;;; EARLY INIT ________________________________________________________________


;; Code that should run as early as possible and would normally reside
;; within '~/.emacs.d/early-init.el' for Emacs 27 and higher


;; Tune garbage collection

;; Temporarily set a high value of 256 MB to trigger less garbage collections
;; during initialization. The Emacs default is a threshold of 800 KB
(setq gc-cons-threshold (* 256 1000000))

;; Then lower the threshold to 16 MB during normal operation to prevent longer
;; GC pauses, but still have it at a higher value than the default to
;; experience less mini-interruptions – eg. while scrolling larger buffers.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1000000))))

;; Show a message when garbage collection happens? Useful while tuning the GC
(setq garbage-collection-messages nil)


;;; PACKAGE MANAGEMENT ________________________________________________________


;; Browse, select and install 3rd-party packages: "M-x list-packages RET"

(require 'package)

;; 1st priority
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; 2nd priority
;; Install form melpa-stable' only if a package from 'melpa' is broken
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Initialize packages
(package-initialize)


;; GNU TLS connection issue workaround for Emacs 26.3
(require 'gnutls)
(if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))


;; Highlight current line in the package manager
(add-hook 'package-menu-mode-hook
          (lambda ()
            (hl-line-mode 1)))


;; Helper function to install 3rd-party packages declaratively
(defun onb-ensure-packages (toggle package-list)
  "PACKAGE-LIST will be installed if 'yes is passed as an argument to TOGGLE.
When TOGGLE receives any other argument – eg. 'no or 'nope, then nothing will
happen. The purpose of this function is to make sure that certain Emacs Lisp
packages will be present on your system."
  (when (eq toggle 'yes)
    (mapc #'(lambda (package)
              (unless (package-installed-p package)
                (package-refresh-contents)
                (package-install package)))
          package-list)))

(defalias 'ont-ensure-packages 'onb-ensure-packages
  "Alias for the function 'onb-ensure-packages' from Emacs ONBOARD.")

;; Example: You can install suggested 3rd-party packages from within this file
;; with single expressions like this:
;;
;; (onb-ensure-packages 'yes '(the-matrix-theme))  ; <-- installs the package
;; (onb-ensure-packages 'no '(the-matrix-theme))   ; <-- does nothing (default)
;;
;; The actual installation will be performed when you restart Emacs
;; or evaluate the function manually – eg. via pressing "C-M-x"
;; while the cursor is placed somewhere inside the function.


;;; SYSTEM ____________________________________________________________________


;; Prevent stale elisp bytecode from shadowing more up-to-date source files
(setq load-prefer-newer t)


;; Increase warning threshold
(setq large-file-warning-threshold (* 64 1000000))

;; Set undo limit to 64 MB
(setq undo-outer-limit (* 64 1000000))

;; Increase the amount of data which Emacs reads from subprocesses
(setq read-process-output-max (* 1024 1024)) ; 1 MB

;; Diagnostics
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s with %d garbage collections."
                     (format "%.3f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


;; Helpers to simplify writing operating system specific code

(defun onb-linp ()
  "True if `system-type' is Linux or something compatible.
For finer granularity, use the variables `system-type'
or `system-configuration' directly."
  (string= system-type (or "gnu/linux" "berkeley-unix" "gnu" "gnu/kfreebsd")))

(defun onb-winp ()
  "True if `system-type' is Windows or something compatible.
For finer granularity, use the variables `system-type'
or `system-configuration' directly."
  (string= system-type (or "windows-nt" "cygwin" "ms-dos")))

(defun onb-macp ()
  "True if `system-type' is MacOS or something compatible.
For finer granularity, use the variables `system-type'
or `system-configuration' directly."
  (string= system-type "darwin"))


;; Emacs knows where your init file is (open and edit '.emacs' or 'init.el')
(defun onb-user-init-file ()
  "Visit the init file."
  (interactive)
  (find-file user-init-file))

;; Open to the '~/.emacs.d' directory in the Dired file manager
(defun onb-user-emacs-directory ()
  "Open the Emacs directory in Dired."
  (interactive)
  (dired user-emacs-directory))


;;; SERVER ____________________________________________________________________


;; Running Emacs as a daemon: "M-x info-emacs-manual" <s> server <RET>

(require 'server)

;; Display the name of the Emacs server process in the frame title
;; to see easily to which server process a client is connected to
;; Further information:
;; https://monkeyjunglejuice.github.io/blog/emacs-server-name-frame-title.howto.html

(defun onb-frame-title ()
  "Set a custom frame title."
  (setq frame-title-format
        (concat "%b (%f)"
                (when (server-running-p)
                  (concat " " server-name)))))

(add-hook 'emacs-startup-hook
          (lambda ()
            "Run functions after loading init files"
            (onb-frame-title)))

(add-hook 'server-mode-hook
          (lambda ()
            "Run functions after entering or leaving 'server-mode'."
            (onb-frame-title)))


;; Shutdown Emacs server process
(defun onb-server-stop ()
  "Save buffers, quit and shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))


;;; FONTS _____________________________________________________________________


(defun onb-fonts ()
  "The height value is in 1/10 pt, so 130 will give 13 pt."
  ;; Set the default monospaced font
  (set-face-attribute 'default nil
                      ;; :family "IBM Plex Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 130)
  ;; Set an alternative monospaced font (optional)
  (set-face-attribute 'fixed-pitch nil
                      ;; :family "IBM Plex Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 130)
  ;; Set another alternative monospaced fonts, preferably with serifs (optional)
  (set-face-attribute 'fixed-pitch-serif nil
                      ;; :family "IBM Plex Mono"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 130)
  ;; Set the proportional font (toggle by "M-x variable-pitch-mode")
  (set-face-attribute 'variable-pitch nil
                      ;; :family "Crimson Pro"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 150))


;; Set the modeline fonts. This function will be called later to have an effect:
;; Uncomment `onb-load-after-theme-light-hook' and `onb-load-after-theme-light-hook'
;; further down under "Theme configuration".

(defun onb-modeline ()
  "Custom modeline styling."
  (set-face-attribute 'mode-line nil
                      ;; :family "IBM Plex Mono"  ; inherited from `default'
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 100)
  (set-face-attribute 'mode-line-inactive nil
                      ;; :family "IBM Plex Mono"  ; inherited from `default'
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 100))


;;; TOGGLE THEME ______________________________________________________________


;; Default/fallback definitions – don't change them here,
;; but scroll further down to "THEME CONFIG"

(defgroup onb-toggle-theme nil
  "Toggle between light and dark theme with a single key press."
  :group 'convenience)

(defcustom onb-light-theme-name 'modus-operandi
  "Name of the light theme."
  :group 'toggle-theme
  :type 'symbol)

(defcustom onb-dark-theme-name 'modus-vivendi
  "Name of the dark theme."
  :group 'toggle-theme
  :type 'symbol)

(defcustom onb-default-theme-variant 'dark
  "Load either the 'light or the 'dark theme at startup?"
  :group 'toggle-theme
  :type 'symbol)

(defvar onb-active-theme-variant nil
  "Holds the information about the currently active theme variant.")

(defcustom onb-load-before-theme-light-hook nil
  "Run before loading the light theme."
  :group 'toggle-theme
  :type 'hook)

(defcustom onb-load-after-theme-light-hook nil
  "Run after loading the light theme."
  :group 'toggle-theme
  :type 'hook)

(defcustom onb-load-before-theme-dark-hook nil
  "Run before loading the dark theme."
  :group 'toggle-theme
  :type 'hook)

(defcustom onb-load-after-theme-dark-hook nil
  "Run after loading the dark theme."
  :group 'toggle-theme
  :type 'hook)

(defun onb-load-theme-light ()
  "Load the light theme and apply some modifications."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (run-hooks 'load-before-theme-light-hook)
  (load-theme onb-light-theme-name t)
  (setq onb-active-theme-variant 'light)
  (run-hooks 'onb-load-after-theme-light-hook))

(defun onb-load-theme-dark ()
  "Load the dark theme and apply some modifications."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (run-hooks 'load-before-theme-dark-hook)
  (load-theme onb-dark-theme-name t)
  (setq onb-active-theme-variant 'dark)
  (run-hooks 'onb-load-after-theme-dark-hook))

(defun onb-toggle-theme ()
  "Toggle between light and dark theme."
  (interactive)
  (cond
   ((equal onb-active-theme-variant 'light) (onb-load-theme-dark))
   ((equal onb-active-theme-variant 'dark) (onb-load-theme-light))
   (t (mapc #'disable-theme custom-enabled-themes))))

(defun onb-load-theme-default ()
  "Load the default theme."
  (cond
   ((equal onb-default-theme-variant 'light) (onb-load-theme-light))
   ((equal onb-default-theme-variant 'dark) (onb-load-theme-dark))
   (t (message
       "Toggle theme: DEFAULT-THEME-VARIANT must be either 'light or 'dark"))))

;; \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

;;; THEME CONFIG

;; Set the light theme:
(cond
 ((>= emacs-major-version 28) (setq onb-light-theme-name 'modus-operandi))
 ((<= emacs-major-version 27) (setq onb-light-theme-name 'leuven)))

;; Set the dark theme:
(cond
 ((>= emacs-major-version 28) (setq onb-dark-theme-name 'modus-vivendi))
 ((<= emacs-major-version 27) (setq onb-dark-theme-name 'wombat)))

;; Set the default variant here, either 'light or 'dark:
(setq onb-default-theme-variant 'dark)

;; Set the keybinding to toggle between light and dark:
(global-set-key (kbd "<f12>") #'onb-toggle-theme)

;; The hooks can be used to run additional functions before or after loading
;; the selected light or dark theme. Useful to set variables that otherwise
;; get overwritten by the themes; for instance the font size of the
;; modeline, which is often explicitly set by the themes themselves.
;; The hooks can also be configured via "M-x customize-group RET toggle-theme"

(add-hook 'onb-load-after-theme-light-hook
          (lambda ()
            (onb-fonts)
            (onb-modeline)))

(add-hook 'onb-load-after-theme-dark-hook
          (lambda ()
            (onb-fonts)
            (onb-modeline)))

;; ////////////////////////////////////////////////////////////////////////////

;; Load the theme eventually
(onb-load-theme-default)


;;; USER INTERFACE ____________________________________________________________


;; Default frame settings – start with an empty alist
(setq default-frame-alist '())

;; Either start Emacs maximized …
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; … or set the default width of the Emacs frame in characters
(add-to-list 'default-frame-alist '(width . 80))

;; … and set the default height of the Emacs frame in lines
(add-to-list 'default-frame-alist '(height . 24))

;; Horizontal position: set the distance from the left screen edge in pixels
;; (add-to-list 'default-frame-alist '(left . 0))

;; Vertical position: set the distance from the top screen edge in pixels
;; (add-to-list 'default-frame-alist '(top . 0))

;; Dont' show the fringe on that side
;; (add-to-list 'default-frame-alist '(right-fringe . 0))

;; Set the cursor type
;; To learn about available cursors, place your cursor behind 'cursor-type' in the
;; code below and hit "C-h o" or "M-x describe-symbol" <RET> cursor-type <RET>

;; Uncomment the next expression to change the curser to a blinking bar
;; (add-to-list 'default-frame-alist '(cursor-type . (bar . 2)))


;; Turn on/off cursor blinking by default?
(blink-cursor-mode -1) ; 1 means 'on' / -1 means 'off'

;; Cursor blinking interval in seconds
(setq blink-cursor-interval 0.4)

;; Emphasize the cursor when running Emacs in a text terminal?
(setq visible-cursor nil)

;; Make sure to highlight the current line only in the active window.
(setq hl-line-sticky-flag nil)


;; Turn the menu bar on/off by default?
(menu-bar-mode 1)

;; Turn the scroll bar on/off by default?
;; (if (fboundp 'scroll-bar-mode) ; Emacs 26.1 compatibility
;;     (scroll-bar-mode -1))

;; Turn the tool bar on/off by default?
(if (fboundp 'tool-bar-mode) ; Emacs 26.1 compatibility
    (tool-bar-mode -1))


;; Enable/disable tooltips?
(tooltip-mode -1)


;; Turn off the startup screen?
(setq inhibit-startup-screen t)


;; Turn off alarms?
(setq ring-bell-function 'ignore)


;; Smooth scrolling
(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))
              mouse-wheel-progressive-speed t
              mouse-wheel-follow-mouse t
              scroll-conservatively 10000
              scroll-step 1
              scroll-margin 0  ; leave n lines on both screen ends
              scroll-preserve-screen-position 1)


;; Pinentry
(require 'epg-config)
(setq epg-pinentry-mode 'loopback)


;;; MODELINE / ECHO AREA / MINIBUFFER _________________________________________


;; Delete duplicates from the command history
(setq history-delete-duplicates t)

;; Allow minibuffer commands while in the minibuffer
;; There are two commands to get out of recursive minibuffers:
;; "C-M-c" `exit-recursive-edit' and "C-]" `abort-recursive-edit'
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; Use the minibuffer instead of dialog boxes
(setq use-dialog-box nil)

;; Change all yes/no style questions to y/n style
(fset 'yes-or-no-p 'y-or-n-p)

;; Show the buffer size in the modeline
(size-indication-mode 1)

;; Show column number along with line number in modeline
(column-number-mode 1)


;;; ELDOC _____________________________________________________________________


(setq eldoc-echo-area-use-multiline-p t
      eldoc-minor-mode-string "")


;;; COMPLETION ________________________________________________________________


(require 'icomplete)

(setq icomplete-in-buffer t
      icomplete-compute-delay 0
      icomplete-delay-completions-threshold 1000
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil)

;; Emacs version 28 and later: vertical completion with fido-vertical
(if (>= emacs-major-version 28)
    (fido-vertical-mode 1))

;; Emacs version 27 and below: vertical completion with fido or icomplete
(if (<= emacs-major-version 27)
    (progn
      ;; Display completions vertically, using the newline separator '\n'
      (setq icomplete-separator "\n")
      ;; Provide intuitive keybindings
      (define-key icomplete-minibuffer-map (kbd "<down>")
        #'icomplete-forward-completions)
      (define-key icomplete-minibuffer-map (kbd "<up>")
        #'icomplete-backward-completions)
      (define-key icomplete-minibuffer-map (kbd "C-n")
        #'icomplete-forward-completions)
      (define-key icomplete-minibuffer-map (kbd "C-p")
        #'icomplete-backward-completions)
      ;; Turn on fido-mode, if available
      (if (fboundp #'fido-mode)
          (fido-mode 1)
        (icomplete-mode 1))))


;; Improve completion by remembering frequently used commands
;; --> recommended 3rd-party package 'amx'
;; If you would like to install the 3rd-party package(s), change 'no into 'yes
;; and evaluate the expression – either via "C-M-x", or simply restart Emacs:
(onb-ensure-packages 'no '(amx))
(when (fboundp #'amx)
  (global-set-key (kbd "M-x") #'amx)
  (global-set-key (kbd "M-X") #'amx-major-mode-commands))


;;; WINDOW MANAGEMENT _________________________________________________________


;; Emacs often opens buffers in new windows. Let's make window splitting
;; and placement more predictable. For the default windows behavior,
;; comment out the following expression and restart Emacs
(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-reuse-mode-window
         display-buffer-same-window
         display-buffer-in-previous-window)))


;; Default window navigation – simply switch to the next window in order
;; Added for convenience; default key binding is "C-x o"
(global-set-key (kbd "M-SPC") #'other-window)


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


;; Ibuffer – the buffer manager
(require 'ibuf-ext)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)))

(setq ibuffer-marked-face 'dired-marked)

(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Uniquify buffer names for identically-named files
(setq uniquify-buffer-name-style 'forward)


;; Kill the current buffer immediately instead of presenting a selection.
;; It's the equivalent to "close tab" in a web browser or other editors
(global-set-key (kbd "C-x k") #'kill-current-buffer)


;; Kill all buffers at once – equivalent to "close all tabs"

(defun kill-all-buffers ()
  "Close all buffers at once."
  (interactive)
  (save-some-buffers)
  (let ((kill-buffer-query-functions '()))
    (mapc #'kill-buffer (buffer-list))))

(global-set-key (kbd "C-x K") 'kill-all-buffers)


;; Get the buffer out of the way, but let it alive
(global-set-key (kbd "C-c k") #'bury-buffer)


;;; SCRATCH BUFFER ____________________________________________________________


;; Empty *scratch* buffer at startup
(setq initial-scratch-message "")

;; Set an initial major mode for the *scratch* buffer
;; (setq initial-major-mode #'lisp-interaction-mode)  ; default
(setq initial-major-mode #'org-mode)  ; for general writing and notes
;; (setq initial-major-mode #'fundamental-mode)  ; basic text mode

;; Quickly jump to the *scratch* buffer
(defun onb-scratch ()
  "Jump to the *scratch* buffer. If it does not exist, create it."
  (interactive)
  (switch-to-buffer "*scratch*"))

(global-set-key (kbd "C-c s s") #'onb-scratch)


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
;; If you would like to install the 3rd-party package(s), change 'no into 'yes
;; and evaluate the expression – either via "C-M-x", or simply restart Emacs:
(onb-ensure-packages 'no '(xclip))
(if (fboundp #'xclip-mode) (xclip-mode 1))


;; Copy the file name (path) of the current file

(defun onb-copy-file-name-to-clipboard ()
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

(defun onb-insert-kill-ring-item ()
  "Select and insert an item from the 'kill-ring'."
  (interactive)
  (insert (completing-read "Yank: " kill-ring nil t)))

(global-set-key (kbd "M-y") #'onb-insert-kill-ring-item)


;;; BACKUP ____________________________________________________________________


;; Make backups of all edited files before saving them
(setq backup-by-copying t
      kept-new-versions 10
      kept-old-versions 3
      delete-old-versions t
      version-control t)

;; Where to save the backups.
;; Specify file name/path patterns and directories ("REGEXP" . "DIRECTORY")
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/"))))


;;; LOCKFILES _________________________________________________________________


;; Let Emacs keep track of files currently visited
(setq create-lockfiles nil)


;;; AUTO-SAVE _________________________________________________________________


(setq auto-save-default nil
      auto-save-interval 0)


;;; HELP ______________________________________________________________________


;; Show all options when running 'apropos' (fulltext search) "C-h a"
(require 'apropos)
(setq apropos-do-all t)


;;; SEARCH ____________________________________________________________________


;; Switch search functions to make regex-search the default
(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)
(global-set-key (kbd "C-S-s") #'isearch-forward)
(global-set-key (kbd "C-S-r") #'isearch-backward)

;; Search and replace
(global-set-key (kbd "M-%") #'query-replace-regexp)
(global-set-key (kbd "C-M-%") #'replace-regexp)


;;; RECENT FILES ______________________________________________________________


(require 'recentf)

;; Turn on recent file mode to visit recently edited files
(recentf-mode 1)

(setq recentf-max-menu-items 10
      recentf-max-saved-items 1000)

;; Ignore some recently visited files,
;; eg. to prevent them from showing up amongst recent files after package upgrades
(add-to-list 'recentf-exclude
             (expand-file-name (concat user-emacs-directory "elpa/")))


;; Use 'completing-read' to choose between recent files

(defun onb-find-recentf ()
  "Find recent file via completion in the minibuffer."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list nil t) nil))

(global-set-key (kbd "C-x f") #'onb-find-recentf)


;;; DIRED _____________________________________________________________________


(require 'dired)


;; Use the system trash when deleting files

(defun onb-trash-on ()
  "Delete files by moving to the system trash."
  (interactive)
  (setq delete-by-moving-to-trash t)
  (setq dired-recursive-deletes 'always) ; don't ask when directory not empty
  (message "Trash on: Deleted files will go to system trash."))

(defun onb-trash-off ()
  "Delete files immediately."
  (interactive)
  (setq delete-by-moving-to-trash nil)
  (setq dired-recursive-deletes 'top) ; ask when directory not empty
  (message "Trash off: Files will be deleted immediately!"))

(onb-trash-on) ; set the default


;; Copying files/directories
(setq dired-recursive-copies 'always)


;; Rename files/directories like normal text via `wdired-mode'
(define-key dired-mode-map (kbd "r") #'wdired-change-to-wdired-mode)


;; Auto refresh dired when contents of a directory change
(require 'autorevert)
(setq auto-revert-verbose nil)
(add-hook 'dired-mode-hook #'auto-revert-mode)


;; Directory listings

(add-hook 'dired-mode-hook
          (lambda ()
            ;; Hide details in file listings? Toggle via "S-("
            (dired-hide-details-mode 1)
            ;; Highlight current line?
            (hl-line-mode 1)))

;; Listing columns; Switch arguments with "C-u s"
;; Show all files: -DlhFA and hide backups with -B
(setq-default dired-listing-switches "-DlhvFA")


;; Mimic dual-pane file managers?
(setq dired-dwim-target t)


;; Images
(require 'image-dired)
(setq image-dired-thumb-margin 0
      image-dired-thumb-relief 0)


;; Linux/Unix only
(defun onb-dired-xdg-open ()
  "Open files and folders with the default desktop app."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(define-key dired-mode-map (kbd "M-RET") #'onb-dired-xdg-open)


;; Go to home directory
(defun onb-home-directory ()
  "Open the home directory in Dired."
  (interactive)
  (dired "~/"))


;; Set new keybinding resembling "C-x C-f" for visiting files
;; Added for convenience; default key binding is "C-x d"
(global-set-key (kbd "C-x C-d") #'dired)


;;; COMINT ____________________________________________________________________


(require 'comint)

(setq comint-input-ignoredups t
      comint-prompt-read-only t)


;;; ESHELL ____________________________________________________________________


;; Eshell is *not* a terminal emulator, but a *shell* equivalent to Bash or Fish
;; that runs within Emacs. It is independent from the OS. Eshell looks like
;; a Posix shell superficially, but is also a REPL for Emacs Lisp expressions.

;; Get rid of the Eshell startup message
(require 'em-banner)
(setq eshell-banner-message "")

;; List directory content after changing into it
(require 'em-dirs)
(setq  eshell-list-files-after-cd t)

;; To open more than one eshell buffer: "C-u C-c x e"
(global-set-key (kbd "C-c x e") #'eshell)


;;; SHELL _____________________________________________________________________


;; Issue shell commands and display their output


;; Set another shell than the default one
;; (setq shell-file-name "/usr/bin/bash")

;; To open more than one shell buffer: "C-u C-c x s"
(global-set-key (kbd "C-c x s") #'shell)


;;; PROCED ____________________________________________________________________


;; Show and manage OS processes


(require 'proced)

(setq proced-auto-update-interval 1)

(setq-default proced-auto-update-flag t
              proced-descend t)


;;; NET-UTILS _________________________________________________________________


(require 'net-utils)

(setq netstat-program "netstat"
      netstat-program-options '("-atupe"))


;; BUILT-IN WEB BROWSER "EWW" _________________________________________________


;; Pretend to be an iPhone
;; (setq url-user-agent
;;       "Mozilla/5.0 (iPhone; CPU iPhone OS 13_6_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/13.1.2 Mobile/15E148 Safari/604.1")

;; Or pretend to be the W3m text-mode browser
(setq url-user-agent "w3m/0.5.3+git20190105")

(setq url-privacy-level '(email lastloc os emacs))
(url-setup-privacy-info)


;;; STANDARD WEB BROWSER ______________________________________________________


;; This can be any graphical web browser,
;; but also a text-mode web browser within Emacs.


;; Set Emacs' `browse-url' function to use:

;; … the system-wide default browser
(setq browse-url-browser-function #'browse-url-default-browser)

;; … or set Firefox explicitly
;; (setq browse-url-browser-function #'browse-url-firefox)


;; Keybinding to browse an URL
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


;;; CALENDAR __________________________________________________________________


(require 'calendar)

(setq calendar-date-style 'iso
      calendar-week-start-day 1
      calendar-weekend-days '(6 0))


;;; ORG-MODE __________________________________________________________________


(require 'org)

;; Set the Org paths
(setq org-directory (expand-file-name "~/Org/"))

(defun onb-org-directory ()
  "Show the Org directory in Dired."
  (interactive)
  (dired org-directory))

(global-set-key (kbd "C-c o d") #'onb-org-directory)


(setq org-default-notes-file (concat org-directory "notes.org"))

(defun onb-org-notes ()
  "Visit the Org main file."
  (interactive)
  (find-file (concat org-directory "notes.org")))

(global-set-key (kbd "C-c o o") #'onb-org-notes)


;; Global todo states
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

;; Capture: put newer entries on top
(setq org-reverse-note-order t)
(global-set-key (kbd "C-c o c") #'org-capture)

;; Agenda
(setq org-agenda-files (list org-directory))
(global-set-key (kbd "C-c o a") #'org-agenda)

;; Links
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c C-l") #'org-insert-link)
(define-key org-mode-map (kbd "C-c o l") #'org-toggle-link-display)


;; Literate programming – activate code blocks via Babel languages
(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)))

;; Insert snippets

(defun onb-org-insert-caption ()
  "Insert caption snippet."
  (interactive)
  (insert "#+caption: "))

(define-key org-mode-map (kbd "C-c C-:") #'onb-org-insert-caption)


;;; TEXT MODES / WRITING ______________________________________________________


;; Visual word wrapping
(add-hook 'text-mode-hook #'visual-line-mode)

;; Sentences end with a single space
(setq sentence-end-double-space nil)


;; Indicate trailing whitespace in "text" modes?
(add-hook 'text-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

;; Cleanup trailing whitespace in "text" modes
(define-key text-mode-map (kbd "C-c w c") #'whitespace-cleanup)


;;; GENERAL EDITING ___________________________________________________________


;; UTF-8
(prefer-coding-system 'utf-8)

;; In a file buffer, remember the place where the cursor was before
(save-place-mode 1)

;; Set desired line length in characters
(setq-default fill-column 80)

;; While a text selection is active, typing characters replaces
;; the selection with the typed characters (default: -1 = off)
(delete-selection-mode -1)

;; Save always with a final new line
(setq require-final-newline t)

;; Better than the default 'just-one-space' (was M-SPC before)
(global-set-key (kbd "S-SPC") #'cycle-spacing)

;; Count lines, words and chars (buffer or region)
(global-set-key (kbd "C-x l") #'count-words)

;; Kill up to character
(global-set-key (kbd "M-z") #'zap-up-to-char)


;;; LINE NUMBERS ______________________________________________________________


;; Line numbers on or off? Toggle with "M-x display-line-numbers-mode"
;; Goto line: "M-g M-g"
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)))


;;; INDENTATION _______________________________________________________________


(setq-default indent-tabs-mode nil ; don't use tabs but spaces
              tab-width 2)         ; set display width for tab characters

;; Indent automatically?
(add-hook 'prog-mode-hook
          (lambda ()
            (electric-indent-mode 1)))

;; Delete the whole indentation instead of one-by-one with <backspace>
;; (Possibly shadowed by 3rd-party packages like 'smartparens-mode')
(setq backward-delete-char-untabify-method 'hungry)


;;; BRACKETS / PARENTHESIS ____________________________________________________


;; How to display matching parens?
(setq show-paren-style 'parenthesis
      show-paren-delay 0.00)


;; Auto-close parens, brackets and quotes?
(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 1)))


;;; WHITESPACE ________________________________________________________________


;; Indicate trailing whitespace in programming modes?
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

;; Cleanup trailing whitespace in programming modes
(define-key prog-mode-map (kbd "C-c w c") #'whitespace-cleanup)


;;; SYNTAX CHECK ______________________________________________________________


;; There are various syntax-checkers coming with the built-in Flymake mode,
;; and additional checkers can be installed as 3rd-party packages via
;; "M-x package-install <RET> flymake-" or `onb-ensure-packages'

(require 'flymake)

;; Disable the legacy backend
(remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)

(add-hook 'prog-mode-hook
          (lambda ()
            (flymake-mode 1)))

(define-key flymake-mode-map (kbd "M-g E") #'flymake-show-project-diagnostics)
(define-key flymake-mode-map (kbd "M-g e") #'flymake-show-buffer-diagnostics)
(define-key flymake-mode-map (kbd "M-g n") #'flymake-goto-next-error)  ; default
(define-key flymake-mode-map (kbd "M-g p") #'flymake-goto-prev-error)  ; default


;;; LISP LANGUAGES ____________________________________________________________


;; Basic support for lispy languages


(defun onb-setup-lisp-buffer ()
  "Basic buffer setup for lispy languages."
  (flymake-mode 1)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (setq-local show-paren-style 'expression)
  (show-paren-mode 1))

(defun onb-setup-lisp-interaction ()
  "Basic interaction-buffer setup for lispy languages."
  (flymake-mode -1)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (setq-local show-paren-style 'expression)
  (show-paren-mode 1))

(defun onb-setup-lisp-repl ()
  "Basic REPL setup for lispy languages."
  (flymake-mode -1)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (setq-local show-paren-style 'expression)
  (show-paren-mode 1))


;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook #'onb-setup-lisp-buffer)
(add-hook 'lisp-interaction-mode-hook #'onb-setup-lisp-interaction)
(add-hook 'ielm-mode-hook #'onb-setup-lisp-repl)

;; Lisp
(add-hook 'lisp-mode-hook #'onb-setup-lisp-buffer)
(add-hook 'inferior-lisp-mode-hook #'onb-setup-lisp-repl)

;; Scheme
(add-hook 'scheme-mode-hook #'onb-setup-lisp-buffer)
(add-hook 'inferior-scheme-mode-hook #'onb-setup-lisp-repl)


;; Additional keybinding to resemble other S-expression related keybindings
;; who usually begin with "C-M". Also useful editing non-lisp languages
(global-set-key (kbd "<C-M-backspace>") #'backward-kill-sexp)


;;; HTML/CSS __________________________________________________________________


(require 'css-mode)
(setq css-indent-offset 2)


;;; ___________________________________________________________________________
(provide 'init-onboard)
;;; init-onboard.el ends here
