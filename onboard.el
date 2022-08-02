;;; onboard.el --- Emacs ONBOARD Starter Kit  -*- lexical-binding: t; -*-
;; Copyright (C) 2021–2022 Dan Dee
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; URL: https://github.com/monkeyjunglejuice/emacs.onboard
;; Version: 0.4
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience
;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:
;; The goal of Emacs ONBOARD is to offer a clean slate to build your personal
;; Emacs config. It stays as close as possible to vanilla Emacs, but offers some
;; convenience and a better user experience, while only relying on built-in
;; packages.

;;; Tested with:
;;  [X] Emacs 28.1 Guix build
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


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; GARBAGE COLLECTION
;; <https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Garbage-Collection>

;; Temporarily set a high value of 256 MB to trigger less garbage collections
;; during initialization. The Emacs default is a threshold of 800 KB
(setq gc-cons-threshold (* 256 1000000))

;; Then lower the threshold to 16 MB during normal operation to prevent longer
;; GC pauses, but still have it at a higher value than the default to experience
;; less mini-interruptions – eg. while scrolling larger buffers.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1000000))))

;; Show a message when garbage collection happens? Useful while tuning the GC
(setq garbage-collection-messages nil)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; PACKAGE MANAGEMENT
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Packages>
;; ... or do "M-x info-emacs-manual s packages RET" to read it within Emacs

;; Browse, select and install 3rd-party packages with "M-x list-packages RET"

(require 'package)

;; 1st priority
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; 2nd priority
;; Install form melpa-stable' only when the package from 'melpa' is broken
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; 3rd priority
;; There is also Gnu Elpa, which is implicitly active by default

;; Initialize packages
(package-initialize)


;; GNU TLS connection issue workaround for Emacs before version 26.3
(require 'gnutls)
(if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))


;; Highlight current line in the package manager
(add-hook 'package-menu-mode-hook
           (lambda ()
             (hl-line-mode 1)))


;; Install packages declaratively within an Emacs Lisp file
(defun onb-package (action package-list)
  "Helper function to install 3rd-party packages declaratively.
PACKAGE-LIST will be installed if 'install is passed as an argument to ACTION.
When ACTION receives 'ignore, then nothing will happen. Use it if you want
to make sure that certain Emacs Lisp packages will be present on your system."
  (when (eq action 'install)
    (mapc #'(lambda (package)
              (unless (package-installed-p package)
                (package-refresh-contents)
                (package-install package nil)))
            package-list)))

(defalias 'ont-package 'onb-package
  "Alias for the function 'onb-package' from Emacs ONBOARD.")

;; Example: You can install suggested 3rd-party packages from within this file
;; with single function calls like so:
;;
;; (onb-package 'install '(the-matrix-theme))  ; installs the package
;; (onb-package 'ignore '(the-matrix-theme))   ; does nothing (default)
;;
;; The installation will be performed when you restart Emacs or
;; when you evaluate a function manually – eg. via pressing "C-M-x"
;; while the cursor is placed somewhere within a function application form.


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; SYSTEM

;; Free the default "C-z" key binding to make it an additional prefix key
;; in the same way as "C-x" and "C-c"
(global-unset-key (kbd "C-z"))

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


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; HELPERS

;; Simplify writing of operating-system-specific code

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


;; Go to home directory
(defun onb-goto-home-directory ()
  "Open the home directory in Dired."
  (interactive)
  (dired "~/"))

;; Open the '~/.emacs.d' directory in the Dired file manager
(defun onb-goto-user-emacs-directory ()
  "Open the Emacs directory in Dired, which is ~/.emacs.d usually."
  (interactive)
  (dired user-emacs-directory))

;; Emacs knows where your init file is (open and edit '.emacs' or 'init.el')
(defun onb-goto-user-init-file ()
  "Visit the init file."
  (interactive)
  (find-file user-init-file))

;; Full path of this file
(defvar onb-onboard-file (or load-file-name (buffer-file-name))
  "Full path of the onboard.el file.")

(defun onb-goto-onboard-file ()
  "Visit the onboard.el file."
  (interactive)
  (find-file onb-onboard-file))


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; SERVER
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Emacs-Server>
;; ... or do "M-x info-emacs-manual s server RET" to read it within Emacs

(require 'server)

;; Display the name of the Emacs server process in the frame title
;; to see easily to which server process a client is connected to
;; Further information:
;; <https://monkeyjunglejuice.github.io/blog/emacs-server-name-frame-title.howto.html>

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


;; Shutdown the Emacs server process
(defun onb-server-stop ()
  "Save buffers, quit and shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; FONTS
;;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Fonts>

;;  This function will be called later under 'THEME CONFIG'

(defun onb-fonts ()
  "The height value is in 1/10 pt, so 130 will give 13 pt."
  ;; Set the default monospaced font
  (set-face-attribute 'default nil
                      ;; :family "Iosevka Curly"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 130)
  ;; Set an alternative monospaced font. Can be the same as above.
  ;; It should have the same character width as the default font
  (set-face-attribute 'fixed-pitch nil
                      ;; :family "Iosevka Curly"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 130)
  ;; Set an alternative monospaced font, preferably with serifs (optional)
  ;; It should have the same character width as the other two fonts above
  (set-face-attribute 'fixed-pitch-serif nil
                      ;; :family "Iosevka Slab"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 130)
  ;; Set the proportional font (toggle by "M-x variable-pitch-mode")
  (set-face-attribute 'variable-pitch nil
                      ;; :family "Iosevka Etoile"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 130)
  ;; Set the modeline fonts
  (set-face-attribute 'mode-line nil
                      ;; :family "Iosevka Curly"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 100)
  (set-face-attribute 'mode-line-inactive nil
                      ;; :family "Iosevka Curly"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 100))


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; TOGGLE THEME

;; Default/fallback definitions – don't change them here,
;; but scroll further down to 'THEME CONFIG'

(defgroup onb-toggle-theme nil
  "Toggle between light and dark theme with a single key press."
  :group 'convenience)

(defcustom onb-light-theme-name
  (cond
   ((>= emacs-major-version 28) (setq onb-light-theme-name 'modus-operandi))
   ((<= emacs-major-version 27) (setq onb-light-theme-name 'leuven)))
  "Name of the light theme."
  :group 'toggle-theme
  :type 'symbol)

(defcustom onb-dark-theme-name
  (cond
   ((>= emacs-major-version 28) (setq onb-dark-theme-name 'modus-vivendi))
   ((<= emacs-major-version 27) (setq onb-dark-theme-name 'wombat)))
  "Name of the dark theme."
  :group 'toggle-theme
  :type 'symbol)

(defcustom onb-default-theme-variant 'dark
  "Load either the 'light or the 'dark theme at startup?"
  :group 'toggle-theme
  :type 'symbol)

(defvar onb-active-theme-variant nil
  "Holds the information about the currently active theme variant.")

(defcustom onb-load-before-light-theme-hook nil
  "Run before loading the light theme."
  :group 'toggle-theme
  :type 'hook)

(defcustom onb-load-after-light-theme-hook nil
  "Run after loading the light theme."
  :group 'toggle-theme
  :type 'hook)

(defcustom onb-load-before-dark-theme-hook nil
  "Run before loading the dark theme."
  :group 'toggle-theme
  :type 'hook)

(defcustom onb-load-after-dark-theme-hook nil
  "Run after loading the dark theme."
  :group 'toggle-theme
  :type 'hook)

(defun onb-load-theme-light ()
  "Load the light theme and apply some modifications."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (run-hooks 'load-before-light-theme-hook)
  (load-theme onb-light-theme-name t)
  (setq onb-active-theme-variant 'light)
  (run-hooks 'onb-load-after-light-theme-hook))

(defun onb-load-theme-dark ()
  "Load the dark theme and apply some modifications."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (run-hooks 'load-before-dark-theme-hook)
  (load-theme onb-dark-theme-name t)
  (setq onb-active-theme-variant 'dark)
  (run-hooks 'onb-load-after-dark-theme-hook))

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


;; ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓

;;; THEME CONFIG

;; Either configure the themes here, or "M-x customize-group RET toggle-theme"


;; Set the light theme:
;; (setq onb-light-theme-name 'modus-operandi)

;; Set the dark theme:
;; (setq onb-dark-theme-name 'modus-vivendi)

;; Set the default variant here, either 'light or 'dark:
;; (setq onb-default-theme-variant 'dark)

;; Set the keybinding to toggle between light and dark:
(global-set-key (kbd "<f12>") #'onb-toggle-theme)

;; The hooks below can be used to run additional functions before or after
;; loading the selected light or dark theme. Useful to set variables that
;; otherwise get overwritten by the themes; for instance the font size of
;; the modeline, which is often explicitly set by the themes themselves.

(add-hook 'onb-load-after-light-theme-hook
          (lambda ()
            (onb-fonts)))

(add-hook 'onb-load-after-dark-theme-hook
          (lambda ()
            (onb-fonts)))

;; ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑


;; Load the theme eventually
(onb-load-theme-default)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; USER INTERFACE


;; Default frame settings – start with an empty alist
(setq default-frame-alist '())

;; Either start Emacs maximized …
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; … or set the default width of the Emacs frame in characters
(add-to-list 'default-frame-alist '(width . 80))

;; … and set the default height of the Emacs frame in lines
(add-to-list 'default-frame-alist '(height . 20))

;; Horizontal position: set the distance from the left screen edge in pixels
;; (add-to-list 'default-frame-alist '(left . 0))

;; Vertical position: set the distance from the top screen edge in pixels
;; (add-to-list 'default-frame-alist '(top . 0))


;; Fringe: choose on which sides (not) to show it
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Fringes>
;; (add-to-list 'default-frame-alist '(right-fringe . 0))

;; Menu bar: on/off by default?
(menu-bar-mode 1)

;; Scroll bar: on/off by default?
;; (if (fboundp 'scroll-bar-mode) ; Emacs 26.1 compatibility
;;     (scroll-bar-mode -1))

;; Tool bar: on/off by default?
(if (fboundp 'tool-bar-mode) ; Emacs 26.1 compatibility
    (tool-bar-mode -1))

;; Tooltips: enable/disable?
(tooltip-mode -1)

;; Startup screen: on/off by default?
(setq inhibit-startup-screen t)

;; Alarms: turn off?
(setq ring-bell-function 'ignore)

;; Redraw the display – useful when running Emacs in a Windows terminal emulator
(global-set-key (kbd "C-c r d") #'redraw-display)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; CURSOR
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Cursor-Display>


;; To learn about available cursors, place your cursor behind 'cursor-type'
;; in the code below or do "M-x describe-symbol RET cursor-type RET"

;; Set the cursor type
;; Uncomment the following expression to change the curser to a vertical bar
;; (add-to-list 'default-frame-alist '(cursor-type . (bar . 2)))

;; Turn on/off cursor blinking by default?
(blink-cursor-mode -1) ; 1 means 'on' / -1 means 'off'

;; Cursor blinking interval in seconds
(setq blink-cursor-interval 0.4)

;; Emphasize the cursor when running Emacs in a text terminal?
(setq visible-cursor nil)

;; Make sure to highlight the current line only in the active window.
(setq hl-line-sticky-flag nil)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; SMOOTH SCROLLING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Scrolling>


(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))
              mouse-wheel-progressive-speed t
              mouse-wheel-follow-mouse t
              scroll-conservatively 10000
              scroll-step 1
              scroll-margin 0  ; leave n lines on both screen ends
              scroll-preserve-screen-position 1)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; PINENTRY


(require 'epg-config)
(setq epg-pinentry-mode 'loopback)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; MODELINE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Mode-Line>

;; Show the buffer size in the modeline
(size-indication-mode 1)

;; Show column number along with line number in modeline
(column-number-mode 1)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; MINIBUFFER
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Minibuffer>

;; Allow minibuffer commands while in the minibuffer
;; There are two commands to get out of recursive minibuffers:
;; "C-M-c" `exit-recursive-edit' and "C-]" `abort-recursive-edit'
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; Use the minibuffer instead of dialog boxes
(setq use-dialog-box nil)

;; Grow and shrink the minibuffer according to its content
(setq resize-mini-windows t)

;; Delete duplicates from the command history
(setq history-delete-duplicates t)

;; Change all yes/no style questions to y/n style
(fset 'yes-or-no-p 'y-or-n-p)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; ELDOC
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Lisp-Doc>

(setq eldoc-echo-area-use-multiline-p t
      eldoc-minor-mode-string "")


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; COMPLETION
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Icomplete>


(require 'icomplete)

(setq icomplete-in-buffer t
      icomplete-compute-delay 0.05
      icomplete-delay-completions-threshold 1000
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil)

;; Emacs version 28 and later: vertical completion with fido-vertical
(when (>= emacs-major-version 28)
  (fido-vertical-mode 1))

;; Emacs version 27 and below: vertical completion with fido or icomplete
(when (<= emacs-major-version 27)
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
    (icomplete-mode 1)))


;; Improve completion by remembering frequently used commands
;; --> recommended 3rd-party package 'amx'
;; If you would like to install the 3rd-party package(s), change 'no to 'yes
;; and evaluate the expression – either via "C-M-x", or simply restart Emacs:
(onb-package 'ignore '(amx))
(when (fboundp #'amx)
  (global-set-key (kbd "M-x") #'amx)
  (global-set-key (kbd "M-X") #'amx-major-mode-commands))


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; WINDOW MANAGEMENT
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Windows>
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Window-Convenience>


;; Emacs often opens buffers in new windows. Let's make window splitting and
;; placement more predictable. For the default window behavior,
;; comment the following expression and restart Emacs
(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-reuse-mode-window
         display-buffer-same-window
         display-buffer-in-previous-window)))

;; Display-buffer: avoid resizing
(setq even-window-sizes nil)

;; Focus follows mouse?
(setq mouse-autoselect-window nil
      focus-follows-mouse nil)


;; Default window navigation – simply switch to the next window in order
;; Added for convenience; the default keybinding is "C-x o"
(global-set-key (kbd "M-SPC") #'other-window)

;; Navigate windows by direction instead
;; (require 'windmove)
;; (setq windmove-wrap-around nil)
;; (global-set-key (kbd "s-j") #'windmove-down)
;; (global-set-key (kbd "s-k") #'windmove-up)
;; (global-set-key (kbd "s-h") #'windmove-left)
;; (global-set-key (kbd "s-l") #'windmove-right)


;; Undo/redo window layouts
(require 'winner)
(winner-mode 1)
(define-key winner-mode-map (kbd "C-x 4 u") #'winner-undo)
(define-key winner-mode-map (kbd "C-x 4 r") #'winner-redo)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; BUFFERS
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Buffers>


;; Ibuffer – the buffer manager
(require 'ibuf-ext)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)))

(setq ibuffer-marked-face 'dired-marked)

(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Uniquify buffer names for buffers that would have identical names
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


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; SCRATCH BUFFER


;; Set an initial major mode for the *scratch* buffer:

;; Lisp interaction mode – that was the default
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Lisp-Interaction>
;; (setq initial-major-mode #'lisp-interaction-mode)

;; Org-mode — for general writing and notes and literate programming
(setq initial-major-mode #'org-mode)

;; Basic text — nothing special
;; (setq initial-major-mode #'fundamental-mode)


;; Should the *scratch* buffer contain some initial content?
(setq initial-scratch-message "")


;; Quickly jump to the *scratch* buffer
(defun onb-scratch ()
  "Jump to the *scratch* buffer. If it does not exist, create it."
  (interactive)
  (switch-to-buffer "*scratch*"))
(global-set-key (kbd "C-c s s") #'onb-scratch)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; VISITING FILES AT POINT

;; "C-x C-v"       – Visit any ressource under the cursor
;; "M-x ffap-menu" – Display a list of all ressources mentioned in this buffer

(ffap-bindings)

(global-set-key (kbd "C-x C-.") #'find-file-at-point)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; CLIPBOARD, COPY & PASTE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Killing>

(require 'select)

(setq
 ;; Use clipboard
 select-enable-clipboard t
 ;; Use primary selection: mark = copy / middle-click = paste
 select-enable-primary t
 ;; When one selects something in another program to pastes it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t
 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)


;; Allow Emacs to copy to and paste from the GUI clipboard
;; when running in a text terminal
;; --> recommended 3rd-party package 'xclip'
;; If you would like to install this 3rd-party package, change 'ignore
;; into 'install and evaluate the expression – either via "C-M-x",
;; or simply restart Emacs:
(onb-package 'ignore '(xclip))
(when (fboundp #'xclip-mode) (xclip-mode 1))


;; Copy the full path of the current file
(defun onb-copy-file-name-to-clipboard ()
  "Copy the full path of the current buffer's file to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard."
               filename))))


;; Simple alternative for 'yank-pop' – present a selection of the kill ring
(defun onb-insert-kill-ring-item ()
  "Select and insert an item from the 'kill-ring'."
  (interactive)
  (insert (completing-read "Yank: " kill-ring nil t)))
(global-set-key (kbd "M-y") #'onb-insert-kill-ring-item)


;; Copy & paste between Windows and Emacs running within WSL
;; (Windows Subsysten for Linux) — which is technically a Linux, not ;Window

;; Copy "kill" text from an Emacs buffer for pasting it into a Windows app
(when (onb-linp)
  (defun onb-wsl-copy (start end)
    "Copy selected text into the Windows clipboard."
    (interactive "r")
    (let ((default-directory "/mnt/c/"))
      (shell-command-on-region start end "clip.exe")))
  (global-set-key (kbd "C-z C-w") 'onb-wsl-copy))

;; Paste "yank" text into Emacs buffer that has been copied from a Windows app
(when (onb-linp)
  (defun onb-wsl-paste ()
    "Paste contents from the Windows clipboard into the Emacs buffer."
    (interactive)
    (let ((coding-system-for-read 'dos)
          (default-directory "/mnt/c/"))
      (insert
       (substring
        (shell-command-to-string "powershell.exe -command 'Get-Clipboard'")
        0  -1))))
  (global-set-key (kbd "C-z C-y") 'onb-wsl-paste))


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; BACKUP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Backup>


;; Make backup before editing
(setq backup-by-copying t
      kept-new-versions 10
      kept-old-versions 3
      delete-old-versions t
      version-control t)

;; Where to save the backups?
;; Specify file name/path patterns and directories ("REGEXP" . "DIRECTORY")
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/"))))


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; LOCKFILES
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Interlocking>


;; Let Emacs keep track of files currently visited?
(setq create-lockfiles nil)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; AUTO-SAVE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Auto-Save>


(setq auto-save-default nil
      auto-save-interval 0)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; HELP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Help>


;; Show all options when running 'apropos' (fulltext search) "C-h a"
(require 'apropos)
(setq apropos-do-all t)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; SEARCH
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Search>


;; Switch search functions to make regex-search the default
(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)
(global-set-key (kbd "C-S-s") #'isearch-forward)
(global-set-key (kbd "C-S-r") #'isearch-backward)

;; Search and replace
;; The 'query-' variant  asks with each string. Confirm with "SPC",
;; or jump to the next via "n"
(global-set-key (kbd "M-%") #'query-replace-regexp)
(global-set-key (kbd "C-M-%") #'replace-regexp)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; RECENT FILES


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


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; DIRED
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Dired>

(require 'dired)


;; The `dired' key binding is "C-x d". This new keybinding is in accordance with
;; "C-x C-f" for visiting files
(global-set-key (kbd "C-x C-d") #'dired)


;; Don't accumulate useless Dired buffers
(defun ont-dired-single-buffer (s)
  "When S is non-nil, prevent superfluous Dired buffers from accumulating.
Kills the current Dired buffer when selecting a new directory"
  (when (not (null s))
    (cond
     ((version< "28.1" emacs-version)
      (put 'dired-find-alternate-file 'disabled nil)
      (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)
      (define-key dired-mode-map (kbd "^") (lambda ()
                                             (interactive)
                                             (find-alternate-file ".."))))
     (t (setq dired-kill-when-opening-new-dired-buffer t)))))

(ont-dired-single-buffer t)  ; set the default


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
(setq-default dired-listing-switches "-lhvFA")


;; Copying files/directories
(setq dired-recursive-copies 'always)

;; Create directories if they don't exist
(setq dired-create-destination-dirs 'ask)

;; Mimic dual-pane file managers?
(setq dired-dwim-target t)


;; Images
(require 'image-dired)
(setq image-dired-thumb-margin 0
      image-dired-thumb-relief 0)


;; Linux/Unix only: hit "M-RET" to open files in desktop app
(when (onb-linp)
  (defun onb-dired-xdg-open ()
    "Open files and folders with the default desktop app."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "xdg-open" nil 0 nil file)
      (message "Opening %s done" file)))
  (define-key dired-mode-map (kbd "M-RET") #'onb-dired-xdg-open))


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; COMINT

(require 'comint)

(setq comint-input-ignoredups t
      comint-prompt-read-only t)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; ESHELL
;; <https://www.gnu.org/software/emacs/manual/html_mono/eshell.html>

;; Eshell is not a terminal emulator, but a shell equivalent to Bash or Fish
;; that runs within Emacs. It is independent from the OS. Eshell looks like
;; a Posix shell superficially, but is also a REPL for Emacs Lisp expressions.

;; Get rid of the Eshell startup message
(require 'em-banner)
(setq eshell-banner-message "")

;; List directory content after changing into it
(require 'em-dirs)
(setq  eshell-list-files-after-cd t)

;; To open more than one eshell buffer: "C-u C-c x e"
(global-set-key (kbd "C-x x e") #'eshell)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; SHELL

;; Issue shell commands and display their output

;; Set another shell than the default one
;; (setq shell-file-name "/usr/bin/bash")

;; To open more than one shell buffer: "C-u C-c x s"
(global-set-key (kbd "C-x x s") #'shell)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; PROCED

;; Show and manage OS processes, like the command line programs top and htop

(require 'proced)

(setq proced-auto-update-interval 1)

(setq-default proced-auto-update-flag t
              proced-descend t)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; NET-UTILS

(require 'net-utils)

(setq netstat-program "netstat"
      netstat-program-options '("-atupe"))


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; BUILT-IN WEB BROWSER "EWW"
;; <https://www.gnu.org/software/emacs/manual/html_mono/eww.html#Top>

;; Pretend to be an iPhone
(setq url-user-agent
      "Mozilla/5.0 (iPhone; CPU iPhone OS 13_6_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/13.1.2 Mobile/15E148 Safari/604.1")

                    ;; Or pretend to be the W3m text-mode browser
                    ;; (setq url-user-agent "w3m/0.5.3+git20190105")

                    (setq url-privacy-level '(email lastloc os emacs))
                    (url-setup-privacy-info)


                    ;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; PRIMARY WEB BROWSER
                    ;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Hyperlinking>

                    ;; This can be any graphical web browser, but also a built-in web browser

                    ;; Set Emacs' `browse-url' function …

                    ;; … to the system-wide default browser
                    (setq browse-url-browser-function #'browse-url-default-browser)

                    ;; … to Firefox explicitly
                    ;; (setq browse-url-browser-function #'browse-url-firefox)

                    ;; … or to the Nyxt browser <https://nyxt.atlas.engineer/>
                    ;; (setq browse-url-generic-program "nyxt")
                    ;; (setq browse-url-browser-function #'browse-url-generic)

                    ;; Keybinding
                    (global-set-key (kbd "C-c w w") #'browse-url)


                    ;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
                    ;; SECONDARY WEB BROWSER

                    ;; Set an alternative browser — currently set to Emacs' built-in EWW
                    (setq browse-url-secondary-browser-function #'browse-web)

                    ;; Keybinding
                    (global-set-key (kbd "C-c w W") #'browse-web)


                    ;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; EMAIL SENDING
                    ;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Sending-Mail>

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


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; CALENDAR
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Calendar_002fDiary>

(require 'calendar)

(setq calendar-date-style 'iso
      calendar-week-start-day 1
      calendar-weekend-days '(6 0))


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; GENERAL EDITING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Basic>

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


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; LINE NUMBERS
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Display-Custom>

;; Line numbers on or off? Toggle with "M-x display-line-numbers-mode" or
;; set it here for all programming modes. Goto line: "M-g M-g"
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)))


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; INDENTATION
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Indentation>

(setq-default indent-tabs-mode nil ; don't use tabs but spaces
              tab-width 2)         ; set display width for tab characters

;; Delete the whole indentation instead spaces one-by-one via <backspace>?
;; (Possibly shadowed by 3rd-party packages like 'smartparens-mode')
(setq backward-delete-char-untabify-method 'hungry)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; BRACKETS / PARENTHESIS
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Parentheses>

;; How to display matching parens? General `show-paren-mode' configuration
(setq show-paren-style 'parenthesis
      show-paren-delay 0.00)

;; Auto-close parens, brackets and quotes?
;; (add-hook 'prog-mode-hook #'electric-pair-mode)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; WHITESPACE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Useless-Whitespace>

;; Indicate trailing whitespace in programming modes?
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))
;; Cleanup trailing whitespace in programming modes
(define-key prog-mode-map (kbd "C-c w c") #'whitespace-cleanup)

;; Indicate trailing whitespace in "text" modes?
(add-hook 'text-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))
;; Cleanup trailing whitespace in "text" modes
(define-key text-mode-map (kbd "C-c w c") #'whitespace-cleanup)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; SYNTAX CHECK / LINTER
;; <https://www.gnu.org/software/emacs/manual/html_mono/flymake.html>

(require 'flymake)

;; There are various syntax-checkers coming with the built-in Flymake mode,
;; and additional checkers can be installed as 3rd-party packages via
;; "M-x package-install <RET> flymake-" or `(onb-package 'install '(NAME))'

;; Disable the legacy backend
(remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)

;; Style the Flymake widget in the modeline
(setq flymake-mode-line-format
      '(" " "FlyM" flymake-mode-line-exception flymake-mode-line-counters))

;; Stop when first/last error is reached
(setq flymake-wrap-around nil)

(define-key flymake-mode-map (kbd "M-g E") #'flymake-show-project-diagnostics)
(define-key flymake-mode-map (kbd "M-g e") #'flymake-show-buffer-diagnostics)
(define-key flymake-mode-map (kbd "M-g n") #'flymake-goto-next-error)  ; default
(define-key flymake-mode-map (kbd "M-g p") #'flymake-goto-prev-error)  ; default


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; TEXT MODES / WRITING

;; Sentences end with a single space
(setq sentence-end-double-space nil)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; ORG-MODE
;; <https://orgmode.org/>
;; <https://www.gnu.org/software/emacs/manual/html_mono/org.html>

(require 'org)

;; Set a default location to look for Org files, but there
;; is no need to put your files into this directory
(setq org-directory (expand-file-name "~/Org/"))

(defun onb-org-directory ()
  "Show the Org directory in Dired."
  (interactive)
  (dired org-directory))
(global-set-key (kbd "C-c o d") #'onb-org-directory)


;; Set a default target for storing notes
(setq org-default-notes-file (concat org-directory "notes.org"))

(defun onb-org-notes ()
  "Visit the Org notes file."
  (interactive)
  (find-file (concat org-directory "notes.org")))
(global-set-key (kbd "C-c o o") #'onb-org-notes)


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


;; Global todo states
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))


;; Visual word wrapping
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Visual-Line-Mode>
(add-hook 'org-mode-hook #'visual-line-mode)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; LITERATE PROGRAMMING
;; <https://orgmode.org/worg/org-contrib/babel/intro.html>

;; Activate code blocks via Babel languages
(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))

;; In case you're using the Emacs ONTOP extensions, further languages
;; should be configured not here, but within their specific ONTOP modules
;; ("onboard-*.el" files)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;; LISP LANGUAGES
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Executing-Lisp>

(defun onb-setup-lisp-languages ()
  "Basic support for languages from the Lisp family."
  (setq-local show-paren-style 'expression)
  (show-paren-local-mode 1)
  (electric-pair-local-mode 1)
  (electric-indent-local-mode 1))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook #'onb-setup-lisp-languages)
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
(add-hook 'lisp-interaction-mode-hook #'onb-setup-lisp-languages)
(add-hook 'ielm-mode-hook #'onb-setup-lisp-languages)
;; Lisp
(add-hook 'lisp-mode-hook #'onb-setup-lisp-languages)
(add-hook 'inferior-lisp-mode-hook #'onb-setup-lisp-languages)
;; Scheme
(add-hook 'scheme-mode-hook #'onb-setup-lisp-languages)
(add-hook 'inferior-scheme-mode-hook #'onb-setup-lisp-languages)

;; Additional keybinding resembling other sexp-related keybindings
;; who usually begin with "C-M". Also useful editing non-lisp languages
(global-set-key (kbd "<C-M-backspace>") #'backward-kill-sexp)


;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
(provide 'onboard)
;;; onboard.el ends here
