;;; eon.el --- Emacs Onboard Starter Kit -*- lexical-binding: t; -*-
;;
;;    ▒░▒░▒░   ▒░     ▒░ ▒░▒░▒░▒░     ▒░▒░▒░      ▒░    ▒░▒░▒░▒░    ▒░▒░▒░▒░
;;   ▒░    ▒░  ▒░▒░   ▒░  ▒░     ▒░  ▒░    ▒░    ▒░▒░    ▒░     ▒░   ▒░    ▒░
;;  ▒░      ▒░ ▒░ ▒░  ▒░  ▒░     ▒░ ▒░      ▒░  ▒░  ▒░   ▒░     ▒░   ▒░     ▒░
;;  ▒░      ▒░ ▒░  ▒░ ▒░  ▒░▒░▒░▒░  ▒░      ▒░ ▒░    ▒░  ▒░▒░▒░▒░    ▒░     ▒░
;;  ▒░      ▒░ ▒░   ▒░▒░  ▒░     ▒░ ▒░      ▒░ ▒░▒░▒░▒░  ▒░   ▒░     ▒░     ▒░
;;   ▒░    ▒░  ▒░     ▒░  ▒░     ▒░  ▒░    ▒░ ▒░      ▒░ ▒░     ▒░   ▒░    ▒░
;;    ▒░▒░▒░  ▒░      ▒░ ▒░▒░▒░▒░     ▒░▒░▒░  ▒░      ▒░ ▒░      ▒░ ▒░▒░▒░▒░
;;
;;; Commentary:
;; Emacs ONBOARD offers a clean slate to build your personal Emacs config.
;; It stays as close as possible to vanilla Emacs, but offers some convenience
;; and a better user experience, while only relying on built-in packages.
;;
;; Copyright (C) 2021–2025 Dan Dee
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; URL: https://github.com/monkeyjunglejuice/emacs.onboard
;; Version: 1.7.0
;; Package-Requires: ((EMACS "30.1"))
;; Keywords: convenience
;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.
;;
;;; Keybindings:
;;
;; "M-x"  Show all commands
;;        – hold down the "Meta key" and press <x>
;;        – the "Meta key" is usually <Alt> on Linux/Windows and <Option> on Mac
;;
;; "C-g"  Get out! Press <Ctrl>+<g> to cancel whatever happens – or hit 3x <ESC>
;;
;; "F12"  Toggle between dark and light theme
;;
;;; Examples:
;;
;; "M-x eon-"                     Show all commands defined by Emacs ONBOARD
;; "M-x eon-visit-user-init-file" Visit main config file: .emacs or init.el
;; "M-x check-parens"             Check if all parens match in Emacs Lisp code
;; "M-x help"                     Reach the ultimate help menu
;;
;; "C-h o" Place the cursor behind a keyword, function, variable or other symbol
;;         to issue the command `describe-symbol' via keybinding
;;         and view the symbol's documentation
;;
;; "M-;"   Comment/uncomment a selected piece of text or code
;;
;;; Code:

;;  ____________________________________________________________________________
;;; GARBAGE COLLECTION
;; <https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Garbage-Collection>

;; Set a high value of 1 GB to prevent frequent garbage collections
;; during initialization
(setq gc-cons-threshold #x40000000)  ; default threshold is 800 KB

;; Prevent longer GC pauses and experience less mini-interruptions.
;; When idle for 15 sec, run the GC no matter what.
;; This hack was stolen from <https://akrl.sdf.org/>
(defmacro eon-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defvar eon-gc-timer nil
  "Timer object for garbage collection monitoring.
The timer can be canceled with `eon-cancel-gc-timer'.")

(defun eon-start-gc-timer ()
  "Start the garbage collection timer."
  (interactive)
  (setq eon-gc-timer
        (run-with-idle-timer 15 t (lambda () (eon-time (garbage-collect))))))

(defun eon-cancel-gc-timer ()
  "Cancel the garbage collection timer."
  (interactive)
  (when (timerp eon-gc-timer)
    (cancel-timer eon-gc-timer)
    (setq eon-gc-timer nil)))

;; Start the GC Timer
(eon-start-gc-timer)

;; Show a message when garbage collection happens? Useful while tuning the GC
(setq garbage-collection-messages nil)

;; Diagnostics
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s with %d garbage collections."
                     (format "%.3f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;  ____________________________________________________________________________
;;; PACKAGE MANAGEMENT
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Packages>
;; ... or do "M-x info-emacs-manual s packages RET" to read it within Emacs

;; Browse, select and install 3rd-party packages with "M-x list-packages RET"

;; The package setup respects when `package-enable-at-startup' is set to nil
;; in `early-init-el', so that you can use other package managers like Straight
;; or Elpaca without issues.

(when package-enable-at-startup
  (require 'package)

  ;; 1st priority
  ;; Gnu Elpa and Non-Gnu Elpa, which are enabled by default

  ;; 2nd priority
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)

  ;; 3rd priority
  ;; Install from melpa-stable' only when the package from 'melpa' is broken
  ;; (add-to-list 'package-archives
  ;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)

  ;; Highlight current line in the package manager
  (add-hook 'package-menu-mode-hook
            (lambda ()
              (hl-line-mode 1))))

;;  ____________________________________________________________________________
;;; NATIVE ELISP COMPILATION

;; Prevent stale elisp bytecode from shadowing more up-to-date source files?
(setq load-prefer-newer t)

;; Natively compile packages at first use or immediately after installation?
(setq package-native-compile t)

;; Ask whether to terminate asynchronous compilations on exit?
(setq native-comp-async-query-on-exit t)

;;  ____________________________________________________________________________
;;; HELPERS

;; Simplify writing of operating-system-specific Elisp code

(defun eon-linp ()
  "True if `system-type' is Linux or something compatible.
For finer granularity, use the variables `system-type'
or `system-configuration' directly."
  (memq system-type '(gnu/linux berkeley-unix gnu gnu/kfreebsd)))

(defun eon-winp ()
  "True if `system-type' is Windows or something compatible.
For finer granularity, use the variables `system-type'
or `system-configuration' directly."
  (memq system-type '(windows-nt cygwin ms-dos)))

(defun eon-macp ()
  "True if `system-type' is MacOS or something compatible.
For finer granularity, use the variables `system-type'
or `system-configuration' directly."
  (eq system-type 'darwin))

(defun eon-androidp ()
  "True if `system-type' is Android or something compatible.
For finer granularity, use the variables `system-type'
or `system-configuration' directly."
  (eq system-type 'android))

;; Open the '~/.emacs.d' directory in the Dired file manager
(defun eon-visit-user-emacs-directory ()
  "Open the Emacs directory in Dired, which is usually '~/.emacs.d'."
  (interactive)
  (dired user-emacs-directory))

;; Emacs knows where your init file is (open and edit '.emacs' or 'init.el')
(defun eon-visit-user-init-file ()
  "Visit the init file."
  (interactive)
  (find-file user-init-file))

;;  ____________________________________________________________________________
;;; KEYMAPS

;; Make "C-z" available as a prefix key in the same manner as "C-x" and "C-c";
;; therefore "C-z" acts like kind of a "leader key".
;; To avoid clashes, new keybindings introduced by Emacs ONboard will usually
;; begin with the prefix "C-z" (with only a few exceptions).
;; This is unlikely to work out of the box if you are using Emacs within a
;; terminal emulator, as terminal emulators are usually configured to suspend
;; a running command via the C-z keybinding.

(global-unset-key (kbd "C-z"))

(define-prefix-command 'ctl-z-map nil "Leader")
(global-set-key (kbd "C-z") 'ctl-z-map)

(define-prefix-command 'ctl-z-c-map nil "Code")
(define-key ctl-z-map (kbd "c") 'ctl-z-c-map)

(define-prefix-command 'ctl-z-e-map nil "Emacs built-ins")
(define-key ctl-z-map (kbd "e") 'ctl-z-e-map)

(define-prefix-command 'ctl-z-g-map nil "Version control")
(define-key ctl-z-map (kbd "g") 'ctl-z-g-map)

(define-prefix-command 'ctl-z-o-map nil "Org")
(define-key ctl-z-map (kbd "o") 'ctl-z-o-map)

(define-prefix-command 'ctl-z-p-map nil "Project")
(define-key ctl-z-map (kbd "p") 'ctl-z-p-map)

(define-prefix-command 'ctl-z-s-map nil "Scratch buffers")
(define-key ctl-z-map (kbd "s") 'ctl-z-s-map)

(define-prefix-command 'ctl-z-w-map nil "Web")
(define-key ctl-z-map (kbd "w") 'ctl-z-w-map)

(define-prefix-command 'ctl-z-x-map nil "Execute")
(define-key ctl-z-map (kbd "x") 'ctl-z-x-map)

;;  ____________________________________________________________________________
;;; KEYBINDINGS

(when (eq system-type 'darwin)
  (setq
   ;; Make the <Command> key on MacOS act as <Ctrl> key: "C- ..."
   mac-command-modifier 'control
   ;; Make the <Option> key on MacOS act as <Meta> key for "M- ..."
   mac-option-modifier 'meta
   ;; Don't bypass "C-h ..." keybindings
   mac-pass-command-to-system nil))

;; Show a help window with possible key bindings?
(when (fboundp #'which-key-mode)
  (setq which-key-lighter ""
        which-key-idle-delay 0.3
        which-key-idle-secondary-delay 0.0
        which-key-sort-uppercase-first nil)
  (which-key-mode 1))

;;  ____________________________________________________________________________
;;; SYSTEM

;; Increase warning threshold
(setq large-file-warning-threshold (* 64 1000000))

;; Increase undo limit
(setq undo-limit 67108864          ; 64mb
      undo-strong-limit 100663296  ; 96mb
      undo-outer-limit 1006632960) ; 960mb

;; Increase the amount of data which Emacs reads from subprocesses
(setq read-process-output-max (* 1024 1024))  ; 1 MB

;;  ____________________________________________________________________________
;;; SERVER
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Emacs-Server>
;; ... or do "M-x info-emacs-manual s server RET" to read it within Emacs

;; Display the name of the Emacs server process in the frame title
;; to see easily to which server process a client is connected to
;; Further information:
;; <https://monkeyjunglejuice.github.io/blog/emacs-server-name-frame-title.howto.html>
(with-eval-after-load 'server

  (defun eon-frame-title ()
    "Set a custom frame title."
    (setq frame-title-format
          (concat "%b (%f)"
                  (when (server-running-p)
                    (concat " " server-name)))))

  (add-hook 'emacs-startup-hook
            (lambda ()
              "Run functions after loading init files"
              (eon-frame-title)))

  (add-hook 'server-mode-hook
            (lambda ()
              "Run functions after entering or leaving server-mode."
              (eon-frame-title)))

  ;; Shutdown the Emacs server process
  (defun eon-server-stop ()
    "Save buffers, quit and shutdown (kill) server."
    (interactive)
    (save-some-buffers)
    (kill-emacs)))

;; Start the server?
;; (server-start)

;;  ____________________________________________________________________________
;;; FONTS
;;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Fonts>

;; You can use this function definition as a template to define your own font
;; set, then call your personal function via `eon-load-after-light-theme-hook'
;; and `eon-load-after-light-theme-hook' (under section 'THEME CONFIG').

(defun eon-fonts-default ()
  "The height value is in 1/10 pt, so 140 will give 14 pt."
  (interactive)
  ;; Set the default monospaced font
  (set-face-attribute 'default nil
                      ;; :family "Iosevka Curly"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 150)
  ;; Set an alternative monospaced font. Can be the same as above.
  ;; It should have the same character width as the default font
  (set-face-attribute 'fixed-pitch nil
                      ;; :family "Iosevka Curly"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  ;; Set an alternative monospaced font, preferably with serifs (optional)
  ;; It should have the same character width as the other two fonts above
  (set-face-attribute 'fixed-pitch-serif nil
                      ;; :family "Iosevka Slab"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  ;; Set the proportional font (toggle by "M-x variable-pitch-mode")
  (set-face-attribute 'variable-pitch nil
                      ;; :family "Iosevka Etoile"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  ;; Set the fonts for the active mode line
  (set-face-attribute 'mode-line nil
                      ;; :family "Iosevka Curly"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.9)
  ;; Set the fonts for the inactive mode line
  (set-face-attribute 'mode-line-inactive nil
                      ;; :family "Iosevka Curly"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.9))

;;  ____________________________________________________________________________
;;; TOGGLE THEME

;; Default/fallback definitions – don't change them here,
;; but scroll further down to 'THEME CONFIG'

(defgroup eon-toggle-theme nil
  "Toggle between light and dark theme with a single key press."
  :group 'convenience)

(defcustom eon-light-theme-name 'modus-operandi
  "Name of the light theme."
  :group 'eon-toggle-theme
  :type 'symbol)

(defcustom eon-dark-theme-name 'modus-vivendi
  "Name of the dark theme."
  :group 'eon-toggle-theme
  :type 'symbol)

(defcustom eon-default-theme-variant 'light
  "Load either the light or the dark theme at startup?"
  :group 'eon-toggle-theme
  :type 'symbol)

(defvar eon-active-theme-variant nil
  "Holds the information about the currently active theme variant.")

(defcustom eon-load-before-light-theme-hook nil
  "Run before loading the light theme."
  :group 'eon-toggle-theme
  :type 'hook)

(defcustom eon-load-after-light-theme-hook nil
  "Run after loading the light theme."
  :group 'eon-toggle-theme
  :type 'hook)

(defcustom eon-load-before-dark-theme-hook nil
  "Run before loading the dark theme."
  :group 'eon-toggle-theme
  :type 'hook)

(defcustom eon-load-after-dark-theme-hook nil
  "Run after loading the dark theme."
  :group 'eon-toggle-theme
  :type 'hook)

(defun eon-load-theme-light ()
  "Load the light theme and apply some modifications.
Some themes may come as functions -- wrap these ones in lambdas."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (run-hooks 'eon-load-before-light-theme-hook)
  (cond ((symbolp eon-light-theme-name)
         (load-theme eon-light-theme-name t))
        ((functionp eon-light-theme-name)
         (funcall eon-light-theme-name)))
  (setq eon-active-theme-variant 'light)
  (run-hooks 'eon-load-after-light-theme-hook))

(defun eon-load-theme-dark ()
  "Load the dark theme and apply some modifications.
Some themes may come as functions -- wrap these ones in lambdas."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (run-hooks 'eon-load-before-dark-theme-hook)
  (cond ((symbolp eon-dark-theme-name)
         (load-theme eon-dark-theme-name t))
        ((functionp eon-dark-theme-name)
         (funcall eon-dark-theme-name)))
  (setq eon-active-theme-variant 'dark)
  (run-hooks 'eon-load-after-dark-theme-hook))

(defun eon-toggle-theme ()
  "Toggle between light and dark theme."
  (interactive)
  (cond
   ((equal eon-active-theme-variant 'light) (eon-load-theme-dark))
   ((equal eon-active-theme-variant 'dark) (eon-load-theme-light))
   (t (mapc #'disable-theme custom-enabled-themes))))

(defun eon-load-theme-default ()
  "Load the default theme."
  (cond
   ((equal eon-default-theme-variant 'light) (eon-load-theme-light))
   ((equal eon-default-theme-variant 'dark) (eon-load-theme-dark))
   (t (error
       "Toggle theme: DEFAULT-THEME-VARIANT must be either 'light or 'dark"))))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

;;; THEME CONFIG
;; Either configure the themes here, or "M-x customize-group RET toggle-theme"

;; Set some defaults for the Modus themes; doesn't affect other themes.
;; These variables must be set before loading the Modus themes.
(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs nil
      modus-themes-mixed-fonts t)
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active bg-mode-line-active)
        (border-mode-line-inactive bg-mode-line-inactive)))

;; --> Set your light theme:
(setq eon-light-theme-name 'modus-operandi-tinted)

;; --> Set your dark theme:
(setq eon-dark-theme-name 'modus-vivendi-tinted)

;; --> Set your default variant here - 'light or 'dark
(setq eon-default-theme-variant 'light)

;; Set the keybinding to toggle between light and dark:
(global-set-key (kbd "<f12>") #'eon-toggle-theme)

;; The hooks below can be used to run additional functions before or after
;; loading the selected light and dark theme. That's useful for setting
;; variables that otherwise would get overwritten by the themes.
;; Restart Emacs to take effect after changing the hooks.

(add-hook 'eon-load-after-light-theme-hook
          (lambda ()
            (eon-fonts-default)
            ))

(add-hook 'eon-load-after-dark-theme-hook
          (lambda ()
            (eon-fonts-default)
            ))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;; Load the theme eventually
(eon-load-theme-default)

;;  ____________________________________________________________________________
;;; DEFAULT AND INITIAL FRAME
;; In Emacs terminology, a "frame" means the ordinary "desktop window";
;; while "window" refers to tiled panels within an Emacs frame. Why?
;; Because Emacs had it first, and today's convention what "window" means
;; appeared later.
;; In order to define properties generally, add them to `default-frame-alist';
;; to affect only the first frame created, add them to `initial-frame-alist'.

;; Either start Emacs maximized …
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; … or set the default width of the Emacs frame in characters
(add-to-list 'default-frame-alist '(width . 80))

;; … and set the default height of the Emacs frame in lines
(add-to-list 'default-frame-alist '(height . 32))

;; Horizontal position: set the distance from the left screen edge in pixels
;; That way, only the first frame created will get a fixed position:
;; (add-to-list 'initial-frame-alist '(left . 0))

;; Vertical position: set the distance from the top screen edge in pixels
;; That way, only the first frame created will get a fixed position:
;; (add-to-list 'initial-frame-alist '(top . 0))

;; Fringe: choose on which sides (not) to show it
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Fringes>
;; (add-to-list 'default-frame-alist '(right-fringe . 0))

;; Bring frame to the front
(when (display-graphic-p)
  (select-frame-set-input-focus (selected-frame)))

;;  ____________________________________________________________________________
;;; CURSOR
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Cursor-Display>

;; To learn about available cursors, place your cursor behind 'cursor-type'
;; in the code below or do "M-x describe-symbol RET cursor-type RET"

;; Set the cursor type
;; Comment out the following expression to change the cursor into a bar
;; (add-to-list 'default-frame-alist '(cursor-type . bar))

;; Turn on/off cursor blinking by default? 1 means 'on' and -1 means 'off'
(blink-cursor-mode -1)

;; Cursor blinking interval in seconds
(setq blink-cursor-interval 0.3)

;; Blink cursor that often before going into solid state
(setq blink-cursor-blinks 3)

;; Emphasize the cursor when running Emacs in a text terminal?
(setq visible-cursor nil)

;; Make sure to highlight the current line only in the active window.
(setq hl-line-sticky-flag nil)
(add-hook 'special-mode-hook
          (lambda ()
            ;; Highlight current line in special modes?
            (hl-line-mode 1)))

;; Render cursors or regions in non-focused windows?
(setq-default cursor-in-non-selected-windows nil)

;;  ____________________________________________________________________________
;;; USER INTERFACE

;; Menu bar: on/off by default?
(menu-bar-mode 1)

;; Scroll bar: on/off by default?
(when (display-graphic-p)
  (scroll-bar-mode -1))

;; Tool bar: on/off by default?
(when (display-graphic-p)
  (tool-bar-mode -1))

;; Tooltips: enable/disable?
(tooltip-mode -1)

;; Startup screen: on/off by default?
(setq inhibit-startup-screen t)

;; Alarms: turn off?
(setq ring-bell-function 'ignore)

;; Redraw the display – useful when running Emacs in a Windows terminal emulator
(define-key ctl-z-map (kbd "C-r") #'redraw-display)

;;  ____________________________________________________________________________
;;; SCROLLING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Scrolling>

(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))
              mouse-wheel-progressive-speed t
              mouse-wheel-follow-mouse t)

(setq-default scroll-preserve-screen-position t
              scroll-margin 1
              scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; Horizontal scrolling
(setq auto-window-vscroll nil
      hscroll-margin 1
      hscroll-step 1)

;; Enable pixel-based scrolling
(when (fboundp #'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;;  ____________________________________________________________________________
;;; MODELINE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Mode-Line>

;; Compress the mode line? If non-nil, repeating spaces are compressed into
;; a single space. If 'long', this is only done when the mode line is longer
;; than the current window width (in columns).
(setq mode-line-compact nil)

;; Show the buffer size in the modeline?
(size-indication-mode 1)

;; Show column number along with line number in modeline?
(column-number-mode 1)

;;  ____________________________________________________________________________
;;; MINIBUFFER
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Minibuffer>

;; Keep cursor outside of any ‘cursor-intangible’ text property
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Recursive minibuffers
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Recursive-Edit>
;; Allow minibuffer commands while in the minibuffer
;; There are two commands to get out of recursive minibuffers:
;; "C-z-c" exit-recursive-edit and "C-]" abort-recursive-edit
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; Use the minibuffer instead of dialog boxes?
(setq use-dialog-box nil)

;; Grow and shrink the minibuffer according to its content?
(setq resize-mini-windows 'grow-only)

;; Save minibuffer history between Emacs sessions?
(setq history-length 300)
(savehist-mode 1)

;; Delete duplicates from the command history?
(setq history-delete-duplicates t)

;; Allow for shorter responses? "y" for "yes" and "n" for "no"
(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;;  ____________________________________________________________________________
;;; COMPLETION
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Icomplete>

;; There are many matching styles available, see `completion-styles-alist'
;; <https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html>
;; The order within the list determines their priority.
(setq completion-styles '(basic substring initials flex partial-completion))
(setq completion-category-defaults nil)
(setq completion-category-overrides
      '((file (styles . (basic partial-completion initials substring)))))

;; Prevent *Completions* buffer from popping up?
(setq completion-auto-help nil)
;; Cycle completion candidates instead
(setq completion-cycle-threshold t)
;; Show docstrings for completion candidates?
(setq completions-detailed nil)

;; Preview current in-buffer completion candidate?
(when (fboundp #'completion-preview-mode)
  (global-completion-preview-mode 1))
(define-key completion-preview-active-mode-map (kbd "M-n")
            #'completion-preview-next-candidate)
(define-key completion-preview-active-mode-map (kbd "M-p")
            #'completion-preview-prev-candidate)

;; Tweaking Icomplete
(with-eval-after-load 'icomplete
  (setq icomplete-in-buffer t
        icomplete-compute-delay 0
        icomplete-delay-completions-threshold 100
        icomplete-show-matches-on-no-input t
        icomplete-hide-common-prefix nil)
  ;; TAB accepts the current candidate in Fido minibuffers
  (define-key icomplete-minibuffer-map (kbd "TAB") #'icomplete-force-complete)
  (define-key icomplete-minibuffer-map (kbd "<tab>") #'icomplete-force-complete))

;; Vertical minibuffer completion with `fido-vertical'
(fido-vertical-mode 1)

;; Dabbrev
(with-eval-after-load 'dabbrev
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

;;  ____________________________________________________________________________
;;; ELDOC
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Lisp-Doc>
;; <https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc>

(setq eldoc-minor-mode-string nil
      eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
      eldoc-echo-area-display-truncation-message nil
      eldoc-echo-area-prefer-doc-buffer nil
      eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)

(define-key ctl-z-c-map (kbd "d") #'eldoc)

;;  ____________________________________________________________________________
;;; PINENTRY

(setq epg-pinentry-mode 'loopback)

;;  ____________________________________________________________________________
;;; WINDOW MANAGEMENT
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Windows>
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Window-Convenience>

;; Display-buffer: avoid resizing
(setq even-window-sizes nil)

;; Focus follows mouse?
(setq mouse-autoselect-window nil
      focus-follows-mouse nil)

;; Default window navigation – simply switch to the next window in order.
;; Added for convenience; the default keybinding is "C-x o"
(global-set-key (kbd "M-o") #'other-window)

;; Navigate windows by direction
;; (setq windmove-wrap-around nil)
;; (global-set-key (kbd "s-j") #'windmove-down)
;; (global-set-key (kbd "s-k") #'windmove-up)
;; (global-set-key (kbd "s-h") #'windmove-left)
;; (global-set-key (kbd "s-l") #'windmove-right)

;; Undo/redo window layouts
(winner-mode 1)
(define-key winner-mode-map (kbd "C-x 4 u") #'winner-undo)
(define-key winner-mode-map (kbd "C-x 4 r") #'winner-redo)

;;  ____________________________________________________________________________
;;; BUFFERS
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Buffers>

;; Uniquify buffer names for buffers that would have identical names
(setq uniquify-buffer-name-style 'forward)

;; Fast buffer switching
(global-set-key (kbd "M-[") #'previous-buffer)
(global-set-key (kbd "M-]") #'next-buffer)

;; Kill the current buffer immediately instead of presenting a selection
;; It's the equivalent to "close tab" in a web browser or other editors
(global-set-key (kbd "C-x k") #'kill-current-buffer)

;; Get the buffer out of the way, but let it alive
(global-set-key (kbd "C-x K") #'bury-buffer)
(global-set-key (kbd "C-x M-k") #'unbury-buffer)

;; Kill all buffers at once – equivalent to "close all tabs"
(defun eon-kill-all-buffers ()
  "Close all buffers at once."
  (interactive)
  (save-some-buffers)
  (let ((kill-buffer-query-functions '()))
    (mapc #'kill-buffer (buffer-list))))

;; Define boring buffers globally, so they can be hidden.
;; These buffers remain accessible via Ibuffer "C-x C-b".
(defvar eon-boring-buffers '("\\` "
                             "\\`\\*Echo Area"
                             "\\`\\*Minibuf"
                             "\\`\\*Completions"
                             "\\`\\*Flymake log"
                             "\\`\\*Semantic SymRef"
                             "\\`\\*Backtrace"
                             "\\`\\*tramp"
                             "\\`\\*EGLOT"
                             ;; And some hidden buffers can be visited by ...
                             "\\`\\*scratch"        ; "C-z s s"
                             "\\`\\*Messages"       ; "C-h e"
                             "\\`\\*Bookmark List"  ; "C-x r l"
                             "\\`\\*Ibuffer"        ; "C-x C-b"
                             )
  "List of buffer names of buffers to hide on several occasions.
The elements of the list are regular expressions.")

;;  ____________________________________________________________________________
;;; IBUFFER – the buffer manager
;; <https://protesilaos.com/codelog/2020-04-02-emacs-intro-ibuffer/>

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)))

(setq ibuffer-marked-face 'dired-marked)

;; Hide the boring buffers from Ibuffer too?
;; (setq ibuffer-never-show-predicates eon-boring-buffers)

(global-set-key (kbd "C-x C-b") #'ibuffer)

;;  ____________________________________________________________________________
;;; SCRATCH BUFFER

;; Set an initial major mode for the *scratch* buffer:

;; Lisp-interaction-mode is the default mode for the scratch buffer
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Lisp-Interaction>
;; (setq initial-major-mode #'lisp-interaction-mode)

;; You can set the scratch buffer to Org-mode which may be more useful
;; for quick notes, writing and literate programming
;; (setq initial-major-mode #'org-mode)

;; Should the *scratch* buffer contain some initial content?
(setq initial-scratch-message "")

;; Quickly jump to the *scratch* buffer
(defun eon-scratch ()
  "Jump to the *scratch* buffer. If it does not exist, create it."
  (interactive)
  (switch-to-buffer "*scratch*"))
(define-key ctl-z-s-map (kbd "s") #'eon-scratch)

;;  ____________________________________________________________________________
;;; VISITING FILES AT POINT

;; "C-x C-v"       – Visit any resource under the cursor
;; "M-x ffap-menu" – Display a list of all resources mentioned in this buffer

(define-key ctl-z-map (kbd "C-.") #'find-file-at-point)

;;  ____________________________________________________________________________
;;; CLIPBOARD, COPY & PASTE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Killing>

;; Prevent duplicates to avoid cluttering the kill ring
(setq kill-do-not-save-duplicates t)

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
;; If you would like to install this 3rd-party package, uncomment and evaluate
;; the following expression – either via "C-M-x", or simply restart Emacs:
;; (use-package xclip
;;  :ensure t)

;; Copy the full path of the current file
(defun eon-copy-file-path ()
  "Copy the full path of the current buffer's file to the kill ring."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the kill ring."
               filename))))

;; Simple alternative for 'yank-pop' – present a selection of the kill ring
(defun eon-insert-kill-ring-item ()
  "Select and insert an item from the 'kill-ring'."
  (interactive)
  (insert (completing-read "Yank: " kill-ring nil t)))
(global-set-key (kbd "M-y") #'eon-insert-kill-ring-item)

;; Copy & paste between Windows and Emacs running within WSL
;; (Windows Subsystem for Linux) — which is technically a Linux, not Windows

;; Copy "kill" text from an Emacs buffer for pasting it into a Windows app
(when (and (eon-linp)
           (file-exists-p "/mnt/c/Windows/System32/clip.exe"))
  (defun eon-wsl-copy (start end)
    "Copy selected text into the Windows clipboard."
    (interactive "r")
    (let ((default-directory "/mnt/c/"))
      (shell-command-on-region start end "clip.exe")))
  (define-key ctl-z-map (kbd "C-w") #'eon-wsl-copy))

;; Paste "yank" text into Emacs buffer that has been copied from a Windows app
(when (and (eon-linp)
           (file-exists-p "/mnt/c/Windows/System32/clip.exe"))
  (defun eon-wsl-paste ()
    "Paste contents from the Windows clipboard into the Emacs buffer."
    (interactive)
    (let ((coding-system-for-read 'dos)
          (default-directory "/mnt/c/"))
      (insert
       (substring
        (shell-command-to-string "powershell.exe -command 'Get-Clipboard'")
        0 -1))))
  (define-key ctl-z-map (kbd "C-y") #'eon-wsl-paste))

;;  ____________________________________________________________________________
;;; BACKUP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Backup>

;; CAUTION: This mode makes copies of the files you are editing.
;; If you're editing files with sensitive data (e.g. on temporally mounted,
;; encrypted devices), either disable this mode or specify the location
;; where to save (or not to save) backup copies of these files.

;; Make backup before editing?
(setq make-backup-files t)

;; Where to save the backups?
;; Specify file name/path patterns and directories ("REGEXP" . "DIRECTORY").
;; Files with sensitive content can be specified (excluded) here, too.
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/"))))

;; Apply the same backup policy for Tramp files on their hosts
;; like the policy for local files
(setq tramp-backup-directory-alist backup-directory-alist)

;; Backup settings
(setq backup-by-copying t
      backup-by-copying-when-linked t
      kept-new-versions 5
      kept-old-versions 5
      delete-old-versions t
      version-control t
      vc-make-backup-files t)

;;  ____________________________________________________________________________
;;; LOCKFILES
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Interlocking>

;; Let Emacs keep track of files currently visited?
(setq create-lockfiles nil)

;;  ____________________________________________________________________________
;;; AUTO-SAVE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Auto-Save>

;; Enable auto-save to safeguard against data loss?
;; The `recover-file' or `recover-session' functions can be used
;; to restore auto-saved data
(setq auto-save-default t)
(setq auto-save-no-message t)

;; Do not auto-disable auto-save after deleting large chunks of text
(setq auto-save-include-big-deletions t)

;; Auto-save locations
(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "autosave-tramp/" user-emacs-directory))

;; Auto save options
(setq kill-buffer-delete-auto-save-files t)

;;  ____________________________________________________________________________
;;; HELP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Help>

;; Show all options when running 'apropos' "C-h a" (fulltext search)
(setq apropos-do-all t)

;;  ____________________________________________________________________________
;;; SEARCH
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Search>

;; Switch search functions to make regex-search the default
(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)
(global-set-key (kbd "C-S-s") #'isearch-forward)
(global-set-key (kbd "C-S-r") #'isearch-backward)

;; Search and replace
;; The 'query-' variant asks for each string. Confirm with "SPC",
;; or jump to the next via "n"
(global-set-key (kbd "M-%") #'query-replace-regexp)
(global-set-key (kbd "C-M-%") #'replace-regexp)

;;  ____________________________________________________________________________
;;; RECENT FILES

;; Turn on recent file mode to visit recently edited files
(recentf-mode 1)

(setq recentf-max-menu-items 10
      recentf-max-saved-items 100)

;; Ignore some recently visited files, eg. to prevent them from showing up
;; amongst recent files after package upgrades
(add-to-list 'recentf-exclude
             (expand-file-name (concat user-emacs-directory "elpa/")))
(add-to-list 'recentf-exclude
             "^/\\(?:ssh\\|su\\|sudo\\)?:")

;; Use 'completing-read' to choose between recent files
(defun eon-find-recentf ()
  "Find recent file via completion in the minibuffer."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list nil t) nil))
(global-set-key (kbd "C-x f") #'eon-find-recentf)

;;  ____________________________________________________________________________
;;; DIRED
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Dired>

;; The `dired' keybinding is "C-x d". This new keybinding is in accordance
;; with "C-x C-f" for visiting files
(global-set-key (kbd "C-x C-d") #'dired)

;; Switch to wdired-mode and edit directory content like a text buffer
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "e") #'wdired-change-to-wdired-mode))

;; Don't accumulate useless Dired buffers
(setq dired-kill-when-opening-new-dired-buffer t)

;; Directory listings
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Hide details in file listings? Toggle via "S-("
            (dired-hide-details-mode 1)
            ;; Highlight current line?
            (hl-line-mode 1)))

;; Listing columns; Switch arguments with "C-u s" e.g. hide backups with -B
(setq-default dired-listing-switches "-lhFA -v --group-directories-first")

;; Copying files/directories
(setq dired-recursive-copies 'always)

;; Create directories if they don't exist
(setq dired-create-destination-dirs 'ask)

;; Mimic dual-pane file managers?
(setq dired-dwim-target t)

;; Images
(setq image-dired-thumb-margin 1
      image-dired-thumb-relief 0
      ;; Store thumbnails in the system-wide thumbnail location
      ;; e.g. ~/.local/cache/thumbnails to make them reusable by other programs
      image-dired-thumbnail-storage 'standard-large)

;; Linux/Unix only: hit "M-RET" to open files in desktop app
(when (eon-linp)
  (defun eon-dired-xdg-open ()
    "Open files and folders with the default desktop app."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "xdg-open" nil 0 nil file)
      (message "Opening %s done" file)))
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "M-RET") #'eon-dired-xdg-open)))

;;  ____________________________________________________________________________
;;; FILE HANDLING

(defun eon-trash-on ()
  "Delete files by moving to the system trash."
  (interactive)
  (setq delete-by-moving-to-trash t)
  (setq dired-recursive-deletes 'always)  ; don't ask when directory not empty
  (message "Trash on: Deleted files will go to system trash."))

(defun eon-trash-off ()
  "Delete files immediately."
  (interactive)
  (setq delete-by-moving-to-trash nil)
  (setq dired-recursive-deletes 'top)  ; ask when directory not empty
  (message "Trash off: Files will be deleted immediately!"))

(eon-trash-on)  ; set the default

;; Use the system trash when deleting files?
(setq remote-file-name-inhibit-delete-by-moving-to-trash t)

;; Resolve symlinks so that operations are conducted from the file's directory?
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Auto refresh dired (and others) when contents change?
(setq global-auto-revert-non-file-buffers t
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)
(global-auto-revert-mode 1)

;; Configure Ediff to use a single frame and split windows horizontally
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;;  ____________________________________________________________________________
;;; COMINT

(setq comint-input-ignoredups t
      comint-prompt-read-only t
      comint-buffer-maximum-size 2048
      comint-scroll-to-bottom-on-input 'this)

;;  ____________________________________________________________________________
;;; ESHELL
;; <https://www.gnu.org/software/emacs/manual/html_mono/eshell.html>

;; Eshell is not a terminal emulator, but a shell equivalent to Bash or Fish
;; that runs within Emacs. It is independent from the OS. Eshell looks like
;; a Posix shell superficially, but is also a REPL for Emacs Lisp expressions.

;; Get rid of the Eshell startup message?
(setq eshell-banner-message "")

;; List directory content after changing into it?
(setq eshell-list-files-after-cd t)

;; To open more than one eshell buffer: "C-u C-z e e"
(define-key ctl-z-e-map (kbd "e") #'eshell)

;;  ____________________________________________________________________________
;;; SHELL
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Shell-Mode>

;; This is also no terminal emulator, but a buffer to issue shell commands
;; and display their output

;; Set another shell than your default one?
;; (setq shell-file-name "/usr/bin/bash")

;; To open more than one shell buffer: "C-u C-z e s"
(define-key ctl-z-e-map (kbd "s") #'shell)

;;  ____________________________________________________________________________
;;; PROCED

;; Show and manage OS processes, like the command line programs top and htop

(setq proced-auto-update-interval 1)

(setq-default proced-auto-update-flag t
              proced-descend t)

;;  ____________________________________________________________________________
;;; NET-UTILS

(setq netstat-program "netstat"
      netstat-program-options '("-atupe"))

;;  ____________________________________________________________________________
;;; WEB BROWSERS

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;; EWW BUILT-IN BROWSER
;; <https://www.gnu.org/software/emacs/manual/html_mono/eww.html#Top>

(setq url-privacy-level '(email lastloc cookies))
(url-setup-privacy-info)

(defun eon-user-agent (browser-name)
  "Accepts a symbol in order to return a pre-defined user-agent string.
BROWSER-NAME can be either `safari-macos', `safari-iphone', `w3m' or t,
which sets the default `eww' user-agent according to `url-privacy-level'."
  (cond
   ((equal browser-name 'safari-macos)
    (setq url-user-agent
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/603.3.8 (KHTML, like Gecko) Version/11.0.1 Safari/603.3.8"))
   ((equal browser-name 'safari-iphone)
    (setq url-user-agent
          "Mozilla/5.0 (iPhone; CPU iPhone OS 18_2 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.2 Mobile/15E148 Safari/604.1"))
   ((equal browser-name 'w3m)
    (setq url-user-agent
          "w3m/0.5.3+git2020050"))
   (t
    (setq url-user-agent
          'default))))

;; Set the user agent for the internal web browser
(eon-user-agent 'safari-iphone)

;; Per default, open links with the internal web browser
(setq browse-url-browser-function #'eww-browse-url)

;; Secondary web browser
(setq browse-url-secondary-browser-function #'browse-url-default-browser)
;; (setq browse-url-browser-function #'browse-url-firefox)
;; (setq browse-url-generic-program (executable-find "nyxt")
;;        browse-url-browser-function #'browse-url-generic)

;; Keybindings
(define-key ctl-z-w-map (kbd "W") #'browse-url)
(define-key ctl-z-w-map (kbd "w") #'browse-web)

;;  ____________________________________________________________________________
;;; EMAIL SENDING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Sending-Mail>

;; TODO: Send emails directly from Emacs using SMTP – example template

;; Should be defined first
;; (setq user-mail-address "mail@example.org")

;; To avoid typing in the password for each email, specify SMTP account(s)
;; in '~/.authinfo.gpg'. Here's a content template for authinfo.gpg:
;; machine mail.example.org port 587 login myuser password mypassword

;; Emacs email variables
(with-eval-after-load 'smtpmail
  (setq send-mail-function #'smtpmail-send-it
        smtpmail-smtp-server "localhost"
        smtpmail-stream-type 'starttls
        smtpmail-smtp-service 1025      ; default port: 587
        smtpmail-queue-dir "~/.mail/queued-mail/"
        smtpmail-smtp-user user-mail-address
        smtpmail-debug-info nil))

(setq message-kill-buffer-on-exit t)

;;  ____________________________________________________________________________
;;; CALENDAR
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Calendar_002fDiary>

(setq calendar-date-style 'iso
      calendar-week-start-day 1
      calendar-weekend-days '(6 0))

;;  ____________________________________________________________________________
;;; GENERAL EDITING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Basic>

;; UTF-8
(prefer-coding-system 'utf-8)

;; Remember the place where the cursor was last time?
(setq save-place-limit 500)
(save-place-mode 1)

;; Set desired line length in characters
(setq-default fill-column 80)

;; While a text selection is active, typing characters replaces
;; the selection with the typed characters (default: -1 = off)
(delete-selection-mode -1)

;; Save always with a final new line?
(setq require-final-newline t)

;; Better than the default 'just-one-space' (was M-SPC before)
(global-set-key (kbd "M-S-SPC") #'cycle-spacing)

;; Count lines, words and chars (buffer or region)
(global-set-key (kbd "C-x l") #'count-words)

;; Kill up to character
(global-set-key (kbd "M-z") #'zap-up-to-char)

;;  ____________________________________________________________________________
;;; LINE NUMBERS
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Display-Custom>

;; Line numbers on or off? Toggle with "M-x display-line-numbers-mode" or
;; set it here for all programming modes. Goto line: "M-g M-g"
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)))

;;  ____________________________________________________________________________
;;; LINE WRAPPING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Line-Truncation>

;; Truncate long lines in programming modes by default.
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local truncate-lines t)))

;; If you prefer to see all text within a window in programming modes, enable
;; visual line breaks, a.k.a soft wrapping
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (visual-line-mode 1)))

;;  ____________________________________________________________________________
;;; FOLDING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Hideshow>

;; Code folding on or off? Show available commands: "M-x hs-"
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (hs-minor-mode 1)))

;;  ____________________________________________________________________________
;;; INDENTATION
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Indentation>

(setq-default indent-tabs-mode nil  ; don't use tabs but spaces
              tab-width 4)          ; set display width for tab characters

;; Delete the whole indentation instead spaces one-by-one via <backspace>?
;; (Possibly shadowed by 3rd-party packages like `smartparens-mode'
(setq backward-delete-char-untabify-method 'hungry)

;; Enable indentation and completion using the TAB key
(setq-default tab-always-indent 'complete)

;;  ____________________________________________________________________________
;;; COMMENTING

;; Enable multi-line commenting to ensure that `comment-indent-new-line'
;; properly continues comments onto new lines.
(setq comment-multi-line t)

;;  ____________________________________________________________________________
;;; BRACKETS / PARENTHESIS
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Parentheses>

;; How to display matching parens generally?
(setq show-paren-style 'parenthesis
      show-paren-delay 0.125
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;; Auto-close parens, brackets and quotes?
(electric-pair-mode 1)

;;  ____________________________________________________________________________
;;; WHITESPACE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Useless-Whitespace>

;; Indicate trailing whitespace in programming modes?
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; Indicate trailing whitespace in "text" modes?
(add-hook 'text-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; Cleanup trailing whitespace
(define-key ctl-z-c-map (kbd "w") #'whitespace-cleanup)

;;  ____________________________________________________________________________
;;; SYNTAX CHECK / LINTER
;; <https://www.gnu.org/software/emacs/manual/html_mono/flymake.html>

;; There are various syntax-checkers coming with the built-in Flymake mode,
;; and additional checkers can be installed as 3rd-party packages via
;; "M-x package-install <RET> flymake-".

;; Style the Flymake widget in the modeline
(setq flymake-mode-line-format
      '(" " "FlyM" flymake-mode-line-exception flymake-mode-line-counters))

;; Stop when first/last error is reached
(setq flymake-wrap-around nil)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-g E") #'flymake-show-project-diagnostics)
  (define-key flymake-mode-map (kbd "M-g e") #'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "M-g n") #'flymake-goto-next-error) ; default
  (define-key flymake-mode-map (kbd "M-g p") #'flymake-goto-prev-error))  ; default

;;  ____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md/>

(with-eval-after-load 'eglot
  ;; Shutdown language server after closing last file?
  (setq eglot-autoshutdown t)
  ;; Allow edits without confirmation?
  (setq eglot-confirm-server-initiated-edits nil)
  ;; Show code action indicators?
  (setq eglot-code-action-indications nil)
  ;; Common keybindings
  (define-key ctl-z-c-map (kbd "r") #'eglot-rename)
  (define-key ctl-z-c-map (kbd "f") #'eglot-format)
  (define-key ctl-z-c-map (kbd "F") #'eglot-format-buffer)
  (define-key ctl-z-c-map (kbd "a") #'eglot-code-actions))

;; Eglot comes with a fairly complete set of associations of major-modes
;; to popular language servers predefined. If you need to add server
;; associations to the default list, use add-to-list. For example, you can
;; add it to the alist like this:
;;
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(lua-mode . ("lua-language-server" "--stdio"))))
;;
;; This will invoke the program tools with the command-line argument --stdio
;; in support of editing source files for which Emacs turns on foo-mode, and
;; will communicate with the program via the standard streams. As usual with
;; invoking programs, the executable file fools should be in one of the
;; directories mentioned by the exec-path variable (see Subprocess Creation
;; in GNU Emacs Lisp Reference Manual), for Eglot to be able to find it.
;; Sometimes, multiple servers are acceptable alternatives for handling a
;; given major-mode. In those cases, you may combine the helper function
;; eglot-alternatives with the functional form of eglot-server-programs.
;;
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                `(lua-mode . ,(eglot-alternatives
;;                               '(("lua-language-server" "--stdio")
;;                                 ("lua-lsp" "--stdio"))))))

;;  ____________________________________________________________________________
;;; TREE-SITTER

;; Define grammar specs for ts-modes already built into Emacs.
;; Grammars can be built and installed via:
;; - `eon-treesitter-ensure-grammar' (declarative)
;; - `treesit-install-language-grammar' (interactive, single grammar)
;; - `eon-treesitter-install-all' (interactive, all grammars)
(defvar eon-treesitter-specs
  '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
    (c          "https://github.com/tree-sitter/tree-sitter-c")
    (c-sharp    "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (cmake      "https://github.com/uyha/tree-sitter-cmake")
    (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
    (css        "https://github.com/tree-sitter/tree-sitter-css")
    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
    (elixir     "https://github.com/elixir-lang/tree-sitter-elixir")
    (go         "https://github.com/tree-sitter/tree-sitter-go")
    (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
    (heex       "https://github.com/phoenixframework/tree-sitter-heex")
    (html       "https://github.com/tree-sitter/tree-sitter-html")
    (java       "https://github.com/tree-sitter/tree-sitter-java")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                "master" "src")
    (json       "https://github.com/tree-sitter/tree-sitter-json")
    (lua        "https://github.com/tree-sitter-grammars/tree-sitter-lua")
    (php        "https://github.com/tree-sitter/tree-sitter-php"
                nil "php/src")
    (python     "https://github.com/tree-sitter/tree-sitter-python")
    (ruby       "https://github.com/tree-sitter/tree-sitter-ruby")
    (rust       "https://github.com/tree-sitter/tree-sitter-rust")
    (toml       "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx        "https://github.com/tree-sitter/tree-sitter-typescript"
                "master" "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                "master" "typescript/src")
    (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
  "Tree-sitter grammar specs: list of (LANG URL [REVISION] [SOURCE-DIR]).
Add further specs without building/installing via `eon-treesitter-add-specs'

Only LANG and URL are mandatory.

LANG is the language symbol.

URL is the URL of the grammar’s Git repository or a directory
where the repository has been cloned.

REVISION is the Git tag or branch of the desired version,
defaulting to the latest default branch.

SOURCE-DIR is the relative subdirectory in the repository in which
the grammar’s parser.c file resides, defaulting to \"src\".")

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;; Internal utilities

(defun eon-treesitter--spec-p (x)
  "Return non-nil if X is a spec tuple (LANG URL [REV] [DIR])."
  (pcase x
    (`(,(and _ (pred symbolp)) ,(and _ (pred stringp)) . ,_) t)
    (_ nil)))

(defun eon-treesitter--normalize-args (args)
  "Normalize ARGS into a flat list of elements (symbols or spec tuples).
Accepted forms:
  - Many args: symbols and/or spec tuples -> returned as-is (copy).
  - Single list arg:
      - If it's a single SPEC tuple, wrap it as a one-element list.
      - If it's a list of symbols/specs (or a mix), return that list.
  - Single non-list arg: wrap as one-element list."
  (pcase args
    ('() nil)
    (`(,(and x (pred eon-treesitter--spec-p))) (list x)) ; single spec
    (`(,(and x (pred listp))) (copy-sequence x))         ; list of elems
    (`(,x) (list x))                                     ; single atom
    (_ (copy-sequence args))))                           ; many args

(defun eon-treesitter--merge-into-alist (alist specs)
  "Return ALIST with SPECS merged; later entries overwrite by LANG."
  (if (null specs)
      alist
    (let ((kill (let ((h (make-hash-table :test 'eq)))
                  (dolist (s specs) (puthash (car s) t h))
                  h)))
      (append
       (cl-remove-if (lambda (pair) (gethash (car pair) kill)) alist)
       specs))))

(defun eon-treesitter--dedupe-specs (specs)
  "Remove duplicate LANG specs in SPECS, keeping the last occurrence."
  (let ((seen-langs (make-hash-table :test 'eq))
        (result '()))
    (dolist (spec (reverse specs))       ; right-to-left, keep last
      (let ((lang (car spec)))
        (unless (gethash lang seen-langs)
          (puthash lang t seen-langs)
          (push spec result))))
    result))

(defconst eon-treesitter--missing-spec-error
  "Treesitter-spec not available per default.
Provide full treesitter-spec as a list of (LANG URL [REVISION] [SOURCE-DIR])"
  "Error message when a default spec is unavailable.")

(defun eon-treesitter--resolve-one (lang-or-spec)
  "Resolve LANG-OR-SPEC to the canonical spec tuple (list (LANG URL [REV] [DIR]))
from `eon-treesitter-specs` when given a LANG symbol; pass tuples through
unchanged. Signal a user-error if LANG is unknown."
  (cond
   ((eon-treesitter--spec-p lang-or-spec) lang-or-spec)
   ((and (symbolp lang-or-spec) (not (keywordp lang-or-spec)))
    (or (assq lang-or-spec eon-treesitter-specs)
        (user-error "%s" eon-treesitter--missing-spec-error)))
   ((keywordp lang-or-spec)
    (user-error
     "eon-treesitter-ensure-grammar: Only specs or language symbols accepted"))
   (t
    (user-error
     "eon-treesitter-ensure-grammar: Invalid element %S (expect (LANG URL \
[REV] [DIR]) or a language symbol)" lang-or-spec))))

(defun eon-treesitter--resolve-elements (langs-or-specs)
  "Resolve LANGS-OR-SPECS to a list of canonical spec tuples.
Each element may be a spec tuple or a LANG symbol registered in
`eon-treesitter-specs`.  Signal a user-error if any LANG is unknown."
  (mapcar #'eon-treesitter--resolve-one langs-or-specs))

(defun eon-treesitter--require-toolchain ()
  "Fail fast with helpful messages if toolchain is missing."
  (unless (require 'treesit nil t)
    (user-error
     "Tree-sitter not available in this Emacs (feature `treesit' missing)"))
  (unless (executable-find "git")
    (user-error "tree-sitter: `git` not found on PATH"))
  (unless (or (executable-find "cc")
              (executable-find "gcc")
              (executable-find "clang"))
    (user-error "tree-sitter: No C compiler found (cc/gcc/clang)")))

(defun eon-treesitter-setup-specs (&optional specs)
  "Merge SPECS into `treesit-language-source-alist', overwriting by LANG.
If SPECS is nil, use `eon-treesitter-specs'."
  (setq treesit-language-source-alist
        (eon-treesitter--merge-into-alist
         treesit-language-source-alist
         (or specs eon-treesitter-specs))))

(with-eval-after-load 'treesit
  (eon-treesitter-setup-specs))

(defun eon-treesitter--ensure-impl (specs reinstall)
  "Core installer for SPECS. If REINSTALL is non-nil, rebuild even if present.
Return an alist of (LANG . STATUS)."
  (eon-treesitter--require-toolchain)
  (pcase-let*
      ((resolved (eon-treesitter--resolve-elements specs))
       (to-merge
        (seq-filter
         (lambda (spec)
           (let ((existing (assq (car spec) eon-treesitter-specs)))
             (not (equal spec existing))))
         resolved)))
    ;; Persist any changed specs first (if any)
    (when to-merge
      (apply #'eon-treesitter-add-specs to-merge))
    ;; Ensure source alist is up to date
    (eon-treesitter-setup-specs)
    ;; Perform installation per language (preserve order)
    (mapcar
     (lambda (spec)
       (let ((lang (car spec)))
         (condition-case err
             (let* ((had   (treesit-language-available-p lang))
                    (needs (or reinstall (not had))))
               (when needs
                 (let ((default-directory (expand-file-name "~")))
                   (treesit-install-language-grammar lang)))
               (cons
                lang
                (if (treesit-language-available-p lang)
                    (if had 'present 'installed)
                  (cons 'error
                        (format
                         "Installed but not available; check \
`treesit-extra-load-path' (currently: %S)"
                         treesit-extra-load-path)))))
           (error
            (cons lang (cons 'error (error-message-string err)))))))
     (eon-treesitter--dedupe-specs resolved))))

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;; Public API

(defun eon-treesitter-add-specs (&rest specs)
  "Add SPECS to `eon-treesitter-specs' and merge them into the source alist.
Use this only if you merely want to register SPECS, but not build/install
them.

SPECS can be many specs or a single list of specs.

Each spec has the form (LANG URL [REVISION] [SOURCE-DIR]).

Only LANG and URL are mandatory.

LANG is the language symbol.

URL is the URL of the grammar’s Git repository or a directory where the
repository has been cloned.

REVISION is the Git tag or branch of the desired version, defaulting to
the latest default branch.

SOURCE-DIR is the relative subdirectory in the repository in which the
grammar’s parser.c file resides, defaulting to \"src\".

Return the updated `eon-treesitter-specs'."
  (let* ((xs0 (eon-treesitter--normalize-args specs))
         (bad (seq-find (lambda (x)
                          (not (eon-treesitter--spec-p x)))
                        xs0)))
    (when bad
      (user-error "eon-treesitter-add-specs: Not a spec: %S" bad))
    (let ((xs (eon-treesitter--dedupe-specs xs0)))
      ;; Merge into registry (new specs take precedence)
      (setq eon-treesitter-specs
            (eon-treesitter--merge-into-alist eon-treesitter-specs xs))
      ;; Reflect into treesit-language-source-alist
      (eon-treesitter-setup-specs xs)
      eon-treesitter-specs)))

(defun eon-treesitter-ensure-grammar (&rest args)
  "Ensure that grammar(s) for ARGS are built and installed.

ARGS may be:
  - Spec tuples: (LANG URL [REVISION] [SOURCE-DIR])
  - Bare LANG symbols as identifiers for specs in `eon-treesitter-specs'
  - A single list containing either of the above forms

Rules:
  - If called with a SPEC tuple whose LANG already exists
    in `eon-treesitter-specs', the existing entry is compared for exact
    equality. If different, the new SPEC is added and takes precedence.
  - If called with bare LANG symbols, they must already exist
    in `eon-treesitter-specs', otherwise signal an error.

Returns an alist of (LANG . STATUS) where STATUS is one of:
  present | installed | (error . STRING)."
  (let ((elems (eon-treesitter--normalize-args args)))
    (when (null elems)
      (user-error "eon-treesitter-ensure-grammar: No specs provided"))
    (eon-treesitter--ensure-impl elems nil)))

(cl-defun eon-treesitter-install-all (&key reinstall)
  "Install all grammars in `eon-treesitter-specs'.

Accepts keyword arg :REINSTALL (non-nil to rebuild grammar even if present).

When called interactively with a prefix argument, acts like :reinstall t.

Returns the same (LANG . STATUS) alist as `eon-treesitter-ensure-grammar'."
  (interactive (list :reinstall current-prefix-arg))
  (eon-treesitter--ensure-impl eon-treesitter-specs (and reinstall t)))

;;  ____________________________________________________________________________
;;; COMPILING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Building>

;; Keep the compilation buffer in the background, except when there's an error
(add-to-list
 'display-buffer-alist
 '("\\*.*compilation\\*" (display-buffer-no-window)))

;; Scroll to the first error
(setq compilation-scroll-output 'first-error)

;; Recenter to the middle of the window for `compile-goto-error'
(setq next-error-recenter '(4))

;;  ____________________________________________________________________________
;;; TEXT MODES / WRITING PROSE

;; Sentences end with a single space
(setq sentence-end-double-space nil)

;; TODO Add Flyspell / Ispell presets here

;;  ____________________________________________________________________________
;;; ORG MODE
;; <https://orgmode.org/>
;; <https://orgmode.org/org.html>

;; Org provides functionality far beyond that of computational notebooks
;; such as Jupyter or R Markdown.

;; Set a default location to look for Org files; but you can have
;; that directory anywhere you like
(setq org-directory (expand-file-name "~/Documents/org/"))

(defun eon-visit-org-directory ()
  "Show the Org directory in Dired."
  (interactive)
  (dired org-directory))

;; Visit the `org-directory' in Dired via `C-z o d'
(define-key ctl-z-o-map (kbd "d") #'eon-visit-org-directory)

;; Turn on visual word wrapping
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Visual-Line-Mode>
(add-hook 'org-mode-hook #'visual-line-mode)

;; Alignment of tags at the end of headlines
(setq  org-auto-align-tags t
       org-tags-column 0)

;; Toggle indicator for headlines
(setq org-ellipsis " ▼ ")

;; Don't add leading indentation to code blocks, remove them during export
(setq org-edit-src-content-indentation 0
      org-src-preserve-indentation nil)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ORG CAPTURE
;; <https://orgmode.org/org.html#Capture>

;; Capture a note via `C-z o c'
(define-key ctl-z-o-map (kbd "c") #'org-capture)
;; Put newer notes on top of the file
(setq org-reverse-note-order t)

;; Set a default target for storing notes
(setq org-default-notes-file (concat org-directory "notes.org"))

(defun eon-visit-org-notes ()
  "Visit the Org notes file."
  (interactive)
  (find-file org-default-notes-file))

;; Visit the default notes file via `C-z o o'
(define-key ctl-z-o-map (kbd "o") #'eon-visit-org-notes)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ORG TODO
;; <https://orgmode.org/org.html#TODO-Items>

;; Set some sensible default states for todo-items
(setq org-todo-keywords
      '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
        (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
        (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ORG AGENDA
;; <https://orgmode.org/org.html#Agenda-Views>

(setq org-agenda-files (list org-directory))

;; Visit your Org agenda via `C-z o a'
(define-key ctl-z-o-map (kbd "a") #'org-agenda)

(add-hook 'org-agenda-mode-hook
          (lambda ()
            ;; Highlight current line in Org agenda?
            (hl-line-mode 1)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ORG LINKS
;; <https://orgmode.org/org.html#Hyperlinks>

;; Store a link via `C-z o L'
(define-key ctl-z-o-map (kbd "L") #'org-store-link)

;; Insert a link into an Org file via `C-z o l'
(define-key ctl-z-o-map (kbd "l") #'org-insert-link)

;; Toggle visible/hidden links via `C-z o M-l'
(define-key ctl-z-o-map (kbd "M-l") #'org-toggle-link-display)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ORG PUBLISH

;; Select a project to publish a project via `C-z o p';
;; This can be used to generate and publish a static blog, ebooks, etc.
(define-key ctl-z-o-map (kbd "p") 'org-publish)

;; Speed up publishing by skipping files that haven't been changed
(setq org-publish-list-skipped-files nil)

;; Where to place the directory containing the timestamps about changed files
(setq org-publish-timestamp-directory
      (concat user-emacs-directory "org-timestamps/"))

(defun org-publish-unchanged-files-toggle ()
  "Toggle whether to re-export Org files that haven't been changed."
  (interactive)
  (if org-publish-use-timestamps-flag
      (progn (setq org-publish-use-timestamps-flag nil)
             (message "Re-export unchanged files"))
    (progn (setq org-publish-use-timestamps-flag t)
           (message "Don't re-export unchanged files (default)"))))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ORG EXPORT

;; HTML export
(setq org-html-checkbox-type 'unicode
      org-html-prefer-user-labels t
      org-html-self-link-headlines t)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; Literate Programming
;; <https://orgmode.org/worg/org-contrib/babel/intro.html>

;; Activate code blocks via Babel languages
(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))

;; In case you're using the Emacs ONTOP extensions, further languages
;; should not be configured here, but within their specific ONTOP modules
;; ("eon-*.el" files)

;;  ____________________________________________________________________________
;;; LISP LANGUAGES
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Executing-Lisp>

(defvar eon-lisp-buffer-modes
  '(emacs-lisp-mode-hook
    lisp-interaction-mode-hook
    lisp-mode-hook
    scheme-mode-hook))

(defvar eon-lisp-interactive-modes
  '(lisp-interaction-mode-hook
    ielm-mode-hook
    inferior-lisp-mode-hook
    inferior-scheme-mode-hook
    eval-expression-minibuffer-setup-hook))

;; Emacs Lisp is supported by Flymake, so let's use it per default
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
(add-hook 'lisp-interaction-mode-hook (lambda () (flymake-mode -1)))

;; Emacs Lisp evaluation: don't truncate printed lists
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Additional keybinding resembling other sexp-related keybindings
(global-set-key (kbd "C-M-<backspace>") #'backward-kill-sexp)

;;  ____________________________________________________________________________
(provide 'eon)
;;; eon.el ends here
