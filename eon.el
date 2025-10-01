;;; eon.el --- Emacs ONboard Starter Kit -*- lexical-binding: t; -*-

;;    ▒░▒░▒░   ▒░     ▒░ ▒░▒░▒░▒░     ▒░▒░▒░      ▒░    ▒░▒░▒░▒░    ▒░▒░▒░▒░
;;   ▒░    ▒░  ▒░▒░   ▒░  ▒░     ▒░  ▒░    ▒░    ▒░▒░    ▒░     ▒░   ▒░    ▒░
;;  ▒░      ▒░ ▒░ ▒░  ▒░  ▒░     ▒░ ▒░      ▒░  ▒░  ▒░   ▒░     ▒░   ▒░     ▒░
;;  ▒░      ▒░ ▒░  ▒░ ▒░  ▒░▒░▒░▒░  ▒░      ▒░ ▒░    ▒░  ▒░▒░▒░▒░    ▒░     ▒░
;;  ▒░      ▒░ ▒░   ▒░▒░  ▒░     ▒░ ▒░      ▒░ ▒░▒░▒░▒░  ▒░   ▒░     ▒░     ▒░
;;   ▒░    ▒░  ▒░     ▒░  ▒░     ▒░  ▒░    ▒░ ▒░      ▒░ ▒░     ▒░   ▒░    ▒░
;;    ▒░▒░▒░  ▒░      ▒░ ▒░▒░▒░▒░     ▒░▒░▒░  ▒░      ▒░ ▒░      ▒░ ▒░▒░▒░▒░

;; Emacs ONboard offers a clean slate to build your personal Emacs config.
;; It stays as close as possible to vanilla Emacs, but offers some convenience
;; and a much better user experience, while only relying on built-in packages.

;; Copyright (C) 2021–2025 Dan Dee
;;
;; Emacs ONboard is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; Emacs ONboard is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; Maintainer: Dan Dee <monkeyjunglejuice@pm.me>
;; URL: https://github.com/monkeyjunglejuice/emacs.onboard
;; Created: 28 Apr 2021
;; Version: 2.1.0
;; Package: eon
;; Package-Requires: ((emacs "30.1"))
;; Keywords: config dotemacs convenience
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Keybindings:
;;
;; "M-x"  Show all commands
;;        – hold down the "Meta key" and press <x>
;;        – the "Meta key" is usually <Alt> on Linux/Windows and <Option> on Mac
;;
;; "C-g"  Get out! Press <Ctrl>+<g> to cancel whatever happens – or hit 3x <ESC>
;;
;; "<leader> x t"  Toggle between dark and light theme
;;
;;; Examples:
;;
;; "M-x eon-"                      Show all commands defined by Emacs ONBOARD
;; "M-x eon-visit-user-init-file"  Visit main config file: .emacs or init.el
;; "M-x check-parens"              Check if all parens match in Emacs Lisp code
;; "M-x help"                      Reach the ultimate help menu
;;
;; "C-h o" Place the cursor behind a keyword, function, variable or other symbol
;;         to issue the command `describe-symbol' via keybinding
;;         and view the symbol's documentation
;;
;; "M-;"   Comment/uncomment a selected piece of text or code
;;
;;; Code:

;; _____________________________________________________________________________
;;; GARBAGE COLLECTION
;; <https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Garbage-Collection>

;; Set a high value of 1 GB to prevent frequent garbage collections
;; during initialization
(setopt gc-cons-threshold #x40000000)  ; default threshold is 800 KB

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
(setopt garbage-collection-messages nil)

;; Diagnostics
(add-hook 'window-setup-hook
          (lambda ()
            (message "Emacs started in %s with %d garbage collections."
                     (format "%.3f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; _____________________________________________________________________________
;;; PACKAGE MANAGEMENT INIT

;; Browse, select and install 3rd-party packages with "M-x list-packages RET"
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Packages>
;; ... or do "M-x info-emacs-manual s packages RET" to read it within Emacs

;; The package setup respects when `package-enable-at-startup' is set to nil in
;; early-init.el (if there's any), so that you can use other package managers
;; like Straight or Elpaca without issues.

(when package-enable-at-startup
  (require 'package)

  ;;; 1st priority: Gnu Elpa, enabled by default

  ;;; 2nd priority: Non-Gnu Elpa, enabled by default

  ;;; 3rd priority: Melpa
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)

  ;;; 4th priority: Melpa stable
  ;; Install from melpa-stable only when a package from 'melpa' is broken.
  ;; Most packages there are utterly outdated.
  ;; (add-to-list 'package-archives
  ;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  )

;; _____________________________________________________________________________
;;; ELISP NATIVE COMPILATION / BYTECODE

;; Prevent stale elisp bytecode from shadowing more up-to-date source files?
(setopt load-prefer-newer t)

;; Natively compile packages at first use or immediately after installation?
(setopt package-native-compile t)

;; Native-compile .elc files asynchronously?
(setopt native-comp-jit-compilation t)

;; Ask whether to terminate asynchronous compilations on exit?
;; Prevents from interrupted compilations and leftover artifacts.
(setopt native-comp-async-query-on-exit t)

;; When to bring the buffer to the foreground?
(setopt warning-minimum-level :error)

;; Reduce bytecode compilation verbosity?
(setopt byte-compile-verbose nil)
(setopt byte-compile-warnings nil)

;; Reduce native code compilation verbosity?
(setopt native-comp-async-report-warnings-errors nil)
(setopt native-comp-warning-on-missing-source nil)

;; _____________________________________________________________________________
;;; EMACS SYSTEM LIMITS

;; Increase warning threshold
(setopt large-file-warning-threshold (* 64 1000000))

;; Increase undo limit
(setopt undo-limit 67108864          ; 64mb
        undo-strong-limit 100663296  ; 96mb
        undo-outer-limit 1006632960) ; 960mb

;; Increase the amount of data which Emacs reads from subprocesses
(setopt read-process-output-max (* 1024 1024))  ; 1 MB

;; _____________________________________________________________________________
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
(add-hook 'window-setup-hook
          (lambda ()
            (when (display-graphic-p)
              (select-frame-set-input-focus (selected-frame)))))

;; _____________________________________________________________________________
;;; USER INTERFACE

;; Menu bar: on/off by default?
(menu-bar-mode 1)

;; Scroll bar: on/off by default?
(when (display-graphic-p) (scroll-bar-mode -1))

;; Tool bar: on/off by default?
(when (display-graphic-p) (tool-bar-mode -1))

;; Tooltips: enable/disable?
(tooltip-mode -1)

;; Startup screen: on/off by default?
(setopt inhibit-startup-screen t)

;; Alarms: turn off?
(setopt ring-bell-function 'ignore)

;; _____________________________________________________________________________
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
(setopt blink-cursor-interval 0.3)

;; Blink cursor that often before going into solid state
(setopt blink-cursor-blinks 3)

;; Emphasize the cursor when running Emacs in a text terminal?
(setopt visible-cursor nil)

;; Make sure to highlight the current line only in the active window.
(setopt hl-line-sticky-flag nil)
(add-hook 'special-mode-hook
          (lambda ()
            ;; Highlight current line in special modes?
            (hl-line-mode 1)))

;; _____________________________________________________________________________
;;; GLOBAL DEFINITIONS & UTILITIES

;; Group for customizations
(defgroup eon nil
  "Emacs ONBOARD starter kit & ONTOP extension layer."
  :group 'convenience)

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

(defun eon-androidp ()
  "True if `system-type' is Android or something compatible.
For finer granularity, use the variables `system-type'
or `system-configuration' directly."
  (eq system-type 'android))

(defun eon-macp ()
  "True if `system-type' is MacOS or something compatible.
For finer granularity, use the variables `system-type'
or `system-configuration' directly."
  (eq system-type 'darwin))

;; Extended variant of `add-to-list' and friends

(defun eon-list-adjoin (cur elements &optional append compare-fn)
  "Return a new list like CUR with ELEMENTS added once.

ELEMENTS may be an item or a list.  If APPEND is non-nil, append items
left→right; otherwise prepend while preserving ELEMENTS order.
COMPARE-FN is the membership predicate (default `equal').  CUR is not
mutated.

Examples (CUR = (a b)):
  (eon-list-adjoin '(a b) 'c)                    ⇒ (c a b)
  (eon-list-adjoin '(a b) '(c a))                ⇒ (c a b)
  (eon-list-adjoin '(a b) '(d) t)                ⇒ (a b d)
  (eon-list-adjoin '(a b) '(\"x\" \"x\") nil #'string-equal)
                                                ⇒ (\"x\" a b)"
  (let* ((xs   (if (listp elements) elements (list elements)))
         (test (or compare-fn #'equal)))
    (cl-labels
        ((step (res items)
           (if (null items)
               res
             (let* ((x (car items))
                    (res* (if (cl-find x res :test test)
                              res
                            (if append
                                (append res (list x))
                              (cons x res)))))
               (step res* (cdr items))))))
      (step (copy-sequence cur)
            (if append xs (reverse xs))))))

(defmacro eon-add-to-list-setopt (var elements &optional append compare-fn)
  "Set VAR to CUR with ELEMENTS adjoined using a single `setopt' call.

VAR is a symbol naming a list variable.  ELEMENTS may be an item or a
list.  APPEND/COMPARE-FN as in `eon-list-adjoin'.  If VAR is unbound,
treat it as nil.

Examples (initial VAR = (a b)):
  (eon-add-to-list-setopt some-var 'c)
  (eon-add-to-list-setopt some-var '(c a))
  (eon-add-to-list-setopt some-var '(d) t)
  (eon-add-to-list-setopt some-var '(\"x\") nil #'string-equal)"
  `(setopt ,var
           (eon-list-adjoin
            (if (boundp ',var) ,var nil)
            ,elements ,append ,compare-fn)))

(defun eon-add-to-list (list-sym elements &optional append compare-fn)
  "Rebind LIST-SYM to a new list with ELEMENTS adjoined once.

LIST-SYM is a symbol naming a list variable.  ELEMENTS may be an item or
a list.  APPEND/COMPARE-FN as in `eon-list-adjoin'.  Returns the new
value of LIST-SYM.

Examples (LIST-SYM value starts as (a b)):
  (eon-add-to-list 'v 'c)                  ⇒ (c a b)
  (eon-add-to-list 'v '(c a))              ⇒ (c a b)
  (eon-add-to-list 'v '(d) t)              ⇒ (a b d)"
  (set list-sym
       (eon-list-adjoin
        (if (boundp list-sym) (symbol-value list-sym) nil)
        elements append compare-fn)))

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

;; Get all the parent major modes
(defun eon-get-parent-modes ()
  "Return major-mode and its parents (child first).
When called interactively, also echo the result."
  (interactive)
  (cl-labels ((collect (mode)
                (if-let ((p (get mode 'derived-mode-parent)))
                    (cons mode (collect p))
                  (list mode))))
    (let ((parents (collect major-mode)))
      (if (called-interactively-p 'interactive)
          (message "%S" parents)
        parents))))

;; _____________________________________________________________________________
;;; LEADER-KEY / LOCAL LEADER-KEY and KEYMAPS

;; To avoid clashes, new keybindings introduced by Emacs ONboard will usually
;; live under the leader prefix (with only a few exceptions).

;; ---> Defaults for graphical Emacs:
;; "C-," is the leader key, reach the local leader via "C-, C-,"
;; 
;; ---> Defaults for Emacs with terminal UI (invoked by "emacs -nw"):
;; "C-z" is the leader key, reach the local leader via "C-z C-z"

;; Terminal note: In `emacs -nw`, "C-z" is normally bound to suspend Emacs.
;; We rebind it as a leader, so it works in modern terminals (e.g. WezTerm).
;; If your TTY converts C-z to SIGTSTP before Emacs sees it (rare), disable
;; the suspend char or move it (see optional snippet below).
;; (add-hook 'tty-setup-hook
;;           (lambda ()
;;             (ignore-errors
;;               ;; No kernel suspend on ^Z
;;               (call-process "stty" nil nil nil "susp" "undef")
;;               ;; No XON/XOFF flow control stealing C-s/C-q
;;               (call-process "stty" nil nil nil "-ixon" "-ixoff"))))

;; Leader implementation

(defun eon-leader--set-key (sym value)
  (let ((old (and (boundp sym) (symbol-value sym))))
    (when (and old (stringp old) (> (length old) 0))
      (keymap-global-unset old t))
    (set-default sym value)
    (when (boundp 'ctl-z-map)
      (keymap-global-set value ctl-z-map))
    (when (and old (string= eon-localleader-key old))
      (eon-localleader--set-key 'eon-localleader-key value))))

(defcustom eon-leader-key
  (if (display-graphic-p) "C-," "C-z")
  "Leader prefix (GUI -> \"C-,\"; TTY -> \"C-z\"). Use `setopt' to change."
  :group 'eon
  :type 'string
  :set #'eon-leader--set-key)

;; Localleader implementation

(defvar-keymap eon-localleader-global-map
  :doc "Global localleader map (fallback for all buffers)."
  "/" '("..." . execute-extended-command-for-buffer))

(defvar-local eon-localleader--map eon-localleader-global-map
  "Active localleader keymap for the current buffer.")

(defun eon-localleader--context-window ()
  "Return the window where the key sequence started."
  (cond
   ((and (boundp 'which-key--original-window)
         (window-live-p which-key--original-window))
    which-key--original-window)
   (t (selected-window))))

(defun eon-localleader--effective-map (&optional _)
  "Return the localleader map for the current context buffer."
  (let* ((win (eon-localleader--context-window))
         (buf (and (window-live-p win) (window-buffer win))))
    (with-current-buffer (or buf (current-buffer))
      (if (keymapp eon-localleader--map)
          eon-localleader--map
        eon-localleader-global-map))))

(defun eon-localleader--set-key (sym value)
  "Setter for `eon-localleader-key'; rebinds the leader entry."
  (let ((old (and (boundp sym) (symbol-value sym))))
    (when (boundp 'ctl-z-map)
      (when old (keymap-unset ctl-z-map old))
      (keymap-set
       ctl-z-map value
       ;; No label; :filter supplies the proper (buffer-local) map.
       '(menu-item "" nil :filter eon-localleader--effective-map))))
  (set-default sym value))

(defcustom eon-localleader-key
  (if (display-graphic-p) "C-," "C-z")
  "Localleader key (pressed after the leader).
GUI: leader then \"C-,\" -> localleader (\"C-, C-,\")
TTY: leader then \"C-z\" -> localleader (\"C-z C-z\")
Use `setopt' to override."
  :group 'eon
  :type 'string
  :set #'eon-localleader--set-key)

;; Sub-keymaps under the leader:
(defvar-keymap ctl-z-b-map   :doc "Buffer")
(defvar-keymap ctl-z-c-map   :doc "Code")
(defvar-keymap ctl-z-e-map   :doc "Exec")
(defvar-keymap ctl-z-f-map   :doc "File")
(defvar-keymap ctl-z-g-map   :doc "Goto")
(defvar-keymap ctl-z-h-map   :doc "Help")
(defvar-keymap ctl-z-o-map   :doc "Org")
(defvar-keymap ctl-z-p-map   :doc "Project")
(defvar-keymap ctl-z-q-map   :doc "Quit")
(defvar-keymap ctl-z-s-map   :doc "Search")
(defvar-keymap ctl-z-t-map   :doc "Tab/WS")
(defvar-keymap ctl-z-v-map   :doc "VC/Git")
(defvar-keymap ctl-z-w-map   :doc "Window")
(defvar-keymap ctl-z-x-map   :doc "Misc")
(defvar-keymap ctl-z-ret-map :doc "Bookmark")

;; Top-level leader keymap:
(defvar-keymap ctl-z-map
  :doc "Leader (top-level) keymap."
  "b"   `("Buffer"   . ,ctl-z-b-map)
  "c"   `("Code"     . ,ctl-z-c-map)
  "e"   `("Exec"     . ,ctl-z-e-map)
  "f"   `("File"     . ,ctl-z-f-map)
  "g"   `("Goto"     . ,ctl-z-g-map)
  "h"   `("Help"     . ,ctl-z-h-map)
  "m"   #'execute-extended-command
  "o"   `("Org"      . ,ctl-z-o-map)
  "p"   `("Project"  . ,ctl-z-p-map)
  "q"   `("Quit"     . ,ctl-z-q-map)
  "s"   `("Search"   . ,ctl-z-s-map)
  "t"   `("Tab/WS"   . ,ctl-z-t-map)
  "v"   `("VC/Git"   . ,ctl-z-v-map)
  "w"   `("Window"   . ,ctl-z-w-map)
  "x"   `("Misc"     . ,ctl-z-x-map)
  "RET" `("Bookmark" . ,ctl-z-ret-map)
  ;; Add dynamic localleader keymap
  eon-localleader-key
  '(menu-item "" nil :filter eon-localleader--effective-map))

;; Initial binding of the leader prefix
(keymap-global-set eon-leader-key ctl-z-map)

;; Make the leader available in the minibuffer
;; If there's a problem with that, please open an issue on Github:
;; <https://github.com/monkeyjunglejuice/emacs.onboard/issues>
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (when (keymapp (current-local-map))
              (keymap-set (current-local-map) eon-leader-key ctl-z-map))))

(defmacro eon-localleader-defkeymap (mode map-sym &rest body)
  "Define MAP-SYM for MODE; inherit global localleader and activate it.
BODY is forwarded to `defvar-keymap'."
  (declare (indent 2))
  (let ((hook (intern (format "%s-hook" mode))))
    `(progn
       (defvar-keymap ,map-sym ,@body)
       ;; Inherit global entries so globals are always available.
       (set-keymap-parent ,map-sym eon-localleader-global-map)
       ;; Activate buffer-locally in this mode.
       (add-hook ',hook (lambda () (setq-local eon-localleader--map ,map-sym)))
       ;; If we're already in MODE (or derived), select it now.
       (when (derived-mode-p ',mode)
         (setq-local eon-localleader--map ,map-sym)))))

;; _____________________________________________________________________________
;;; KEYBINDING RELATED SETTINGS

;; Which-key: show a menu with available keybindings
(when (fboundp #'which-key-mode)
  (setopt which-key-lighter ""
          which-key-separator " "
          which-key-idle-delay 0.3
          which-key-idle-secondary-delay 0.0
          which-key-sort-uppercase-first nil
          which-key-sort-order 'which-key-key-order-alpha
          which-key-allow-evil-operators t
          which-key-preserve-window-configuration t
          which-key-show-remaining-keys t
          which-key-show-transient-maps t)
  (which-key-mode 1))

(when (eon-macp)
  (setopt
   ;; Make the <Command> key on MacOS act as <Ctrl> key: "C- ..."
   mac-command-modifier 'control
   ;; Make the <Option> key on MacOS act as <Meta> key for "M- ..."
   mac-option-modifier 'meta
   ;; Don't bypass "C-h ..." keybindings
   mac-pass-command-to-system nil))

;; _____________________________________________________________________________
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
                      :height 0.8)
  ;; Set the fonts for the inactive mode line
  (set-face-attribute 'mode-line-inactive nil
                      ;; :family "Iosevka Curly"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 0.8))

;; _____________________________________________________________________________
;;; TOGGLE THEME

;; Default/fallback definitions – don't change them here,
;; but scroll further down to 'THEME CONFIG'

(defcustom eon-theme-name-light 'modus-operandi-tinted
  "Name of the light theme."
  :group 'eon
  :type 'symbol)

(defcustom eon-theme-name-dark 'modus-vivendi-tinted
  "Name of the dark theme."
  :group 'eon
  :type 'symbol)

(defcustom eon-theme-variant-default 'light
  "Load either the light or the dark theme at startup?"
  :group 'eon
  :type 'symbol)

(defvar eon-theme-variant-active nil
  "Holds the information about the currently active theme variant.")

(defcustom eon-theme-light-pre-load-hook nil
  "Run before loading the light theme."
  :group 'eon
  :type 'hook)

(defcustom eon-theme-light-post-load-hook nil
  "Run after loading the light theme."
  :group 'eon
  :type 'hook)

(defcustom eon-theme-dark-post-load-hook nil
  "Run before loading the dark theme."
  :group 'eon
  :type 'hook)

(defcustom eon-theme-dark-post-load-hook nil
  "Run after loading the dark theme."
  :group 'eon
  :type 'hook)

(defun eon-theme-load-light ()
  "Load the light theme and apply some modifications.
Some themes may come as functions -- wrap these ones in lambdas."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (run-hooks 'eon-theme-light-pre-load-hook)
  (cond ((symbolp eon-theme-name-light)
         (load-theme eon-theme-name-light t))
        ((functionp eon-theme-name-light)
         (funcall eon-theme-name-light)))
  (setq eon-theme-variant-active 'light)
  (run-hooks 'eon-theme-light-post-load-hook))

(defun eon-theme-load-dark ()
  "Load the dark theme and apply some modifications.
Some themes may come as functions -- wrap these ones in lambdas."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (run-hooks 'eon-theme-dark-post-load-hook)
  (cond ((symbolp eon-theme-name-dark)
         (load-theme eon-theme-name-dark t))
        ((functionp eon-theme-name-dark)
         (funcall eon-theme-name-dark)))
  (setq eon-theme-variant-active 'dark)
  (run-hooks 'eon-theme-dark-post-load-hook))

(defun eon-theme-toggle ()
  "Toggle between light and dark theme."
  (interactive)
  (cond
   ((equal eon-theme-variant-active 'light) (eon-theme-load-dark))
   ((equal eon-theme-variant-active 'dark) (eon-theme-load-light))
   (t (mapc #'disable-theme custom-enabled-themes))))

(defun eon-theme-load-default ()
  "Load the default theme."
  (cond
   ((equal eon-theme-variant-default 'light) (eon-theme-load-light))
   ((equal eon-theme-variant-default 'dark) (eon-theme-load-dark))
   (t (error
       "Toggle theme: DEFAULT-THEME-VARIANT must be either 'light or 'dark"))))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; THEME CONFIG
;; Either configure the themes here,
;; or "M-x customize-group RET eon-theme-toggle"

;; Set the keybinding to toggle between light and dark: "<leader> x t"
(keymap-set ctl-z-x-map "t" #'eon-theme-toggle)

;; Set some defaults for the Modus themes; doesn't affect other themes.
;; These variables must be set before loading the Modus themes.
(setopt modus-themes-bold-constructs t
        modus-themes-italic-constructs nil
        modus-themes-mixed-fonts t)
(setopt modus-themes-common-palette-overrides
        '((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)))

;; Do not extend `region' background past the end of the line?
(custom-set-faces
 '(region ((t :extend nil))))

;;; ---> Set your light theme:
;; (setopt eon-theme-name-light 'modus-operandi)

;;; ---> Set your dark theme:
;; (setopt eon-theme-name-dark 'modus-vivendi)

;;; ---> Set your default variant here - 'light or 'dark
;; (setopt eon-theme-variant-default 'dark)

;; The hooks below can be used to run additional functions before or after
;; loading the selected light and dark theme. That's useful for setting
;; variables that otherwise would get overwritten by themes.
;; Restart Emacs to take effect after changing the hooks.

;;; Call a function before/after loading the light theme
;; Example for commands ("interactive" functions):
;; (add-hook 'eon-theme-light-post-load-hook #'my-interactive-function)
;; Normal functions not designated as "(interactive)" must be wrapped in lambdas:
;; (add-hook 'eon-theme-light-post-load-hook
;;           (lambda ()
;;             ;; Your arbitrary non-interactive function(s) here
;;             ))

;; Load the default font set; if you want to load a different font set,
;; "unhook" `eon-fonts-default' first via:
;; (remove-hook 'eon-theme-dark-post-load-hook #'eon-fonts-default)
(add-hook 'eon-theme-light-post-load-hook #'eon-fonts-default)

;;; Call a function before/after loading the dark theme
;; Example for commands ("interactive" functions):
;; (add-hook 'eon-theme-dark-post-load-hook #'my-interactive-function)
;; Normal functions not designated as "(interactive)" must be wrapped in lambdas:
;; (add-hook 'eon-theme-dark-post-load-hook
;;           (lambda ()
;;             ;; Your arbitrary non-interactive function(s) here
;;             ))

;; Load the default font set; if you want to load a different font set,
;; "unhook" `eon-fonts-default' first via:
;; (remove-hook 'eon-theme-dark-post-load-hook #'eon-fonts-default)
(add-hook 'eon-theme-dark-post-load-hook #'eon-fonts-default)

;; Load the theme
(eon-theme-load-default)

;; _____________________________________________________________________________
;;; DISPLAY & SCROLLING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Scrolling>

(setopt mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed t
        mouse-wheel-follow-mouse t)

(setopt scroll-preserve-screen-position t
        scroll-margin 1
        scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01)

;; Horizontal scrolling
(setopt auto-window-vscroll nil
        hscroll-margin 1
        hscroll-step 1)

;; Enable pixel-based scrolling
(when (fboundp #'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; Redraw the display – useful when running Emacs in a Windows terminal emulator
;; and the display shows garbage. Accessible via "<leader> x r"
(keymap-set ctl-z-x-map "r" #'redraw-display)

;; _____________________________________________________________________________
;;; MODE LINE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Mode-Line>

;; Compress the mode line?
;; If non-nil, repeating spaces are compressed into a single space.
;; If 'long, this is only done when the mode line is longer than
;; the current window width (in columns).
(setopt mode-line-compact nil)

;; Show the buffer size in the modeline?
(size-indication-mode 1)

;; Show the current line number along with column number in mode line?
(column-number-mode 1)

;; _____________________________________________________________________________
;;; MINIBUFFER
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Minibuffer>

;; Recursive minibuffers
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Recursive-Edit>
;; Allow minibuffer commands while in the minibuffer
;; There are two commands to get out of recursive minibuffers:
;; "C-z-c" exit-recursive-edit and "C-]" abort-recursive-edit
(setopt enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '( read-only t
         cursor-intangible t
         face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; For mouse commands to ask questions, use dialog box instead of minibuffer?
(setopt use-dialog-box nil)

;; Grow and shrink the minibuffer according to its content?
(setopt resize-mini-windows 'grow-only)

;; Allow for shorter responses? "y" for "yes" and "n" for "no"
(setopt read-answer-short t)
(setopt use-short-answers t)
(advice-add 'yes-or-no-p :override #'y-or-n-p)

;; _____________________________________________________________________________
;;; COMPLETION
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Icomplete>

;; There are many matching styles available, see `completion-styles-alist'
;; <https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html>
;; The order within the list determines their priority.
(setopt completion-styles '(basic substring initials flex partial-completion))
(setopt completion-category-defaults nil)
(setopt completion-category-overrides
        '((file (styles . (basic partial-completion initials substring)))))

;; Prevent *Completions* buffer from popping up?
(setopt completion-auto-help nil)
;; Cycle completion candidates instead
(setopt completion-cycle-threshold t)
;; Show docstrings for completion candidates?
(setopt completions-detailed nil)

;; Preview current in-buffer completion candidate?
(global-completion-preview-mode 1)
(keymap-set completion-preview-active-mode-map "M-n"
            #'completion-preview-next-candidate)
(keymap-set completion-preview-active-mode-map "M-p"
            #'completion-preview-prev-candidate)

;; Tweaking Icomplete
(with-eval-after-load 'icomplete
  (setopt icomplete-in-buffer t
          icomplete-compute-delay 0
          icomplete-delay-completions-threshold 100
          icomplete-show-matches-on-no-input t
          icomplete-hide-common-prefix nil)
  ;; TAB accepts the current candidate in Fido minibuffers
  (keymap-set icomplete-minibuffer-map "TAB" #'icomplete-force-complete)
  (keymap-set icomplete-minibuffer-map "<tab>" #'icomplete-force-complete))

;; Vertical minibuffer completion with `fido-vertical'
(fido-vertical-mode 1)

;; Dabbrev
(with-eval-after-load 'dabbrev
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

;; _____________________________________________________________________________
;;; HELP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Help>

(keymap-set ctl-z-h-map "/" `("..." . ,help-map))
(keymap-set ctl-z-h-map "e" #'view-echo-area-messages)
(keymap-set ctl-z-h-map "f" #'describe-function)
(keymap-set ctl-z-h-map "k" #'describe-key-briefly)
(keymap-set ctl-z-h-map "o" #'describe-symbol)
(keymap-set ctl-z-h-map "v" #'describe-variable)

;; Show all options when running 'apropos' "C-h a" (fulltext search)
(setopt apropos-do-all t)

;; _____________________________________________________________________________
;;; PACKAGE MANAGER UI

(when package-enable-at-startup

  ;; Open the package manager interface: "<leader> x p"
  (keymap-set ctl-z-x-map "p" #'list-packages)

  ;; Highlight current line in the package manager
  (add-hook 'package-menu-mode-hook
            (lambda ()
              (hl-line-mode 1))))

;; _____________________________________________________________________________
;;; CUSTOMIZATION UI

;; Don't accumulate customization buffers
(setopt custom-buffer-done-kill t)

(defun eon-customize-group ()
  "Set preferences via GUI."
  (interactive)
  (customize-group 'eon))

(keymap-set ctl-z-x-map "C" #'eon-customize-group)

;; _____________________________________________________________________________
;;; ELDOC
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Lisp-Doc>
;; <https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc>

(setopt eldoc-minor-mode-string nil
        eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
        eldoc-echo-area-display-truncation-message nil
        eldoc-echo-area-prefer-doc-buffer nil
        eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)

(keymap-set ctl-z-c-map "d" #'eldoc)

;; _____________________________________________________________________________
;;; SEARCH
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Search>

;; Switch search functions to make regex-search the default
(keymap-global-set "C-s"   #'isearch-forward-regexp)
(keymap-global-set "C-r"   #'isearch-backward-regexp)
(keymap-global-set "C-S-s" #'isearch-forward)
(keymap-global-set "C-S-r" #'isearch-backward)

;; Search and replace
;; The 'query-' variant asks for each string. Confirm with "SPC",
;; or jump to the next via "n"
(keymap-global-set "M-%"   #'query-replace-regexp)
(keymap-global-set "C-M-%" #'replace-regexp)

;; _____________________________________________________________________________
;;; PINENTRY

(setopt epg-pinentry-mode 'loopback)

;; _____________________________________________________________________________
;;; WINDOW MANAGEMENT
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Windows>
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Window-Convenience>

;; Should `display-buffer' try to even window sizes?
;; Set to nil to leave the window configuration alone.
(setopt even-window-sizes 'height-only)

;; Focus follows mouse?
(setopt mouse-autoselect-window nil
        focus-follows-mouse nil)

;; Default window navigation – simply switch to the next window in order.
;; Added for convenience; the default keybinding is "C-x o"
(keymap-global-set "M-o" #'other-window)

;; Undo/redo window layouts
(setopt winner-dont-bind-my-keys t)
(keymap-set ctl-z-w-map "u" #'winner-undo)
(keymap-set ctl-z-w-map "r" #'winner-redo)
(winner-mode 1)

;; _____________________________________________________________________________
;;; TAB MANAGEMENT

;; Create new tab
(keymap-set ctl-z-t-map "t" #'tab-new-to)

;; Close tab
(keymap-set ctl-z-t-map "c" #'tab-close)
(keymap-set ctl-z-t-map "C" #'tab-close-other)

;; Fast tab switching
(keymap-set ctl-z-t-map "p" #'tab-previous)
(keymap-set ctl-z-t-map "n" #'tab-next)

;; _____________________________________________________________________________
;;; BUFFER MANAGEMENT
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Buffers>

;; Fast buffer switching
(keymap-set ctl-z-b-map "p" #'previous-buffer)
(keymap-set ctl-z-b-map "n" #'next-buffer)

(keymap-set ctl-z-b-map "b" #'switch-to-buffer)

;; Kill the current buffer immediately instead of presenting a selection
;; It's the equivalent to "close tab" in a web browser or other editors
(keymap-set ctl-z-map "k" #'kill-current-buffer)

;; Kill the window too
(keymap-set ctl-z-map "K" #'kill-buffer-and-window)

;; Get the buffer out of the way, but let it alive
(keymap-set ctl-z-b-map "k" #'bury-buffer)

;; Kill all buffers at once – equivalent to "close all tabs"
(defun eon-kill-all-buffers ()
  "Close all buffers at once."
  (interactive)
  (save-some-buffers)
  (let ((kill-buffer-query-functions '()))
    (mapc #'kill-buffer (buffer-list))))

(keymap-set ctl-z-b-map "K" #'eon-kill-all-buffers)

;; Uniquify buffer names for buffers that would have identical names
(setopt uniquify-buffer-name-style 'forward)

;; Define boring buffers globally, so they can be hidden.
;; These buffers remain accessible via Ibuffer "C-x C-b".
(defcustom eon-boring-buffers
  '("\\` "
    "\\`\\*Echo Area"
    "\\`\\*Minibuf"
    "\\`\\*Completions"
    "\\`\\*Flymake log"
    "\\`\\*Semantic SymRef"
    "\\`\\*Backtrace"
    "\\`\\*Async-native-compile-log"
    "\\`\\*tramp"
    "\\`\\*EGLOT"
    ;; And some hidden buffers can be visited by ...
    "\\`\\*Bookmark List"  ; "C-x r l"
    "\\`\\*Ibuffer"        ; "<leader> b i"
    "\\`\\*Messages"       ; "<leader> h e"
    )
  "List of regexps matching buffers considered uninteresting.
These buffers may be skipped from navigation commands.
Use `eon-boring-buffers-add' to extend the list."
  :type '(repeat regexp)
  :group 'eon)

(defun eon-boring-buffers-add (&optional regexp)
  "Add REGEXP (string or list of strings) to `eon-boring-buffers'.
Called without argument just syncs `eon-boring-buffers' to other places."
  (when regexp
    (eon-add-to-list 'eon-boring-buffers regexp))
  ;; Define other places where `eon-boring-buffers' are synced to:
  (setopt switch-to-prev-buffer-skip-regexp eon-boring-buffers
          switch-to-next-buffer-skip-regexp eon-boring-buffers))

;; Hide boring buffers from `next-buffer' and `prev-buffer'.
(with-eval-after-load 'window (eon-boring-buffers-add))

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; IBUFFER – the buffer manager
;; <https://protesilaos.com/codelog/2020-04-02-emacs-intro-ibuffer/>

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)))

;; Hide the boring buffers from Ibuffer too?
;; (setopt ibuffer-never-show-predicates eon-boring-buffers)

(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-set ctl-z-b-map "i" #'ibuffer)

;; _____________________________________________________________________________
;;; SCRATCH BUFFER

;; Set an initial major mode for the *scratch* buffer:

;; Lisp-interaction-mode is the default mode for the scratch buffer
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Lisp-Interaction>
;; (setopt initial-major-mode #'lisp-interaction-mode)

;; You can set the scratch buffer to Org-mode which may be more useful
;; for quick notes, writing and literate programming
;; (setopt initial-major-mode #'org-mode)

;; Should the *scratch* buffer contain some initial content?
(setopt initial-scratch-message "")

;; Quickly jump to the *scratch* buffer
(keymap-set ctl-z-map "z" #'scratch-buffer)

;; _____________________________________________________________________________
;;; CLIPBOARD, COPY & PASTE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Killing>

;; Prevent duplicates to avoid cluttering the kill ring
(setopt kill-do-not-save-duplicates t)

(setopt
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
(defun eon-yank-from-kill-ring ()
  "Select and insert an item from the 'kill-ring'."
  (interactive)
  (insert (completing-read "Yank: " kill-ring nil t)))
(keymap-global-set "M-y" #'eon-yank-from-kill-ring)

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
  (keymap-set ctl-z-map "C-w" #'eon-wsl-copy))

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
  (keymap-set ctl-z-map "C-y" #'eon-wsl-paste))

;; _____________________________________________________________________________
;;; HISTORY

;; Which histories to save between Emacs sessions?
(savehist-mode 1)
(eon-add-to-list 'savehist-additional-variables
                 '(kill-ring
                   register-alist
                   mark-ring
                   global-mark-ring
                   search-ring
                   regexp-search-ring))

;; History length for various histories
(setopt history-length 500)
;; Delete duplicates from the command history?
(setopt history-delete-duplicates t)

;; Remember where the cursor was, the last time you visited the file?
(setopt save-place-limit 500)
(save-place-mode 1)

;; _____________________________________________________________________________
;;; FILE MANAGEMENT

;; Open arbitrary file: "<leader> f f"
(keymap-set ctl-z-f-map "f" #'find-file)
;; Open another file that has common file base name: "<leader> f a"
(keymap-set ctl-z-f-map "a" #'find-alternate-file-other-window)
;; Open the file you really wanted: "<leader> f A"
(keymap-set ctl-z-f-map "A" #'find-alternate-file)

;; Open any resource under the cursor: "<leader> f o"
(keymap-set ctl-z-f-map "o" #'find-file-at-point)
;; Display a list of all resources mentioned in this buffer: "<leader> f O"
(keymap-set ctl-z-f-map "O" #'ffap-menu)

;; Save buffer if modified: "<leader> f s"
(keymap-set ctl-z-f-map "s" #'save-buffer)
;; Save some modified file-visiting buffers, but ask: "<leader> f S"
(keymap-set ctl-z-f-map "S" #'save-some-buffers)
;; Write buffer to file ("save file as ..."): "<leader> f w"
(keymap-set ctl-z-f-map "w" #'write-file)

;; Deleting files

(defun eon-trash-on ()
  "Delete files by moving to the system trash."
  (interactive)
  (setopt delete-by-moving-to-trash t)
  (setopt dired-recursive-deletes 'always)  ; don't ask when directory not empty
  (message "Trash on: Deleted files will go to system trash."))

(defun eon-trash-off ()
  "Delete files immediately."
  (interactive)
  (setopt delete-by-moving-to-trash nil)
  (setopt dired-recursive-deletes 'top)  ; ask when directory not empty
  (message "Trash off: Files will be deleted immediately!"))

(eon-trash-on)  ; set the default

;; Use the system trash when deleting files?
(setopt remote-file-name-inhibit-delete-by-moving-to-trash t)

;; Resolve symlinks so that operations are conducted from the file's directory?
(setopt find-file-visit-truename t
        vc-follow-symlinks t)

;; Auto refresh dired (and others) when contents change?
(setopt global-auto-revert-non-file-buffers t
        auto-revert-stop-on-user-input nil
        auto-revert-verbose t)
(global-auto-revert-mode 1)

;; Configure Ediff to use a single frame and split windows horizontally
(setopt ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)

;; _____________________________________________________________________________
;;; RECENT FILES

;; Turn on recent file mode to visit recently edited files
(recentf-mode 1)

(setopt recentf-max-menu-items 10
        recentf-max-saved-items 100)

;; Ignore some recently visited files, eg. to prevent them from showing up
;; amongst recent files after package upgrades
(add-to-list 'recentf-exclude
             (expand-file-name (concat user-emacs-directory "elpa/")))
(add-to-list 'recentf-exclude
             "^/\\(?:ssh\\|su\\|sudo\\)?:")

(keymap-global-set "C-x f" #'recentf-open)
(keymap-set ctl-z-f-map "r" #'recentf-open)

;; _____________________________________________________________________________
;;; BACKUP FILES
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Backup>

;; CAUTION: This mode makes copies of the files you are editing.
;; If you're editing files with sensitive data (e.g. on temporally mounted,
;; encrypted devices), either disable this mode or specify the location
;; where to save (or not to save) backup copies of these files.

;; Make backup before editing?
(setopt make-backup-files t)

;; Where to save the backups?
;; Specify file name/path patterns and directories ("REGEXP" . "DIRECTORY").
;; Files with sensitive content can be specified (excluded) here, too.
(setopt backup-directory-alist
        `(("." . ,(concat user-emacs-directory "backup/"))))

;; Apply the same backup policy for Tramp files on their hosts
;; like the policy for local files
(setopt tramp-backup-directory-alist backup-directory-alist)

;; Backup settings
(setopt backup-by-copying t
        backup-by-copying-when-linked t
        kept-new-versions 5
        kept-old-versions 5
        delete-old-versions t
        version-control t
        vc-make-backup-files t)

;; _____________________________________________________________________________
;;; LOCKFILES
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Interlocking>

;; Let Emacs keep track of files currently visited?
(setopt create-lockfiles nil)

;; _____________________________________________________________________________
;;; AUTO-SAVE FILES
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Auto-Save>

;; Enable auto-save to safeguard against data loss?
;; The `recover-file' or `recover-session' functions can be used
;; to restore auto-saved data
(setopt auto-save-default t)
(setopt auto-save-no-message t)

;; Do not auto-disable auto-save after deleting large chunks of text
(setopt auto-save-include-big-deletions t)

;; Auto-save locations
(setopt auto-save-list-file-prefix
        (expand-file-name "autosave/" user-emacs-directory))
(setopt tramp-auto-save-directory
        (expand-file-name "autosave-tramp/" user-emacs-directory))

;; Auto save options
(setopt kill-buffer-delete-auto-save-files t)

;; _____________________________________________________________________________
;;; DIRED FILE MANAGER
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Dired>

;; The `dired' keybinding is "C-x d". This new keybinding is in accordance
;; with "C-x C-f" for visiting files
(keymap-global-set "C-x C-d" #'dired)

;; Open the Dired file manager from the leader menu: "<leader> d"
(keymap-set ctl-z-map "d" #'dired)
;; Open the directory of the currently visited file in Dired: "<leader> f d"
(keymap-set ctl-z-f-map "d" #'dired-jump)

;; Switch to wdired-mode and edit directory content like a text buffer
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "e" #'wdired-change-to-wdired-mode))

;; Don't accumulate useless Dired buffers
(setopt dired-kill-when-opening-new-dired-buffer t)

;; Improving directory listings
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Hide details in file listings? Toggle via "S-("
            (dired-hide-details-mode 1)
            ;; Highlight current line?
            (hl-line-mode 1)))

;; Listing columns; Switch arguments with "C-u s" e.g. hide backups with -B
(setopt dired-listing-switches "-lhFA -v --group-directories-first")

;; Copying files/directories with sub-directories?
(setopt dired-recursive-copies 'always)

;; Create directories if they don't exist?
(setopt dired-create-destination-dirs 'ask)

;; Register file renaming in underlying version control system?
(setopt dired-vc-rename-file t)

;; Mimic dual-pane file managers?
(setopt dired-dwim-target t)

;; Check for directory modifications?
(setopt dired-auto-revert-buffer 'dired-buffer-stale-p)

;; Images
(setopt image-dired-thumb-margin 1
        image-dired-thumb-relief 0
        ;; Store thumbnails in the system-wide thumbnail location
        ;; e.g. ~/.local/cache/thumbnails to make them reusable by other programs
        image-dired-thumbnail-storage 'standard-large)

;; Linux/Unix only: hit "M-RET" to open files in the corresponding desktop app
(when (eon-linp)
  (defun eon-dired-xdg-open ()
    "Open files and folders with the default desktop app."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "xdg-open" nil 0 nil file)
      (message "Opening %s done" file)))
  (with-eval-after-load 'dired
    (keymap-global-set dired-mode-map "M-RET" #'eon-dired-xdg-open)))

;; _____________________________________________________________________________
;;; BOOKMARKS

;; Bind common bookmark commands to the leader menu
(keymap-set ctl-z-ret-map "RET" #'bookmark-jump)
(keymap-set ctl-z-ret-map "d"   #'bookmark-delete)
(keymap-set ctl-z-ret-map "l"   #'list-bookmarks)
(keymap-set ctl-z-ret-map "m"   #'bookmark-set)
(keymap-set ctl-z-ret-map "M"   #'bookmark-set-no-overwrite)
(keymap-set ctl-z-ret-map "r"   #'bookmark-rename)

;; _____________________________________________________________________________
;;; COMINT

(setopt comint-input-ignoredups t
        comint-prompt-read-only t
        comint-buffer-maximum-size 2048
        comint-scroll-to-bottom-on-input 'this)

;; _____________________________________________________________________________
;;; ESHELL
;; <https://www.gnu.org/software/emacs/manual/html_mono/eshell.html>

;; Eshell is not a terminal emulator, but a shell equivalent to Bash or Fish
;; that runs within Emacs. It is independent from the OS. Eshell looks like
;; a Posix shell superficially, but is also a REPL for Emacs Lisp expressions.

;; Get rid of the Eshell startup message?
(setopt eshell-banner-message "")

;; List directory content after changing into it?
(setopt eshell-list-files-after-cd t)

(defun eon-eshell-new ()
  "Open a new eshell instance."
  (interactive)
  (eshell 't))

;; Launch an Eshell buffer: "<leader> e e"; re-visit the buffer by repeating
(keymap-set ctl-z-e-map "e" #'eshell)
;; Launch a fresh Eshell buffer: "<leader> e E"
(keymap-set ctl-z-e-map "E" #'eon-eshell-new)

;; _____________________________________________________________________________
;:;; SHELL
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Shell-Mode>

;; This is also no terminal emulator, but a buffer to issue shell commands
;; and display their output

;; Set another shell than your default one?
;; (setopt shell-file-name "/usr/bin/bash")

;; Make `shell' behave like `eshell'
(setopt shell-kill-buffer-on-exit t)

(defun eon-shell-new ()
  "Open a new shell instance."
  (interactive)
  (shell (generate-new-buffer-name "*shell*")))

;; Launch a Shell buffer: "<leader> e s"; re-visit the buffer by repeating
(keymap-set ctl-z-e-map "s" #'shell)
;; Launch a fresh Shell buffer: "<leader> e S"
(keymap-set ctl-z-e-map "S" #'eon-shell-new)

;; _____________________________________________________________________________
;;; PROJECT MANAGEMENT
;; Setup for Emacs' built-in project management

;; Open file in current project
(keymap-global-set "C-x f" #'project-find-file)
;; Open directory in current project
(keymap-global-set "C-x d" #'project-find-dir)

;; Switch to current project buffers: "<leader> n"
(keymap-set ctl-z-map "n" #'project-switch-to-buffer)
;; "<leader> p" inherits all commands from the `project-prefix-map'
(set-keymap-parent ctl-z-p-map project-prefix-map)

;; Show all project keybindings in the selection?
(setopt project-switch-use-entire-map nil)
;; Show these insteads:
(setopt project-switch-commands '((project-find-file   "File"   ?f)
                                  (project-find-dir    "Dired"  ?d)
                                  (project-find-regexp "Grep"   ?g)
                                  (project-vc-dir      "VC/Git" ?v)
                                  (project-eshell      "Eshell" ?e)
                                  (project-shell       "Shell"  ?s)))

;; _____________________________________________________________________________
;;; PROCED

;; Show and manage OS processes, like the command line programs top and htop

(setopt proced-auto-update-interval 1)

(setopt proced-auto-update-flag t
        proced-descend t)

;; _____________________________________________________________________________
;;; NET-UTILS

(setopt netstat-program "netstat"
        netstat-program-options '("-atupe"))

;; _____________________________________________________________________________
;;; WEB BROWSERS

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;; EWW BUILT-IN BROWSER
;; <https://www.gnu.org/software/emacs/manual/html_mono/eww.html#Top>

(setopt url-privacy-level '(email lastloc cookies))
(url-setup-privacy-info)

(defun eon-user-agent (browser-name)
  "Accepts a symbol in order to return a pre-defined user-agent string.
BROWSER-NAME can be either `safari-macos', `safari-iphone', `w3m' or t,
which sets the default `eww' user-agent according to `url-privacy-level'."
  (cond
   ((equal browser-name 'safari-macos)
    (setopt url-user-agent
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/603.3.8 (KHTML, like Gecko) Version/11.0.1 Safari/603.3.8"))
   ((equal browser-name 'safari-iphone)
    (setopt url-user-agent
            "Mozilla/5.0 (iPhone; CPU iPhone OS 18_2 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.2 Mobile/15E148 Safari/604.1"))
   ((equal browser-name 'w3m)
    (setopt url-user-agent
            "w3m/0.5.3+git2020050"))
   (t
    (setopt url-user-agent
            'default)))

  ;; Set the user agent for the internal web browser
  (eon-user-agent 'safari-iphone)

  ;; Per default, open links with the internal web browser
  (setopt browse-url-browser-function #'eww-browse-url)

  ;; Secondary web browser
  (setopt browse-url-secondary-browser-function #'browse-url-default-browser))
;; (setopt browse-url-browser-function #'browse-url-firefox)
;; (setopt browse-url-generic-program (executable-find "nyxt")
;;         browse-url-browser-function #'browse-url-generic)

;; Keybindings
(keymap-set ctl-z-g-map "W" #'browse-url)
(keymap-set ctl-z-g-map "w" #'browse-web)

;; _____________________________________________________________________________
;;; EMAIL
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Sending-Mail>

;; TODO: Send emails directly from Emacs using SMTP – example template

;; Should be defined first
;; (setopt user-mail-address "mail@example.org")

;; To avoid typing in the password for each email, specify SMTP account(s)
;; in '~/.authinfo.gpg'. Here's a content template for authinfo.gpg:
;; machine mail.example.org port 587 login myuser password mypassword

;; Emacs email variables
(with-eval-after-load 'smtpmail
  (setopt send-mail-function #'smtpmail-send-it
          smtpmail-smtp-server "localhost"
          smtpmail-stream-type 'starttls
          smtpmail-smtp-service 1025      ; default port: 587
          smtpmail-queue-dir "~/.mail/queued-mail/"
          smtpmail-smtp-user user-mail-address
          smtpmail-debug-info nil))

(setopt message-kill-buffer-on-exit t)

;; _____________________________________________________________________________
;;; CALENDAR
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Calendar_002fDiary>

(setopt calendar-date-style 'iso
        calendar-week-start-day 1
        calendar-weekend-days '(6 0))

;; _____________________________________________________________________________
;;; GENERAL EDITING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Basic>

;; UTF-8
(prefer-coding-system 'utf-8)

;; Set desired line length in characters
(setopt fill-column 80)

;; While a text selection is active, typing characters replaces
;; the selection with the typed characters (default: -1 = off)
(delete-selection-mode -1)

;; Save always with a final new line?
(setopt require-final-newline t)

;; Better than the default 'just-one-space' (was M-SPC before)
(keymap-global-set "M-S-SPC" #'cycle-spacing)

;; Count lines, words and chars (buffer or region)
(keymap-global-set "C-x l" #'count-words)

;; Kill up to character
(keymap-global-set "M-z" #'zap-up-to-char)

;; Define keymap in order to group formatting commands
(defvar-keymap eon-format-map :doc "Format"
               "a" #'align
               "s" #'sort-lines)
;; Hook the keymap into the "Code" sub-keymap under the leader to make it
;; available via "<leader> c f"
(keymap-set ctl-z-c-map "F" `("Format" . ,eon-format-map))

;; _____________________________________________________________________________
;;; LINE NUMBERS
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Display-Custom>

;; Line numbers on or off? Toggle with "M-x display-line-numbers-mode" or
;; set it here for all programming modes. Goto line: "M-g M-g"
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)))

;; _____________________________________________________________________________
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

;; _____________________________________________________________________________
;;; FOLDING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Hideshow>

;; Code folding on or off? Show available commands: "M-x hs-"
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (hs-minor-mode 1)))

;; _____________________________________________________________________________
;;; INDENTATION
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Indentation>

(setopt indent-tabs-mode nil  ; don't use tabs but spaces
        tab-width 4)          ; set display width for tab characters

;; Delete the whole indentation instead spaces one-by-one via <backspace>?
;; (Possibly shadowed by 3rd-party packages like `smartparens-mode'
(setopt backward-delete-char-untabify-method 'hungry)

;; Enable indentation and completion using the TAB key
(setopt tab-always-indent 'complete)

;; Trigger automatic indentation by newline and DEL/backspace
(setopt electric-indent-chars '(?\n ?\^?))

;; _____________________________________________________________________________
;;; COMMENTS

;; Enable multi-line commenting to ensure that `comment-indent-new-line'
;; continues comments onto new lines?
(setopt comment-multi-line t)

;; Also comment empty lines between?
(setopt comment-empty-lines t)

;; _____________________________________________________________________________
;;; BRACKETS / PARENTHESIS
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Parentheses>

;; How to display matching parens generally?
(setopt show-paren-style 'parenthesis
        show-paren-delay 0.125
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)

;; Auto-close parens, brackets and quotes?
(electric-pair-mode 1)

;; _____________________________________________________________________________
;;; WHITESPACE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Useless-Whitespace>

;; Indicate trailing whitespace in programming modes?
(add-hook 'prog-mode-hook
          (lambda () (setopt show-trailing-whitespace nil)))

;; Indicate trailing whitespace in "text" modes?
(add-hook 'text-mode-hook
          (lambda () (setopt show-trailing-whitespace nil)))

;; Cleanup trailing whitespace
(keymap-set ctl-z-c-map "w" #'whitespace-cleanup)

;; _____________________________________________________________________________
;;; SYNTAX CHECK / LINTER
;; <https://www.gnu.org/software/emacs/manual/html_mono/flymake.html>

;; There are various syntax-checkers coming with the built-in Flymake mode,
;; and additional checkers can be installed as 3rd-party packages via
;; "M-x package-install <RET> flymake-".

;; Style the Flymake widget in the modeline
(setopt flymake-mode-line-format
        '(" " "FlyM" flymake-mode-line-exception flymake-mode-line-counters))

;; Stop when first/last error is reached
(setopt flymake-wrap-around nil)

(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-g E" #'flymake-show-project-diagnostics)
  (keymap-set flymake-mode-map "M-g e" #'flymake-show-buffer-diagnostics)
  (keymap-set flymake-mode-map "M-g n" #'flymake-goto-next-error)  ; default
  (keymap-set flymake-mode-map "M-g p" #'flymake-goto-prev-error))  ; default

;; _____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER (LSP)
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md/>

(with-eval-after-load 'eglot
  ;; Shutdown language server after closing last file?
  (setopt eglot-autoshutdown t)
  ;; Allow edits without confirmation?
  (setopt eglot-confirm-server-initiated-edits nil)
  ;; Show code action indicators?
  (setopt eglot-code-action-indications nil)
  ;; Common keybindings
  (keymap-set ctl-z-c-map "r"   #'eglot-rename)
  (keymap-set ctl-z-c-map "f"   #'eglot-format)
  (keymap-set ctl-z-c-map "F b" #'eglot-format-buffer)
  (keymap-set ctl-z-c-map "a"   #'eglot-code-actions))

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

;; _____________________________________________________________________________
;;; TREE-SITTER

;; Define grammar specs for ts-modes already built into Emacs.
;; Grammars can be built and installed via:
;; - `eon-treesitter-ensure-grammar' (declarative in your Elisp code)
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

- Only LANG and URL are mandatory.
- LANG is the language symbol.
- URL is the URL of the grammar’s Git repository or a directory
  where the repository has been cloned.
- REVISION is the Git tag or branch of the desired version,
  defaulting to the latest default branch.
- SOURCE-DIR is the relative subdirectory in the repository in which
  the grammar’s parser.c file resides, defaulting to \"src\".")

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; Internal utilities

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
  (setopt treesit-language-source-alist
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

;; Public API

(defun eon-treesitter-add-specs (&rest specs)
  "Add SPECS to `eon-treesitter-specs' and merge them into the source alist.
Use this only if you merely want to register SPECS, but not build/install
them.

- SPECS can be many specs or a single list of specs.
  Each spec has the form (LANG URL [REVISION] [SOURCE-DIR]).
- Only LANG and URL are mandatory.
- LANG is the language symbol.
- URL is the URL of the grammar’s Git repository or a directory where the
  repository has been cloned.
- REVISION is the Git tag or branch of the desired version, defaulting to
  the latest default branch.
- SOURCE-DIR is the relative subdirectory in the repository in which the
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

;; _____________________________________________________________________________
;;; COMPILING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Building>

;; Keep the compilation buffer in the background, except when there's an error
(add-to-list 'display-buffer-alist
             '("\\*.*compilation\\*" (display-buffer-no-window)))

;; Scroll to the first error
(setopt compilation-scroll-output 'first-error)

;; Recenter to the middle of the window for `compile-goto-error'
(setopt next-error-recenter '(4))

;; _____________________________________________________________________________
;;; TEXT MODES / WRITING PROSE

;; Visual line wrapping in text mode
(add-hook 'text-mode-hook #'visual-line-mode)

;; Sentences end with a single space
(setopt sentence-end-double-space nil)

;; TODO Add Flyspell / Ispell presets here

;; _____________________________________________________________________________
;;; ORG MODE
;; <https://orgmode.org/>
;; <https://orgmode.org/org.html>
;; Org provides functionality far beyond that of computational notebooks
;; such as Jupyter or R Markdown.

;; Create the local leader keymap
(eon-localleader-defkeymap org-mode eon-localleader-org-mode-map
  :doc "Localleader map for `org-mode'.")

;; Set a default location to look for Org files; but you can have
;; that directory anywhere you like
(setopt org-directory (expand-file-name "~/Documents/org/"))

(defun eon-visit-org-directory ()
  "Show the Org directory in Dired."
  (interactive)
  (dired org-directory))

;; Visit the `org-directory' in Dired via `C-z o d'
(keymap-set ctl-z-o-map "d" #'eon-visit-org-directory)

;; Turn on visual word wrapping
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Visual-Line-Mode>
(add-hook 'org-mode-hook #'visual-line-mode)

;; Alignment of tags at the end of headlines
(setopt  org-auto-align-tags t
         org-tags-column 0)

;; Toggle indicator for headlines
(setopt org-ellipsis " ▼ ")

;; Don't add leading indentation to code blocks, remove them during export
(setopt org-edit-src-content-indentation 0
        org-src-preserve-indentation nil)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ORG CAPTURE
;; <https://orgmode.org/org.html#Capture>

;; Capture a note via `C-z o c'
(keymap-set ctl-z-o-map "c" #'org-capture)
;; Put newer notes on top of the file
(setopt org-reverse-note-order t)

;; Set a default target for storing notes
(setopt org-default-notes-file (concat org-directory "notes.org"))

(defun eon-visit-org-notes ()
  "Visit the Org notes file."
  (interactive)
  (find-file org-default-notes-file))

;; Visit the default notes file via `C-z o o'
(keymap-set ctl-z-o-map "o" #'eon-visit-org-notes)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ORG TODO
;; <https://orgmode.org/org.html#TODO-Items>

;; Set some sensible default states for todo-items
(setopt org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ORG AGENDA
;; <https://orgmode.org/org.html#Agenda-Views>

(setopt org-agenda-files (list org-directory))

;; Visit your Org agenda via `C-z o a'
(keymap-set ctl-z-o-map "a" #'org-agenda)

(add-hook 'org-agenda-mode-hook
          (lambda ()
            ;; Highlight current line in Org agenda?
            (hl-line-mode 1)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ORG LINKS
;; <https://orgmode.org/org.html#Hyperlinks>

;; Store a link via "<localleader> L"
(keymap-set eon-localleader-org-mode-map "L" #'org-store-link)

;; Insert a link into an Org file via "<localleader> l"
(keymap-set eon-localleader-org-mode-map "l" #'org-insert-link)

;; Toggle visible/hidden links via "<localleader> M-l"
(keymap-set eon-localleader-org-mode-map "M-l" #'org-toggle-link-display)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ORG PUBLISH

;; Select a project to publish a project via "<localleader> p";
;; This can be used to generate and publish a static blog, ebooks, etc.
(keymap-set eon-localleader-org-mode-map "p" #'org-publish)

;; Speed up publishing by skipping files that haven't been changed
(setopt org-publish-list-skipped-files nil)

;; Where to place the directory containing the timestamps about changed files
(setopt org-publish-timestamp-directory
        (concat user-emacs-directory "org-timestamps/"))

(defun org-publish-unchanged-files-toggle ()
  "Toggle whether to re-export Org files that haven't been changed."
  (interactive)
  (if org-publish-use-timestamps-flag
      (progn (setopt org-publish-use-timestamps-flag nil)
             (message "Re-export unchanged files"))
    (progn (setopt org-publish-use-timestamps-flag t)
           (message "Don't re-export unchanged files (default)"))))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ORG EXPORT

;; HTML export
(setopt org-html-checkbox-type 'unicode
        org-html-prefer-user-labels t
        org-html-self-link-headlines t)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; LITERATE PROGRAMMING
;; <https://orgmode.org/worg/org-contrib/babel/intro.html>

;; Activate code blocks via Babel languages
(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))

;; In case you're using the Emacs ONTOP extensions, further languages
;; should not be configured here, but within their specific ONTOP modules
;; ("eon-*.el" files)

;; _____________________________________________________________________________
;;; EMACS LISP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Executing-Lisp>

;;; Localleader keymaps

;; Group macro-related commands into a keymap
(defvar-keymap eon-localleader-elisp-macro-map
  :doc "Macro/expand commands for Emacs Lisp."
  "m" #'emacs-lisp-macroexpand
  "p" #'pp-macroexpand-last-sexp)

;; Group compilaton commands ito a keymap
(defvar-keymap eon-localleader-elisp-compile-map
  :doc "Compilation commands for Emacs Lisp."
  "b" #'elisp-byte-compile-buffer
  "f" #'elisp-byte-compile-file)

;; Define localleader keymap for `emacs-lisp-mode'
(eon-localleader-defkeymap emacs-lisp-mode eon-localleader-elisp-map
  :doc "Local leader keymap for Emacs Lisp buffers."
  ;; Hook the "compilation" keymap into the localleader keymap
  "c" `("Compile" . ,eon-localleader-elisp-compile-map)
  "d" #'eval-defun
  "D" #'edebug-defun
  "e" #'eval-last-sexp
  "h" #'describe-symbol
  ;; Hook the "macro" keymap into the localleader keymap
  "m" `("Macro" . ,eon-localleader-elisp-macro-map)
  "p" #'pp-eval-last-sexp
  "r" #'elisp-eval-region-or-buffer)

;; Enable Flymake for Emacs Lisp, but never for lisp-interaction-mode.
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (unless (derived-mode-p 'lisp-interaction-mode)
              (flymake-mode 1))))

;; Reach eval-expression via "<leader> x"
(keymap-set ctl-z-e-map "x" #'eval-expression)

;; Emacs Lisp evaluation: don't truncate printed lists
(setopt eval-expression-print-length nil
        eval-expression-print-level nil)

;; Additional keybinding resembling other sexp-related keybindings
(keymap-global-set "C-M-<backspace>" #'backward-kill-sexp)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; Helpers

(defvar eon-lisp-src-modes-registry
  '(common-lisp-mode
    emacs-lisp-mode
    lisp-mode
    lisp-data-mode
    scheme-mode)
  "Registry of Lisp-related source modes.")

(defvar eon-lisp-repl-modes-registry
  '(eshell-mode
    inferior-emacs-lisp-mode
    inferior-lisp-mode
    inferior-scheme-mode
    lisp-interaction-mode)
  "Registry of Lisp-related REPL modes.")

(defun eon-lisp--modes-transform (modes switch)
  "Transform MODES according to SWITCH.
- 'hook returns corresponding -hook symbols"
  (pcase switch
    ('hook (mapcar (lambda (m) (intern (format "%s-hook" m))) modes))
    (_ modes)))

(defun eon-lisp-src-modes (&optional switch)
  "Return installed Lisp-related source modes from the registry.
With SWITCH = 'hook, return -hook variables."
  (eon-lisp--modes-transform
   (seq-filter #'fboundp eon-lisp-src-modes-registry)
   switch))

(defun eon-lisp-repl-modes (&optional switch)
  "Return installed Lisp-related REPL modes from the registry.
With SWITCH = 'hook, return -hook variables."
  (eon-lisp--modes-transform
   (seq-filter #'fboundp eon-lisp-repl-modes-registry)
   switch))

;; _____________________________________________________________________________
;;; QUIT EMACS

;; The standard way to leave Emacs
(keymap-set ctl-z-q-map "q" #'save-buffers-kill-terminal)

;; The standard way to leave Emacs
(keymap-set ctl-z-q-map "r" #'restart-emacs)

;; _____________________________________________________________________________
;;; SERVER
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Emacs-Server>
;; ... or do "M-x info-emacs-manual s server RET" to read it within Emacs

;; Display the name of the Emacs server process in the frame title
;; to see easily to which server process a client is connected to
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
(add-hook 'emacs-startup-hook #'server-start)

;; _____________________________________________________________________________
(provide 'eon)
;;; eon.el ends here
