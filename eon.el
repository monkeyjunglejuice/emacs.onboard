;;; eon.el --- Emacs ONBOARD Starter Kit -*- lexical-binding: t; no-byte-compile: t; -*-

;;    ▒░▒░▒░   ▒░     ▒░ ▒░▒░▒░▒░     ▒░▒░▒░      ▒░    ▒░▒░▒░▒░    ▒░▒░▒░▒░
;;   ▒░    ▒░  ▒░▒░   ▒░  ▒░     ▒░  ▒░    ▒░    ▒░▒░    ▒░     ▒░   ▒░    ▒░
;;  ▒░      ▒░ ▒░ ▒░  ▒░  ▒░     ▒░ ▒░      ▒░  ▒░  ▒░   ▒░     ▒░   ▒░     ▒░
;;  ▒░      ▒░ ▒░  ▒░ ▒░  ▒░▒░▒░▒░  ▒░      ▒░ ▒░    ▒░  ▒░▒░▒░▒░    ▒░     ▒░
;;  ▒░      ▒░ ▒░   ▒░▒░  ▒░     ▒░ ▒░      ▒░ ▒░▒░▒░▒░  ▒░   ▒░     ▒░     ▒░
;;   ▒░    ▒░  ▒░     ▒░  ▒░     ▒░  ▒░    ▒░ ▒░      ▒░ ▒░     ▒░   ▒░    ▒░
;;    ▒░▒░▒░  ▒░      ▒░ ▒░▒░▒░▒░     ▒░▒░▒░  ▒░      ▒░ ▒░      ▒░ ▒░▒░▒░▒░

;; Emacs ONBOARD offers a clean slate to build your personal Emacs config.
;; It stays close to vanilla Emacs, but offers convenience and a better
;; user experience, while only relying on built-in packages.

;; Copyright (C) 2021–2025 Dan Dee
;;
;; Emacs ONBOARD is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; Emacs ONBOARD is distributed in the hope that it will be useful, but WITHOUT
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
;; Version: 2.4.2
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
;; Examples:
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
;;; Please report bugs and issues here:
;; <https://github.com/monkeyjunglejuice/emacs.onboard/issues>
;;
;;; Code:

;; _____________________________________________________________________________
;;; CUSTOMIZATION GROUPS

;; Customize the most important Emacs ONBOARD settings via "<leader> x C"

(defgroup eon nil
  "Emacs ONBOARD starter kit & ONTOP extension layer."
  :group 'convenience)

(defgroup eon-misc nil
  "Various settings that don't belong to another group."
  :group 'eon)

;; _____________________________________________________________________________
;;; GARBAGE COLLECTION
;; <https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Garbage-Collection>

;; "Garbage Collection Magic Hack" by Andrea Corallo <akrl@sdf.org> v0.2.1
;; Original: <https://gitlab.com/koral/gcmh> License: GPL-3.0-OR-LATER
;; Discussion: <https://news.ycombinator.com/item?id=39190110>

(defgroup eon-gcmh nil
  "Garbage collection tuning."
  :group 'eon)

(defcustom eon-gcmh-high-cons-threshold (* 1024 1024 1024)  ; 1 GiB
  "High cons GC threshold.
This should be set to a value that makes GC unlikely but does not
cause OS paging."
  :group 'eon-gcmh
  :type '(number))

;; Set the high value immediately to prevent frequent garbage collections
;; during initialization. Will be adjusted dynamically when `eon-gcmh-mode'
;; is activated via `emacs-startup-hook'.
(setq gc-cons-threshold eon-gcmh-high-cons-threshold)

(defcustom eon-gcmh-low-cons-threshold 800000
  "Low cons GC threshold.
This is the GC threshold used while idling. Default value is the
same of `gc-cons-threshold' default."
  :group 'eon-gcmh
  :type '(number))

(defcustom eon-gcmh-idle-delay 15
  "Idle time to wait in seconds before triggering GC.
If `auto', this is auto-computed based on `eon-gcmh-auto-idle-delay-factor'."
  :group 'eon-gcmh
  :type '(choice number (const auto)))

(defcustom eon-gcmh-auto-idle-delay-factor 20
  "Factor to compute the idle delay when in idle-delay auto mode.
The idle delay will be `eon-gcmh-auto-idle-delay-factor' times the
time the last non idle garbage collection time."
  :group 'eon-gcmh
  :type '(number))

(defcustom eon-gcmh-verbose nil
  "If t, print a message when garbage collecting."
  :group 'eon-gcmh
  :type '(boolean))

(defvar eon-gcmh-idle-timer nil
  "Idle timer for triggering GC.")

(defmacro eon-gcmh-time (&rest body)
  "Measure and return the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defun eon-gcmh-set-high-threshold ()
  "Set the high GC threshold.
This is to be used with the `pre-command-hook'."
  (setf gc-cons-threshold eon-gcmh-high-cons-threshold))

(defvar eon-gcmh-last-gc-time 0.1
  "How long it took to perform the last garbage collection.")

(defun eon-gcmh-register-idle-gc ()
  "Register a timer to run `eon-gcmh-idle-garbage-collect'.
Cancel the previous one if present."
  (let ((idle-t (if (eq eon-gcmh-idle-delay 'auto)
                    (* eon-gcmh-auto-idle-delay-factor eon-gcmh-last-gc-time)
                  eon-gcmh-idle-delay)))
    (when (timerp eon-gcmh-idle-timer)
      (cancel-timer eon-gcmh-idle-timer))
    (setf eon-gcmh-idle-timer
          (run-with-timer idle-t nil #'eon-gcmh-idle-garbage-collect))))

(defun eon-gcmh-idle-garbage-collect ()
  "Run garbage collection after `eon-gcmh-idle-delay'."
  (if eon-gcmh-verbose
      (progn
        (message "Garbage collecting...")
        (condition-case-unless-debug e
            (message "Garbage collecting...done (%.3fs)"
                     (setf eon-gcmh-last-gc-time (eon-gcmh-time (garbage-collect))))
          (error (message "Garbage collecting...failed")
                 (signal (car e) (cdr e)))))
    (setf eon-gcmh-last-gc-time (eon-gcmh-time (garbage-collect))))
  (setf gc-cons-threshold eon-gcmh-low-cons-threshold))

(define-minor-mode eon-gcmh-mode
  "Minor mode to tweak Garbage Collection strategy."
  :group 'eon-gcmh
  :global t
  :init-value t
  (if eon-gcmh-mode
      (progn
        (setf gc-cons-threshold eon-gcmh-high-cons-threshold)
        ;; Release severe GC strategy before the user restart to working
        (add-hook 'pre-command-hook #'eon-gcmh-set-high-threshold)
        (add-hook 'post-command-hook #'eon-gcmh-register-idle-gc))
    (setf gc-cons-threshold eon-gcmh-low-cons-threshold
          eon-gcmh-idle-timer nil)
    (remove-hook 'pre-command-hook #'eon-gcmh-set-high-threshold)
    (remove-hook 'post-command-hook #'eon-gcmh-register-idle-gc)))

;; Activate GCMH mode (idle timer) after Emacs startup
(add-hook 'emacs-startup-hook #'eon-gcmh-mode)

;; _____________________________________________________________________________
;;; DEBUG / DIAGNOSTICS

;; Show Emacs init time and how many garbage collections happened during init
(add-hook 'window-setup-hook
          (lambda ()
            (message "Emacs started in %s with %d garbage collections."
                     (format "%.3f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

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

;; This options are not set if Emacs is started via "emacs --debug-init"
(unless init-file-debug
  (setopt
   ;; When to bring the buffer to the foreground? Defaults to :warning
   warning-minimum-level :error
   ;; Allow bytecode compilation to be verbose?
   byte-compile-verbose nil
   ;; Allow native compilation to be verbose?
   native-comp-async-report-warnings-errors nil
   native-comp-warning-on-missing-source nil))

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

  ;;; 1st priority: Gnu Elpa, enabled per default

  ;;; 2nd priority: Non-Gnu Elpa, enabled per default

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
;;; GLOBAL DEFINITIONS & UTILITIES

;;; - Some commonly useful predicates

(defun eon-linp ()
  "True if `system-type' is GNU/Linux or something compatible.
For finer granularity, use the variables `system-type'
or `system-configuration' directly."
  (memq system-type '(gnu/linux berkeley-unix gnu gnu/kfreebsd)))

(defun eon-wslp ()
  "True if `system-type' is GNU/Linux or compatible, running within WSL.
For finer granularity, use the variables `system-type'
or `system-configuration' directly."
  (and (memq system-type '(gnu/linux berkeley-unix gnu gnu/kfreebsd))
       (or (getenv "WSLENV")
           (getenv "WSL_DISTRO_NAME"))))

(defun eon-winp ()
  "True if `system-type' is Microsoft Windows or something compatible.
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

(defun eon-terminalp ()
  "True if Emacs does run in the terminal UI."
  (and (tty-type)
       (not (display-graphic-p))))

;;; - Extended `add-to-list' and friends

;; TODO Since we're using `cl-lib' anyway, we could use keyword arguments
;; instead of merely positional arguments.
(require 'cl-lib)

(defun eon-adjoin (cur elements &optional append compare-fn)
  "Return a new list like CUR with ELEMENTS added once.

ELEMENTS may be an item or a list. If APPEND is non-nil, append items
left->right; otherwise prepend while preserving ELEMENTS order.
COMPARE-FN is the membership predicate (default `equal').

Examples:
  (eon-adjoin '(a b) 'c)                                 ; => (c a b)
  (eon-adjoin '(a b) '(c a))                             ; => (c a b)
  (eon-adjoin '(a b) '(d) t)                             ; => (a b d)
  (eon-adjoin '(a b) '(\"x\" \"x\") nil #'string-equal)  ; => (\"x\" a b)"
  (let* ((xs   (if (consp elements) elements (list elements)))
         (test (or compare-fn #'equal))
         (cand (if append (append cur xs) (append xs cur))))
    (cl-remove-duplicates cand :test test)))

(defun eon-add-to-list (list-sym elements &optional append compare-fn)
  "Modifies the current binding of LIST-SYM, respects buffer-local.

LIST-SYM is a symbol naming a list variable whose *current binding*
will be modified (i.e. buffer-local if such exists, otherwise global).

ELEMENTS may be a single item or a list of items to add to LIST-SYM.
If APPEND is non-nil, append items left→right; otherwise prepend them
while preserving the order of ELEMENTS.

COMPARE-FN, if non-nil, is a function used to test for membership; it
defaults to `equal`.

Examples (assuming LIST-SYM initially holds (a b)):
  (eon-add-to-list 'v 'c)           ; => (c a b)
  (eon-add-to-list 'v '(c a))       ; => (c a b)
  (eon-add-to-list 'v '(d) t)       ; => (a b d)

Returns the new current value of LIST-SYM."
  (unless (symbolp list-sym)
    (error "eon-add-to-list: LIST-SYM must be quoted: 'my-var"))
  (set list-sym
       (eon-adjoin
        (if (boundp list-sym) (symbol-value list-sym) nil)
        elements append compare-fn)))

(defun eon-add-to-list* (list-sym elements &optional append compare-fn)
  "Modifies the default, custom or global value of LIST-SYM.

If LIST-SYM is a user option (see `custom-variable-p'), use
`customize-set-variable' so its :set function and type checks are
applied. Otherwise, use `set-default' to modify the variable’s global
default value directly.

ELEMENTS may be a single item or a list of items to add to the
variable’s *default* (global) value.

If APPEND is non-nil, append items left->right;
otherwise prepend them while preserving the order of ELEMENTS.

COMPARE-FN, if non-nil, is a function used to test for membership;
it defaults to `equal'.

Returns the new default value of LIST-SYM."
  (unless (symbolp list-sym)
    (error "eon-add-to-list*: LIST-SYM must be a symbol"))
  (let* ((cur (and (default-boundp list-sym) (default-value list-sym)))
         (new (eon-adjoin cur elements append compare-fn)))
    (if (custom-variable-p list-sym)
        (customize-set-variable list-sym new 'setopt)
      (set-default list-sym new))
    new))

;; General helper to update association lists
(cl-defun eon-alist-update (key value alist
                                &key (test #'equal) unique)
  "Return a new ALIST like ALIST but with KEY mapped to VALUE.

- If KEY is present (per TEST), replace the first occurrence,
  preserving order.
- If UNIQUE is non-nil, also remove any later occurrences of KEY
  in the tail.
- If KEY is not present, cons (KEY . VALUE) to the front.

Does not mutate ALIST, so if ALIST should to be set, it must be
set explicitly."
  (let ((found nil))
    (setq alist
          (cl-loop for (k . v) in alist
                   unless (and unique
                               found
                               (funcall test key k))
                   collect (if (and (not found)
                                    (funcall test key k))
                               (prog1 (cons key value)
                                 (setq found t))
                             (cons k v))))
    (if found
        alist
      (cons (cons key value) alist))))

;; Get all the parent major modes
(defun eon-get-parent-modes ()
  "Return a list with `major-mode' as the `car' and its parents as the `cdr'.
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

;; Ensure directory names have one trailing slash
(defun eon-ensure-trailing-slash (dir)
  "Return DIR with exactly one trailing slash."
  (declare (important-return-value t) (side-effect-free error-free))
  ;; Both "/tmp" and "/tmp//" result in "/tmp/"
  (file-name-as-directory (directory-file-name dir)))

;; _____________________________________________________________________________
;;; EMACS SYSTEM LIMITS

;; Increase warning threshold
(setopt large-file-warning-threshold (* 64 1024 1024))  ; 64 MiB

;; Increase undo limit
(setopt undo-limit (* 64 1024 1024)  ; 64 MiB
        undo-strong-limit (* 96 1024 1024)   ; 96 MiB
        undo-outer-limit (* 960 1024 1024))  ; 960 MiB

;; Increase the amount of data that Emacs reads from subprocesses in one chunk.
;; Aims to increase performance for communication with language servers, etc.
(setopt read-process-output-max (* 1024 1024))  ; 1 MiB

;; _____________________________________________________________________________
;;; DEFAULT AND INITIAL FRAME

;; In Emacs terminology, a "frame" means the ordinary "desktop window";
;; while "window" refers to tiled panels within an Emacs frame. Why?
;; Because Emacs had it first, and today's convention what "window" means
;; appeared later.
;;
;; In order to define properties generally, add them to `default-frame-alist';
;; to affect only the first frame created, add them to `initial-frame-alist'.
;; The following examples set the default for all Emacs frames created:

;; Either start each Emacs frame maximized …
;; (setf (alist-get 'fullscreen default-frame-alist) 'maximized)

;; … or set the default width of each frame - in columns, or full width.
;; (setf (alist-get 'width default-frame-alist) 80)
;; (setf (alist-get 'fullscreen default-frame-alist) 'fullwidth)

;; … or set the default height of each frame - in lines, or full height.
;; (setf (alist-get 'height default-frame-alist) 32)
;; (setf (alist-get 'fullscreen default-frame-alist) 'fullheight)

;; Horizontal position - set the distance from the screen edge in pixels.
;; Distance from the left edge:
;; (setf (alist-get 'left default-frame-alist) 1)
;; Distance from the right edge, using negative numbers:
;; (setf (alist-get 'left default-frame-alist) -1)

;; Vertical position - set the distance from the screen edge in pixels.
;; Distance from the top edge:
;; (setf (alist-get 'top default-frame-alist) 1)
;; Distance from the bottom edge, using negative numbers:
;; (setf (alist-get 'top default-frame-alist) -1)

;; Bring frame to the front (steals focus)
(add-hook 'window-setup-hook
          (lambda ()
            (when (display-graphic-p)
              (select-frame-set-input-focus (selected-frame)))))

;; _____________________________________________________________________________
;;; USER INTERFACE

;; Menu bar: on/off by default?
(menu-bar-mode 1)

;; Scroll bar: on/off by default?
(when (fboundp #'scroll-bar-mode) (scroll-bar-mode -1))

;; Tool bar: on/off by default?
(when (fboundp #'tool-bar-mode) (tool-bar-mode -1))

;; Tooltips: enable/disable?
(tooltip-mode -1)

;; Startup screen: on/off by default?
(setopt inhibit-startup-screen t)

;; Alarms: turn off?
(setopt ring-bell-function 'ignore)

;; Draw extra space/lines between windows?
;; Helps if a certain theme makes the window boundaries indistinguishable,
;; or if you prefer to have some space between windows.
(window-divider-mode -1)
(setopt window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)

;; _____________________________________________________________________________
;;; CURSOR
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Cursor-Display>

;; Turn on/off cursor blinking by default? 1 means 'on' and -1 means 'off'
(blink-cursor-mode -1)

;; Cursor blinking interval in seconds
(setopt blink-cursor-interval 0.3)

;; Blink cursor that often before going into solid state
(setopt blink-cursor-blinks 3)

;; Emphasize the cursor when running Emacs in a text terminal?
(setopt visible-cursor nil)

;; Show a cursor in inactive windows?
(setopt cursor-in-non-selected-windows nil)

;; Make sure to highlight the current line only in the active window
(setopt hl-line-sticky-flag nil)

;; Highlight current line in special modes?
(add-hook 'special-mode-hook (lambda () (hl-line-mode 1)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Change the cursor type based on a certain state

;; To learn about available cursor types, place your cursor on `cursor-type'
;; and do "C-h o"; or do "M-x describe-symbol RET cursor-type RET"

;; Example how to set your default cursor in your init.el:
;; (setopt eon-cursor-type-write 'box)  ; block-style cursor

(defgroup eon-cursor nil
  "Cursor styles."
  :group 'eon)

(defun eon-cursor-type--set (symbol value)
  "Set SYMBOL to VALUE.
Only `eon-cursor-type-write' updates frame cursor defaults."
  (set-default symbol value)
  (when (eq symbol 'eon-cursor-type-write)
    (setopt initial-frame-alist
            (cons (cons 'cursor-type value)
                  (assq-delete-all 'cursor-type
                                   initial-frame-alist)))
    (setopt default-frame-alist
            (cons (cons 'cursor-type value)
                  (assq-delete-all 'cursor-type
                                   default-frame-alist))))
  (force-mode-line-update t))

(defcustom eon-cursor-type-write 'bar
  "Cursor style for text input.
Accepts an expression that returns either:
- t or nil
- one of the symbols: 'bar 'hbar 'box 'hollow
- a pair '(SYMBOL . INTEGER), e.g. '(hbar . 3).
See also `cursor-type'."
  :type '(sexp)
  :group 'eon-cursor
  :set #'eon-cursor-type--set)

(defcustom eon-cursor-type-select 'bar
  "Cursor style for selecting text in writing- or modal insert states.
Accepts an expression that returns either:
- t or nil
- one of the symbols: 'bar 'hbar 'box 'hollow
- a pair '(SYMBOL . INTEGER), e.g. '(hbar . 3).
See also `cursor-type'."
  :type '(sexp)
  :group 'eon-cursor
  :set #'eon-cursor-type--set)

(defcustom eon-cursor-type-view '(hbar . 3)
  "Cursor style for read-only buffers.
Accepts an expression that returns either:
- t or nil
- one of the symbols: 'bar 'hbar 'box 'hollow
- a pair '(SYMBOL . INTEGER), e.g. '(hbar . 3).
See also `cursor-type'."
  :type '(sexp)
  :group 'eon-cursor
  :set #'eon-cursor-type--set)

(defcustom eon-cursor-type-extra 'box
  "Cursor style for command- and modal \"normal\" states.
Accepts an expression that returns either:
- t or nil
- one of the symbols: 'bar 'hbar 'box 'hollow
- a pair '(SYMBOL . INTEGER), e.g. '(hbar . 3).
See also `cursor-type'."
  :type '(sexp)
  :group 'eon-cursor
  :set #'eon-cursor-type--set)

(defcustom eon-cursor-type-extra-select 'hollow
  "Cursor style for command- and modal \"normal\" states when region is active.
Accepts an expression that returns either:
- t or nil
- one of the symbols: 'bar 'hbar 'box 'hollow
- a pair '(SYMBOL . INTEGER), e.g. '(hbar . 3)
See also `cursor-type'."
  :type '(sexp)
  :group 'eon-cursor
  :set #'eon-cursor-type--set)

(defvar eon-cursor-functions nil
  "Hook of functions that may compute a cursor type.
Each function is called with no args and should return either
a `cursor-type' or nil. The first non-nil return wins.")

(defun eon-cursor-type--desired ()
  "Compute desired cursor type for the current buffer."
  (or (run-hook-with-args-until-success 'eon-cursor-functions)
      (cond
       ((region-active-p) eon-cursor-type-select)
       (buffer-read-only eon-cursor-type-view)
       (t eon-cursor-type-write))))

(defun eon-cursor-update (&rest _)
  "Apply the desired cursor type to the current buffer."
  (setq-local cursor-type (eon-cursor-type--desired)))

(define-minor-mode eon-cursor-mode
  "Globally change cursor type according to state."
  :group 'eon-cursor
  :global t
  :init-value t
  (if eon-cursor-mode
      (progn
        (mapc (lambda (buf)
                (with-current-buffer buf (eon-cursor-update)))
              (buffer-list))
        ;; It seems unreasonable to use `after-command-hook' to update
        ;; the cursor type; better avoid that potential overhead.
        ;; Instead we're listing the triggers one-by-one.
        (add-hook   'buffer-list-update-hook      #'eon-cursor-update)
        (add-hook   'read-only-mode-hook          #'eon-cursor-update)
        (add-hook   'after-change-major-mode-hook #'eon-cursor-update)
        ;; React to selections
        (add-hook   'activate-mark-hook           #'eon-cursor-update)
        (add-hook   'deactivate-mark-hook         #'eon-cursor-update)
        ;; Make sure the cursor will be updated after leaving wdired
        (advice-add 'wdired-abort-changes :after  #'eon-cursor-update)
        (advice-add 'wdired-finish-edit :after    #'eon-cursor-update))
    ;; Tear down
    (remove-hook   'buffer-list-update-hook      #'eon-cursor-update)
    (remove-hook   'read-only-mode-hook          #'eon-cursor-update)
    (remove-hook   'after-change-major-mode-hook #'eon-cursor-update)
    (remove-hook   'activate-mark-hook           #'eon-cursor-update)
    (remove-hook   'deactivate-mark-hook         #'eon-cursor-update)
    (advice-remove 'wdired-abort-changes         #'eon-cursor-update)
    (advice-remove 'wdired-finish-edit           #'eon-cursor-update)))

;; Turn it on
(eon-cursor-mode 1)

;; _____________________________________________________________________________
;;; WHICH-KEY
;; Show a menu with available keybindings

(which-key-mode 1)

(setopt which-key-lighter ""
        which-key-separator " "
        which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.0
        which-key-sort-uppercase-first nil
        which-key-sort-order 'which-key-key-order-alpha
        which-key-preserve-window-configuration t
        which-key-show-remaining-keys t
        which-key-show-transient-maps t)

;; _____________________________________________________________________________
;;; LEADER-KEY / LOCAL LEADER-KEY and KEYMAPS

;; To avoid clashes, new keybindings introduced by Emacs ONBOARD will usually
;; live under the leader prefix (with only a few exceptions).
;;
;;; - Defaults for graphical Emacs:
;; "C-," is the leader key, reach the local leader via "C-, C-,"
;;
;;; - Defaults for Emacs with terminal UI - invoked by "emacs -nw":
;; "C-z" is the leader key, reach the local leader via "C-z C-z"
;;
;; Terminal note: In "emacs -nw", "C-z" is normally bound to suspend Emacs.
;; We rebind it as a leader, and it works in modern terminals - e.g. WezTerm.
;; If your TTY converts C-z to SIGTSTP before Emacs sees it (rare), disable
;; the suspend char or move it - see optional snippet below.
;;
;; (add-hook 'tty-setup-hook
;;           (lambda ()
;;             (ignore-errors
;;               ;; No kernel suspend on ^Z
;;               (call-process "stty" nil nil nil "susp" "undef")
;;               ;; No XON/XOFF flow control stealing C-s/C-q
;;               (call-process "stty" nil nil nil "-ixon" "-ixoff"))))

;; Group for customizations
(defgroup eon-leader nil
  "Customize leader key and local leader key behavior."
  :group 'eon)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Local leader implementation

(defvar-keymap eon-localleader-global-map
  :doc "Global local leader keymap (fallback for all major and minor modes).
You can bind commands here that should appear in all local leader keymaps."
  "," '("..." . execute-extended-command-for-buffer))

(defvar-local eon-localleader--map eon-localleader-global-map
  "Active localleader keymap for the current buffer.
Don't bind any keys/commands to this keymap.")

;; KLUDGE Relies currently on `which-key' internals;
;; that's a bit of an eyesore, but seems to work reliably.
(defun eon-localleader--context-window ()
  "Return the window where the key sequence started."
  (cond
   ((and (boundp 'which-key--original-window)
         (window-live-p which-key--original-window))
    which-key--original-window)
   (t (selected-window))))

(defun eon-localleader--set-key (sym value)
  "Setter for `eon-localleader-key'."
  (let ((old (and (boundp sym) (symbol-value sym))))
    (when (boundp 'ctl-z-map)
      (when old (keymap-unset ctl-z-map old))
      (keymap-set ctl-z-map value eon-localleader-map)))
  (set-default sym value))

;; Empty named prefix, so which-key shows the label "Local"
(defvar-keymap eon-localleader-map
  :doc "Frontend; lets the loacal leader machinery appear as a single keymap.

You may not want to bind keys/commands to this map, because they will be
globally visible under the local leader and override commands bound
to the same keys.

- If you actually want a certain key/command always appear under the
  local leader, bind the key/command to `eon-localleader-global-map'.

- If you want to bind/rebind a key/command in a mode-specific local leader
  keymap, then use `keymap-set'.
  Example: (keymap-set eon-localleader-elisp-map \"x\" #'eval-defun)

  Pre-defined local leader keymaps are named according to the schema
  'eon-localleader-[mode name or purpose]-map'. You can look them up
  via '<leader> h v'.

- If you want to define a new local leader keymap for a specific mode,
  use `eon-localleader-defkeymap'."
  :name "Local")

(defun eon-localleader--sync-local-prefix-parent ()
  "Make `eon-localleader-map' inherit the effective local leader keymap.
Respects the which-key origin window so that the correct buffer's
local leader is shown."
  (let* ((win (eon-localleader--context-window))
         (buf (and (window-live-p win) (window-buffer win)))
         (map (with-current-buffer (or buf (current-buffer))
                (if (keymapp eon-localleader--map)
                    eon-localleader--map
                  eon-localleader-global-map))))
    (set-keymap-parent eon-localleader-map map)))

;; Keep the UI prefix parent fresh when modes change, even without which-key
(add-hook 'after-change-major-mode-hook
          #'eon-localleader--sync-local-prefix-parent)

(with-eval-after-load 'which-key
  (advice-add 'which-key--update :before
              (lambda (&rest _)
                (eon-localleader--sync-local-prefix-parent))))

(defcustom eon-localleader-key
  (if (display-graphic-p) "C-," "C-z")
  "Local leader key, pressed after the leader.

- GUI default: \"C-,\" -> reach local leader via \"C-, C-,\"
- TTY default: \"C-z\" -> reach local leader via \"C-z C-z\"

Use `eon-customize-group' to change, or use `setopt' from Lisp."
  :group 'eon-leader
  :type '(string)
  :set #'eon-localleader--set-key)

(defmacro eon-localleader-defkeymap (mode map-sym &rest body)
  "Define a mode-specific local leader keymap.

MODE can be any major or minor mode.
MAP-SYM can be an arbitrary name for your keymap.
BODY will be forwarded to `defvar-keymap'.

- Example how to define an empty mode-specific local leader keymap:
  (eon-localleader-defkeymap org-mode eon-localleader-org-mode-map
    :doc \"Localleader map for Org mode.\")

  Bind keys/commands or sub-keymaps with `keymap-set' later.

- Example how to define a mode-specific local leader keymap with bindings:
  (eon-localleader-defkeymap emacs-lisp-mode eon-localleader-elisp-map
    :doc \"Local leader keymap for Emacs Lisp buffers.\"
    \"e\"   #'eval-last-sexp
    \"E\"   #'pp-eval-last-sexp
    \"h\"   #'describe-symbol)"
  (declare (indent 2))
  (let ((hook (intern (format "%s-hook" mode))))
    `(progn
       (defvar-keymap ,map-sym ,@body)
       ;; Inherit global entries so globals are always available
       (set-keymap-parent ,map-sym eon-localleader-global-map)
       ;; Activate buffer-locally in this mode
       (add-hook ',hook (lambda ()
                          (setq-local eon-localleader--map ,map-sym))))))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Leader implementation

;; FIXME Customizing `eon-leader-key' doesn't set the label of the alternative
;; (mirrored) local leader kebinding/keymap, so that the local leader keybinding
;; shows only the default "+prefix" in which-key.
;; But the leader setter for Evil and God mode in Emacs ONTOP work just fine,
;; and that code has to be merged anyway - it will probably fix this issue.
(defun eon-leader--set-key (sym value)
  "Setter for `eon-leader-key'.
SYM is the variable, VALUE is the keybinding as a string.

Also sets the local leader key to the same keybinding within the
local leader keymap, so that the local leader key can be typed rapidly
by holding down one key and hitting the other twice.

Example:
Setting the leader to \"M-SPC\" will set the local leader to \"M-SPC\".
Customize `eon-localleader-key' explicitly to override this default."
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
  "Leader prefix key. GUI default: \"C-,\" - TTY default: \"C-z\".
Use `eon-customize-group' to change, or `setopt' from Lisp."
  :group 'eon-leader
  :type '(string)
  :set #'eon-leader--set-key)

;; Sub-keymaps under the leader

(defvar-keymap ctl-z-b-map   :doc "Buffer")
(defvar-keymap ctl-z-c-map   :doc "Code")
(defvar-keymap ctl-z-e-map   :doc "Exec")
(defvar-keymap ctl-z-f-map   :doc "File")
(defvar-keymap ctl-z-g-map   :doc "Goto")
(defvar-keymap ctl-z-h-map   :doc "Help")
(defvar-keymap ctl-z-i-map   :doc "Insert")
(defvar-keymap ctl-z-j-map   :doc "User")
(defvar-keymap ctl-z-o-map   :doc "Org")
(defvar-keymap ctl-z-p-map   :doc "Project")
(defvar-keymap ctl-z-q-map   :doc "Quit")
(defvar-keymap ctl-z-r-map   :doc "Register")
(defvar-keymap ctl-z-s-map   :doc "Search")
(defvar-keymap ctl-z-t-map   :doc "Tab/WS")
(defvar-keymap ctl-z-v-map   :doc "VC/Git")
(defvar-keymap ctl-z-w-map   :doc "Window")
(defvar-keymap ctl-z-x-map   :doc "Misc")
(defvar-keymap ctl-z-ret-map :doc "Bookmark")

;; Default Top-level leader keymap, referencing the sub-keymaps
;; TODO Rename ctl-z-.*-map to eon-leader-default-.*-map, because
;; the ctl-z-... part is merely historical and has no meaning anymore.

(defvar-keymap ctl-z-map
  :doc "Top-level leader keymap."
  "b"   `("Buffer"   . ,ctl-z-b-map)
  "c"   `("Code"     . ,ctl-z-c-map)
  "e"   `("Exec"     . ,ctl-z-e-map)
  "f"   `("File"     . ,ctl-z-f-map)
  "g"   `("Goto"     . ,ctl-z-g-map)
  "h"   `("Help"     . ,ctl-z-h-map)
  "i"   `("Insert"   . ,ctl-z-i-map)
  "j"   `("User"     . ,ctl-z-j-map)
  "o"   `("Org"      . ,ctl-z-o-map)
  "p"   `("Project"  . ,ctl-z-p-map)
  "q"   `("Quit"     . ,ctl-z-q-map)
  "r"   `("Register" . ,ctl-z-r-map)
  "s"   `("Search"   . ,ctl-z-s-map)
  "t"   `("Tab/WS"   . ,ctl-z-t-map)
  "v"   `("VC/Git"   . ,ctl-z-v-map)
  "w"   `("Window"   . ,ctl-z-w-map)
  "x"   `("Misc"     . ,ctl-z-x-map)
  "RET" `("Bookmark" . ,ctl-z-ret-map)
  ;; Add dynamic localleader keymap
  eon-localleader-key `("Local" . ,eon-localleader-map))

;; Don't like the pre-defined keybindings of the default leader keymap?
;; There is an alternative, blank-slate leader keymap.
;; The only bound key/command is the local leader.
(defvar-keymap eon-leader-user-map
  :doc "Alternative top-level leader keymap, initially empty.
Ready to populate with your own sub-keymaps and keybindings:

- How to add single commands to the `eon-leader-user-map' top-level
  (keymap-set eon-leader-user-map \"f\" #'find-file)

- How to add new sub-keymaps
1. Define a sub-keymap:
   (defvar-keymap my-leader-g-map :doc \"Goto\")
2. Add your sub-keymap to the `eon-leader-user-map':
   (keymap-set eon-leader-user-map \"g\" `(\"Goto\" . ,my-leader-g-map))

- How to add a keybinding to your previously defined sub-keymap
  (keymap-set my-leader-g-map \"w\" #'browse-web)

In order to activate this keymap instead of the default leader keymap,
customize `eon-leader-map-name'."
  ;; Add dynamic localleader keymap
  eon-localleader-key `("Local" . ,eon-localleader-map))

(defvar eon-leader-map nil
  "Resolved leader keymap from `eon-leader-map-name'.
This variable always holds the actual keymap object currently selected
as the leader keymap.

You usually should bind keys in the source keymap you selected,
because changing `eon-leader-map-name' will make this variable
point to a different keymap. The source keymaps are:
- `ctl-z-map'; contains default leader keybindings
- `eon-leader-user-map'; a pre-defined but clean-slate leader keymap
- Other keymap you specified via `eon-leader-map-name'")

(defun eon-leader-map--set (sym value)
  "Setter for `eon-leader-map-name', storing VALUE in SYM.
Warns if VALUE is bound but not a keymap; allows unbound symbols."
  (set-default sym value)
  (cond
   ;; Bound, but not a keymap
   ((and (symbolp value) (boundp value)
         (not (keymapp (symbol-value value))))
    (setq eon-leader-map nil)
    (message "Warning: %S is bound, but not to a keymap." value))
   ;; Bound and a keymap: resolve + rebind
   ((and (symbolp value) (boundp value)
         (keymapp (symbol-value value)))
    (setq eon-leader-map (symbol-value value))
    (when (boundp 'eon-leader-key)
      (keymap-global-set eon-leader-key eon-leader-map)))
   ;; Unbound symbol allowed, but nothing to bind yet
   (t
    (setq eon-leader-map nil))))

(defcustom eon-leader-map-name 'ctl-z-map
  "Name of the keymap that will act as the top-level leader keymap.

When setting this variable from Lisp, make sure to quote the symbol.
Example: (setopt eon-leader-map-name 'eon-leader-user-map)

- 'Default leader keymap' points to `ctl-z-map':
  The keymap is active per default and contains a useful set
  of pre-defined keybindings.

- 'User leader keymap' points to `eon-leader-user-map':
  This keymap is initially empty, for you to roll your own keybindings.
  See `eon-leader-user-map' for examples how to set them.

- 'Other keymap':
  Specify a symbol bound to a keymap, or expression that evaluates to
  a quoted symbol bound to a keymap. You can use any keymap you like."
  :group 'eon-leader
  :type '(choice (const :tag "Default leader keymap" ctl-z-map)
                 (const :tag "User leader keymap" eon-leader-user-map)
                 (symbol :tag "Other keymap"))
  :set #'eon-leader-map--set)

;; Initial binding of the leader prefix to the enabled top-level leader keymap
(keymap-global-set eon-leader-key eon-leader-map)

;; Make the leader key available in the minibuffer too
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (when (keymapp (current-local-map))
              (keymap-set (current-local-map)
                          eon-leader-key eon-leader-map))))

;; _____________________________________________________________________________
;;; GENERAL KEYBINDINGS

(when (eon-macp)
  (setopt
   ;; Make the <Command> key on MacOS act as <Ctrl> key: "C- ..."
   mac-command-modifier 'control
   ;; Make the <Option> key on MacOS act as <Meta> key for "M- ..."
   mac-option-modifier 'meta
   ;; Don't bypass "C-h ..." keybindings
   mac-pass-command-to-system nil))

;; Make "M-x" available in the leader keymap
(keymap-set ctl-z-map "m" #'execute-extended-command)

;; Add `universal-argument' to the leader keymap
(keymap-set ctl-z-map "u" #'universal-argument)

;; Quit Emacs
(keymap-set ctl-z-q-map "q" #'save-buffers-kill-terminal)

;; Restart Emacs
(keymap-set ctl-z-q-map "r" #'restart-emacs)

;; _____________________________________________________________________________
;;; VI KEYBINDINGS (VIPER-MODE)

;; FIXME Needs a customizable leader key;
;; e.g. eon-viper-leader-key and eon-viper-localleader-key.
;; Best keymap to bind it seems `viper-vi-global-user-map'.

(with-eval-after-load 'viper
  (setopt
   viper-inhibit-startup-message t    ; don't show Viper's start up message
   viper-expert-level            '5   ; use max Emacs experience level
   viper-case-fold-search        t    ; ingore case when searching
   viper-ex-style-editing        nil  ; delete past line's beginning
   viper-ex-style-motion         nil  ; move past line's beginning
   ))

(with-eval-after-load 'viper-cmd
  (viper-buffer-search-enable))

;; _____________________________________________________________________________
;;; FONTS
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Fonts>

;; Either set your preferred fonts with the Customization UI "<leader> x C"
;; or in your 'init.el' (fonts must be installed on your computer).
;; Example:
;; (setopt eon-font-default "Iosevka Curly"      ; font name
;;         eon-font-default-size 150             ; 15pt - base size in 1/10 pt
;;         eon-font-proportional "Gentium Plus"  ; font name
;;         eon-font-proportional-size 160        ; size in 1/10 pt
;;         eon-font-marginal-size 0.9)           ; 90% for mode line and tabs

(defgroup eon-font-settings nil
  "Font settings."
  :group 'eon)

;; FIXME Fix inheritance, so that the fallbacks are actually set
;; TODO Add a setter function to the custom variables

(defcustom eon-font-default nil
  "Name of the default font; set it to a monospaced or duospaced font.
If not set explicitly, choosen by Emacs according to your system's default."
  :group 'eon-font-settings
  :type '(string))

(defcustom eon-font-default-size 140
  "Set the default font size in 1/10 of the desired point size.
Example: 140 -> 14 pt
You must specify an absolute size as an integer."
  :group 'eon-font-settings
  :type '(integer))

(defcustom eon-font-fixed eon-font-default
  "Optionally name a fixed-width font.
When `eon-font-default' is set to a fixed-width font,
the font specified here should have the same character width.
If not set explicitly, fall back to `eon-font-default'."
  :group 'eon-font-settings
  :type '(string))

(defcustom eon-font-fixed-alt eon-font-fixed
  "Optionally name an alternative fixed-width font.
It should have the same character width as `eon-font-fixed'.
If not set explicitly, fall back to `eon-font-fixed'."
  :group 'eon-font-settings
  :type '(string))

(defcustom eon-font-proportional nil
  "Name for the proportional font, used for text that isn't code.
If not set explicitly, choosen by Emacs according to your system's default."
  :group 'eon-font-settings
  :type '(string))

(defcustom eon-font-proportional-size eon-font-default-size
  "Size for the proportinal font.
You can specify an absolute size as an integer, or a relative size as a float.
Examples: 140 -> 14 pt / 0.9 -> 90% of `eon-font-default-size'.
If not set explicitly, fall back to `eon-font-default-size'."
  :group 'eon-font-settings
  :type '(number))

(defcustom eon-font-marginal-size 0.9
  "Size for the mode line, tab bar and the tab line.
You can specify an absolute size as an integer, or a relative size as a float.
Examples: 140 -> 14 pt / 0.9 -> 90% of `eon-font-default-size'.
If not set explicitly, fall back to 90% of `eon-font-default-size'."
  :group 'eon-font-settings
  :type '(number))

(defcustom eon-font-mode-line eon-font-default
  "Base font face for the mode-line.
If not set explicitly, fall back to `eon-font-default'."
  :group 'eon-font-settings
  :type '(string))

(defcustom eon-font-tab-bar eon-font-mode-line
  "Base font face used for the tab bar.
When not set explicitly, fall back to `eon-font-mode-line'."
  :group 'eon-font-settings
  :type '(string))

(defcustom eon-font-tab-line eon-font-tab-bar
  "Base font face for the tab line.
When not set explicitly, fall back to `eon-font-tab-bar'."
  :group 'eon-font-settings
  :type '(string))

;; TODO Refactor into a setter function for the custom variables
(defun eon-fonts ()
  "Set the font faces.
Per default, the function is called by the hooks:
`eon-theme-light-post-load-hook' - set font faces after loading the light theme;
`eon-theme-dark-post-load-hook' - set font faces after loading the dark theme."
  (interactive)
  ;; Default font
  (set-face-attribute 'default nil
                      :family eon-font-default
                      :weight 'normal
                      :width  'normal
                      :height eon-font-default-size)
  ;; Fixed-width font face
  (set-face-attribute 'fixed-pitch nil
                      :family eon-font-fixed
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  ;; Alternative fixed-width font face
  (set-face-attribute 'fixed-pitch-serif nil
                      :family eon-font-fixed-alt
                      :weight 'normal
                      :width  'normal
                      :height 1.0)
  ;; Proportional font face;
  ;; can be toggled for the current buffer via "M-x variable-pitch-mode"
  (set-face-attribute 'variable-pitch nil
                      :family eon-font-proportional
                      :weight 'normal
                      :width  'normal
                      :height eon-font-proportional-size)
  ;; Base font face for the active mode line
  (set-face-attribute 'mode-line nil
                      :family eon-font-mode-line
                      :weight 'normal
                      :width  'normal
                      :height eon-font-marginal-size)
  ;; Base font face for the inactive mode line
  (set-face-attribute 'mode-line-inactive nil
                      :family eon-font-mode-line
                      :weight 'normal
                      :width  'normal
                      :height eon-font-marginal-size)
  ;; Base font face for the tab bar
  (set-face-attribute 'tab-bar nil
                      :family eon-font-tab-bar
                      :weight 'normal
                      :width  'normal
                      :height eon-font-marginal-size)
  ;; Base font face for the tab line
  (set-face-attribute 'tab-line nil
                      :family eon-font-tab-line
                      :weight 'normal
                      :width  'normal
                      :height eon-font-marginal-size)
  ;; Don't extend the selection background past the end of the line
  (set-face-attribute 'region nil :extend nil))

;; _____________________________________________________________________________
;;; TOGGLE THEME

;; Default/fallback definitions – don't change them here,
;; but set them in your 'init.el'. For examples, see THEME CONFIG.

;; TODO Refactor in order to dissolve duplication
;; TODO Add setters to custom variables

(defgroup eon-theme nil
  "Set your light and dark themes."
  :group 'eon)

(defcustom eon-theme-light 'modus-operandi-tinted
  "The theme can be either a symbol, function symbol or lambda."
  :group 'eon-theme
  :type '(restricted-sexp
          :match-alternatives (functionp symbolp)))

(defcustom eon-theme-dark 'modus-vivendi-tinted
  "The theme can be either a symbol, function symbol or lambda."
  :group 'eon-theme
  :type '(restricted-sexp
          :match-alternatives (functionp symbolp)))

(defcustom eon-theme-variant-default 'light
  "Load either the light or the dark theme at startup?"
  :group 'eon-theme
  :type '(radio
          (const :tag "Dark" dark)
          (const :tag "Light" light)))

(defvar eon-theme--variant-active nil
  "Holds the information about the currently active theme variant.")

(defcustom eon-theme-light-pre-load-hook nil
  "Run before loading the light theme."
  :group 'eon-theme
  :type '(hook))

(defcustom eon-theme-light-post-load-hook nil
  "Run after loading the light theme."
  :group 'eon-theme
  :type '(hook))

(defcustom eon-theme-dark-pre-load-hook nil
  "Run before loading the dark theme."
  :group 'eon-theme
  :type '(hook))

(defcustom eon-theme-dark-post-load-hook nil
  "Run after loading the dark theme."
  :group 'eon-theme
  :type '(hook))

(defun eon-theme-load-light ()
  "Load the light theme and apply some modifications.
Some themes may come as functions -- wrap these ones in lambdas."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (run-hooks 'eon-theme-light-pre-load-hook)
  (cond ((symbolp eon-theme-light) (load-theme eon-theme-light t))
        ((functionp eon-theme-light) (funcall eon-theme-light)))
  (setq eon-theme--variant-active 'light)
  (run-hooks 'eon-theme-light-post-load-hook))

(defun eon-theme-load-dark ()
  "Load the dark theme and apply some modifications.
Some themes may come as functions -- wrap these ones in lambdas."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (run-hooks 'eon-theme-dark-pre-load-hook)
  (cond ((symbolp eon-theme-dark) (load-theme eon-theme-dark t))
        ((functionp eon-theme-dark) (funcall eon-theme-dark)))
  (setq eon-theme--variant-active 'dark)
  (run-hooks 'eon-theme-dark-post-load-hook))

(defun eon-theme-toggle ()
  "Toggle between light and dark theme."
  (interactive)
  (cond
   ((equal eon-theme--variant-active 'light) (eon-theme-load-dark))
   ((equal eon-theme--variant-active 'dark) (eon-theme-load-light))
   (t (mapc #'disable-theme custom-enabled-themes))))

(defun eon-theme-load-default ()
  "Load the default theme."
  (interactive)
  (cond
   ((equal eon-theme-variant-default 'light) (eon-theme-load-light))
   ((equal eon-theme-variant-default 'dark) (eon-theme-load-dark))
   (t (error
       "Theme: `eon-theme-variant-default' must be set to 'light or 'dark"))))

;; Keybinding to toggle between light and dark: "<leader> x t"
(keymap-set ctl-z-x-map "t" #'eon-theme-toggle)

;; _____________________________________________________________________________
;;; THEME CONFIG

;; Set some defaults for the Modus themes; doesn't affect other themes.
;; These variables must be set before loading the Modus themes.

;; Allow bold and italic fonts?
(setopt modus-themes-bold-constructs t
        modus-themes-italic-constructs nil
        modus-themes-mixed-fonts t)
;; Remove the border around the mode line
(setopt modus-themes-common-palette-overrides
        '((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)))

;; Customize via "M-x eon-customize-group" or via `setopt' in your init.el:

;;; - Set your light theme:
;; (setopt eon-theme-light 'modus-operandi)

;;; - Set your dark theme:
;; (setopt eon-theme-dark 'modus-vivendi)

;;; - Set 'light or 'dark as your default theme variant:
;; (setopt eon-theme-variant-default 'light)

;;; - Theme hooks

;; The hooks can be used to run additional functions before or after
;; loading the selected light and dark theme. That's useful for setting
;; variables that otherwise would get overwritten by themes.
;; Restart Emacs to take effect after changing the hooks.

;; Light theme hooks

;; Call a function before/after loading the light theme.
;; Example for commands ("interactive" functions):
;; (add-hook 'eon-theme-light-post-load-hook #'my-interactive-function)
;; Functions not designated as "(interactive)" must be wrapped in lambdas.

;; Load the default font set; if you want to load a different font set,
;; "unhook" `eon-fonts' first via:
;; (remove-hook 'eon-theme-dark-post-load-hook #'eon-fonts)
(add-hook 'eon-theme-light-post-load-hook #'eon-fonts)

;; Dark theme hooks

;; Call a function before/after loading the dark theme.
;; Example for commands ("interactive" functions):
;; (add-hook 'eon-theme-dark-post-load-hook #'my-interactive-function)
;; Functions not designated as "(interactive)" must be wrapped in lambdas.

;; Load the default font set; if you want to load your own font set,
;; "unhook" `eon-fonts' first via:
;; (remove-hook 'eon-theme-dark-post-load-hook #'eon-fonts)
(add-hook 'eon-theme-dark-post-load-hook #'eon-fonts)

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
        hscroll-margin 2
        hscroll-step 1)

;; Enable pixel-based scrolling?
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
;; If 'long, this is only done when the mode line is longer
;; than the current window width in columns.
(setopt mode-line-compact 'long)

;; Show the buffer size in the modeline?
(size-indication-mode 1)

;; Show the current line number along with column number in mode line?
;; This is nice if you think line numbers on the left margin are distracting.
;; The line number indicator turnes off if you enable
;; `display-line-numbers-mode' or `global-display-line-numbers-mode'.
(column-number-mode 1)
(line-number-mode 1)

;; _____________________________________________________________________________
;;; MINIBUFFER
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Minibuffer>

;; Recursive minibuffers
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Recursive-Edit>
;; Allow minibuffer commands while in the minibuffer!
;; There are two commands to get out of recursive minibuffers:
;; "C-M-c" `exit-recursive-edit' and "C-]" `abort-recursive-edit'.
(setopt enable-recursive-minibuffers t)
;; Show how deep you're in there?
(minibuffer-depth-indicate-mode 1)

;; Do not allow the cursor in the minibuffer prompt
(setopt minibuffer-prompt-properties
        (plist-put minibuffer-prompt-properties 'cursor-intangible t))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Prevent visual line wrapping in narrow frames
(add-hook 'minibuffer-setup-hook (lambda () (setq-local truncate-lines t)))

;; For mouse commands to ask questions, use a dialog box instead of minibuffer?
(setopt use-dialog-box nil)

;; Grow and shrink the minibuffer according to its lines of content?
;; If you experience too much jumping, set it to 'grow-only.
(setopt resize-mini-windows t)

;; Allow for shorter responses? Lets you type "y" for "yes" and "n" for "no"
(setopt read-answer-short t)
(setopt use-short-answers t)

;; _____________________________________________________________________________
;;; COMPLETION
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Icomplete>

;; There are many matching styles available, see `completion-styles-alist'
;; <https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html>
;; The order within the list determines their priority.
(setopt completion-styles '(basic partial-completion flex))
(setopt completion-category-defaults nil)
(setopt completion-category-overrides
        '((file (styles . (partial-completion basic)))))

;; Make TAB try completion when appropriate
(setopt tab-always-indent 'complete)
(setopt tab-first-completion 'word-or-paren-or-punct)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Dabbrev

(with-eval-after-load 'dabbrev
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Completions buffer

;; Allow the *Completions* buffer to pop up?
(setopt completion-auto-help 'always)

;; Display settings for the *Completions* buffer; only if not disabled above
(setopt
 ;; Show the help lines on top of the *Completions* buffer?
 completion-show-help nil
 ;; Cycle completion candidates instead?
 completion-cycle-threshold nil
 ;; Show docstrings for completion candidates?
 completions-detailed t
 ;; Automatically select the *Completions* buffer?
 completion-auto-select 'second-tab
 ;; Define the appearance of completions?
 completions-format 'one-column
 ;; Maximum height of the *Completions* buffer in lines?
 completions-max-height 12
 ;; Enable grouping of completion candidates?
 completions-group t
 ;; Prefer recent completions?
 completions-sort 'historical)

;; Prevent visual line wrapping in narrow frames
(add-hook 'completion-list-mode-hook (lambda () (setq-local truncate-lines t)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Icomplete

;; Make Icomplete snappy and tweak it further
(with-eval-after-load 'icomplete
  (setopt icomplete-compute-delay 0.01
          icomplete-delay-completions-threshold 256
          icomplete-show-matches-on-no-input t
          icomplete-hide-common-prefix nil))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Fido vertical mode

;; The default vertical minibuffer completion UI for Emacs ONBOARD

;; Prevent minibuffer window from jumping when the number of candidates changes
(add-hook 'icomplete-minibuffer-setup-hook
          (lambda () (setq-local resize-mini-windows 'grow-only)))

;; Let TAB accept the current candidates in Icomplete/Fido minibuffers
(with-eval-after-load 'icomplete
  (keymap-set icomplete-minibuffer-map "TAB" #'icomplete-force-complete)
  (keymap-set icomplete-minibuffer-map "<tab>" #'icomplete-force-complete)
  ;; Let "SPC" insert the literal space character instead of triggering
  ;; `minibuffer-complete-word'. This enables filtering for candidates who
  ;; contain whitespace and prevents the *Completions* buffer from popping up.
  (keymap-set icomplete-minibuffer-map "SPC" #'self-insert-command))

(fido-vertical-mode 1)

;; _____________________________________________________________________________
;;; HELP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Help>

;; Define local leader keymap for `help-mode'
(eon-localleader-defkeymap help-mode eon-localleader-help-map
  :doc "Local leader keymap for Help buffers."
  "s" #'help-find-source)

;; Make commonly used help commands available under the leader key
(keymap-set ctl-z-h-map "," `("..." . ,help-map))
(keymap-set ctl-z-h-map "e" #'view-echo-area-messages)
(keymap-set ctl-z-h-map "f" #'describe-function)
(keymap-set ctl-z-h-map "k" #'describe-key)
(keymap-set ctl-z-h-map "o" #'describe-symbol)
(keymap-set ctl-z-h-map "v" #'describe-variable)

;; Focus a help window when it appears?
(setopt help-window-select t)

;; Show all options when running `apropos' and friends (fulltext search)?
;; Keybinding: <leader> h a"
(setopt apropos-do-all t)
(keymap-set ctl-z-h-map "a" #'apropos)

;; _____________________________________________________________________________
;;; PACKAGE MANAGER UI SETTINGS

(when package-enable-at-startup

  ;; Open the package manager interface: "<leader> x p"
  (keymap-set ctl-z-x-map "p" #'list-packages)

  ;; Highlight current line in the package manager UI?
  (add-hook 'package-menu-mode-hook (lambda () (hl-line-mode 1))))

(with-eval-after-load 'package-vc
  (setopt package-vc-register-as-project nil))

;; _____________________________________________________________________________
;;; CUSTOMIZATION UI SETTINGS

;; The most important Emacs ONBOARD preferences are customizable via GUI.
;; Open the GUI via "<leader> x C"

;; Define local leader keymap for `Custom-mode'
(eon-localleader-defkeymap Custom-mode eon-localleader-customzation-map
  :doc "Local leader keymap for Customization buffers."
  ;; Pop up a  buffer to edit the settings in '.dir-locals.el'
  "d" #'customize-dirlocals)

(defun eon-customize-group ()
  "Set preferences via GUI."
  (interactive)
  (customize-group 'eon))

(keymap-set ctl-z-x-map "C" #'eon-customize-group)

;; Don't accumulate customization buffers?
(setopt custom-buffer-done-kill t)

;; No line wrapping in descriptions?
(add-hook 'Custom-mode-hook (lambda () (setq-local truncate-lines t)))

;; _____________________________________________________________________________
;;; ELDOC
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Lisp-Doc>
;; <https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc>

(setopt eldoc-minor-mode-string nil
        eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
        eldoc-echo-area-display-truncation-message nil
        eldoc-echo-area-prefer-doc-buffer nil)

;; _____________________________________________________________________________
;;; SEARCH
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Search>
;; <https://emacsredux.com/blog/2025/03/18/you-have-no-idea-how-powerful-isearch-is>

;;; - Isearch: "C-s" and "C-r"

;; When isearching, enable M-<, M->, C-v and M-v to skip between matches
;; in an intuitive fashion.
(setopt isearch-allow-motion t
        isearch-motion-changes-direction t)

;; Swap search functions to make regexp-search the default
;; (keymap-global-set "C-s"   #'isearch-forward-regexp)
;; (keymap-global-set "C-r"   #'isearch-backward-regexp)
;; (keymap-global-set "C-S-s" #'isearch-forward)
;; (keymap-global-set "C-S-r" #'isearch-backward)

;;; - Search and replace
;; If text is selected, then the commands act on that region only

;; The 'query-' variant asks for each replacement
;; Confirm with "SPC" / "y", or deny and jump to the next via "n"
(keymap-global-set      "M-%"   #'query-replace-regexp)
(keymap-set ctl-z-s-map "r"     #'query-replace-regexp)
(keymap-set ctl-z-s-map "R"     #'query-replace)

;; Replace all occurences immediately
(keymap-global-set      "C-M-%" #'replace-regexp-as-diff)
(keymap-set ctl-z-s-map "C-r"   #'replace-regexp-as-diff)

;; _____________________________________________________________________________
;;; IMENU
;; Imenu provides navigation for buffer content, e.g. code, outlines and more

(with-eval-after-load 'imenu
  (setopt imenu-auto-rescan t
          imenu-max-item-length 128))

(keymap-set ctl-z-g-map "i" #'imenu)

;; _____________________________________________________________________________
;;; FRAME MANAGEMENT

(keymap-set ctl-z-x-map "f" #'toggle-frame-maximized)
(keymap-set ctl-z-x-map "F" #'toggle-frame-fullscreen)

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

;; Make window splits more evenly sized?
(setopt window-resize-pixelwise t)

;; Undo/redo window layouts
(setopt winner-dont-bind-my-keys t)
(keymap-set ctl-z-w-map "u" #'winner-undo)
(keymap-set ctl-z-w-map "r" #'winner-redo)
(winner-mode 1)

;; Common window management commands under the leader key
(keymap-set ctl-z-w-map "b" #'display-buffer)
(keymap-set ctl-z-w-map "c" #'delete-window)
(keymap-set ctl-z-w-map "d" #'dired-other-window)
(keymap-set ctl-z-w-map "f" #'find-file-other-window)
(keymap-set ctl-z-w-map "k" #'kill-buffer-and-window)
(keymap-set ctl-z-w-map "m" #'delete-other-windows)
(keymap-set ctl-z-w-map "o" #'other-window-prefix)
(keymap-set ctl-z-w-map "q" #'quit-window)
(keymap-set ctl-z-w-map "s" #'split-window-below)
(keymap-set ctl-z-w-map "t" #'window-toggle-side-windows)
(keymap-set ctl-z-w-map "T" #'toggle-window-dedicated)
(keymap-set ctl-z-w-map "v" #'split-window-right)
(keymap-set ctl-z-w-map "w" #'other-window)

;; Default window navigation – simply switch to the next window in order.
;; Added for convenience; the default keybinding is "C-x o"
(keymap-global-set "M-o" #'other-window)

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

;; Run commands in a new tab
(keymap-set ctl-z-t-map "f" #'find-file-other-tab)
(keymap-set ctl-z-t-map "d" #'dired-other-tab)
(keymap-set ctl-z-t-map "b" #'switch-to-buffer-other-tab)

;; What to do with a window whose buffer was killed?
;; nil = no special handling. Let `set-window-configuration' decide,
;; instead of displaying a placeholder bufffer.
(setopt tab-bar-select-restore-windows nil)

;; _____________________________________________________________________________
;;; BUFFER MANAGEMENT
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Buffers>

;; Fast buffer switching
(keymap-set ctl-z-b-map "p" #'previous-buffer)
(keymap-set ctl-z-b-map "n" #'next-buffer)
(keymap-set ctl-z-b-map "b" #'switch-to-buffer)

;; Reload buffer; when visiting a file, discard all unsaved changes
(keymap-set ctl-z-b-map "C-r" #'revert-buffer)

;; Get the buffer out of the way, but keep it alive
(defun eon-bury-buffer (&optional restore)
  "Bury the current buffer.
If visiting a file and modified, ask to save first; then bury the buffer.
When prefix arg RESTORE is non-nil, bring the buffer back.
If called from the minibuffer, exit via `abort-recursive-edit'."
  (interactive "P")
  (if (minibufferp)
      (abort-recursive-edit)
    (if restore
        (unbury-buffer)
      (when (and buffer-file-name (buffer-modified-p))
        (when (y-or-n-p (format "Save buffer %s before bury? "
                                (buffer-name)))
          (basic-save-buffer)))
      (bury-buffer))))
(keymap-set ctl-z-map "k" #'eon-bury-buffer)

;; Get the buffer out of the way and close the window
(defun eon-bury-window (&optional restore)
  "Bury the current buffer.
If visiting a file and modified, ask to save first;
then bury the buffer and delete the window.
When prefix arg RESTORE is non-nil, restore the previous window configuration.
If called from the minibuffer, exit via `abort-recursive-edit'."
  (interactive "P")
  (if (minibufferp)
      (abort-recursive-edit)
    (if restore
        (progn
          (winner-undo)
          (unbury-buffer))
      (when (and buffer-file-name (buffer-modified-p))
        (when (y-or-n-p (format "Save buffer %s before bury? "
                                (buffer-name)))
          (basic-save-buffer)))
      (bury-buffer)
      (unless (one-window-p)
        (delete-window)))))
(keymap-set ctl-z-map "K" #'eon-bury-window)

;; Kill the current buffer immediately instead of presenting a selection
(keymap-set ctl-z-b-map "k" #'kill-current-buffer)

;; Kill the window too
(keymap-set ctl-z-b-map "K" #'kill-buffer-and-window)

;; Kill all buffers at once
(defun eon-kill-all-buffers ()
  "Close all buffers at once."
  (interactive)
  (save-some-buffers)
  (let ((kill-buffer-query-functions '()))
    (mapc #'kill-buffer (buffer-list))))
(keymap-set ctl-z-b-map "C-k" #'eon-kill-all-buffers)

;; Kill matching buffers - select via regexp
(keymap-set ctl-z-b-map "M-k" #'kill-matching-buffers)

;; Uniquify buffer names for buffers that would have identical names
(setopt uniquify-buffer-name-style 'forward)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Boring buffers

;; Define boring buffers globally, so they can be hidden. These buffers
;; remain accessible via Ibuffer: "C-x C-b" or "<leader> b i"

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
    "\\`\\*Bookmark List"  ; "<leader> RET l"
    "\\`\\*Ibuffer"        ; "<leader> b i"
    "\\`\\*Messages"       ; "<leader> h e"
    )
  "List of regexps matching buffers considered uninteresting.
These buffers may be skipped from navigation commands.
Use `eon-boring-buffers-add' to extend the list."
  :type '(repeat regexp)
  :group 'eon-misc)

(defun eon-boring-buffers-add (&optional regexp)
  "Add REGEXP (string or list of strings) to `eon-boring-buffers'.
Called without argument just syncs `eon-boring-buffers' to other places."
  (when regexp
    (eon-add-to-list 'eon-boring-buffers regexp))
  ;; Define other places where `eon-boring-buffers' are synced to:
  (eon-add-to-list* 'switch-to-prev-buffer-skip-regexp eon-boring-buffers)
  (eon-add-to-list* 'switch-to-next-buffer-skip-regexp eon-boring-buffers))

;; Hide boring buffers
(with-eval-after-load 'window (eon-boring-buffers-add))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Ibuffer, the buffer manager
;; <https://protesilaos.com/codelog/2020-04-02-emacs-intro-ibuffer/>

(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-set ctl-z-b-map "i" #'ibuffer)

;; _____________________________________________________________________________
;;; SCRATCH BUFFER

;; Set an initial major mode for the *scratch* buffer:

;; `lisp-interaction-mode' is the default mode for the scratch buffer
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Lisp-Interaction>

;; You can set the scratch buffer to `org-mode' or `text-mode',
;; which may be more useful for quick notes or writing.
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
;; If you want to install this 3rd-party package, set this in your `init.el'
;; and then restart Emacs:
;; (use-package xclip :ensure t)

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
(keymap-set ctl-z-f-map "M-w" #'eon-copy-file-path)

;; Copy & paste between Windows and Emacs running within WSL
;; (Windows Subsystem for Linux)

;; Copy "kill" text from an Emacs buffer for pasting it into a Windows app
(when (and (eon-wslp)
           (file-exists-p "/mnt/c/Windows/System32/clip.exe"))
  (defun eon-wsl-copy (start end)
    "Copy selected text into the Windows clipboard."
    (interactive "r")
    (let ((default-directory "/mnt/c/"))
      (shell-command-on-region start end "clip.exe")))
  (keymap-set ctl-z-map "C-w" #'eon-wsl-copy))

;; Paste "yank" text into Emacs buffer that has been copied from a Windows app
(when (and (eon-wslp)
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
;;; REGISTERS

;; Better register preview
(setopt register-use-preview t)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - General keybindings

;; View register content
(keymap-set ctl-z-r-map "v" #'view-register)

(defun eon-register-clear ()
  "Pick a register with the built-in preview and clear it.
Prompts using `register-read-with-preview', the same UI `view-register'
uses.  After selection, remove the entry from `register-alist'.
If the chosen register is empty, signal a user error instead of
pretending to clear it."
  (interactive)
  (let* ((reg (register-read-with-preview "Clear register: "))
         (val (get-register reg)))
    (unless val
      (user-error "Register %s is empty"
                  (single-key-description reg)))
    (set-register reg nil)
    (message "Cleared register %s" (single-key-description reg))))
(keymap-set ctl-z-r-map "c" #'eon-register-clear)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Insertable

;; Insert from register
(keymap-set ctl-z-r-map "i" #'insert-register)

;; Copy the region into register
(keymap-set ctl-z-r-map "r" #'copy-to-register)
;; Copy the region-rectangle into register
(keymap-set ctl-z-r-map "R" #'copy-rectangle-to-register)

;; Store number in register. Example: "C-u 23 <leader> r n"
(keymap-set ctl-z-r-map "n" #'number-to-register)
;; Increment register by number; behaves differently when register contains text
(keymap-set ctl-z-r-map "+" #'increment-register)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Jumpable

;; Jump to a "jumpable" register
(keymap-set ctl-z-r-map "j" #'jump-to-register)

;; Save the state of all frames in register
(keymap-set ctl-z-r-map "f" #'frameset-to-register)
;; Save the state of the selected frame's windows in register
(keymap-set ctl-z-r-map "w" #'window-configuration-to-register)
;; Store keyboard macro in register
(keymap-set ctl-z-r-map "k" #'kmacro-to-register)
;; Record the position of point and the current buffer in register
(keymap-set ctl-z-r-map "SPC" #'point-to-register)

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
;;; HISTORY

;; Which histories to save between Emacs sessions?
(eon-add-to-list* 'savehist-additional-variables
                  '(kill-ring  ; CAUTION, persists copied text - see below
                    register-alist
                    search-ring
                    regexp-search-ring
                    compile-command))

;; Enable `savehist-mode' after setting the variables
(savehist-mode 1)

;; Wipe Emacs' kill ring and quit via "<leader> q Q"
(defun eon-clear-kill-ring ()
  "Clear the kill ring; do not touch the system clipboard."
  (interactive)
  (setq kill-ring nil
        kill-ring-yank-pointer nil))

(defun eon-quit-clear-kill-ring ()
  "Clear kill ring (not the system clipboard), save buffers, then quit Emacs."
  (interactive)
  (eon-clear-kill-ring)
  (save-buffers-kill-terminal))
(keymap-set ctl-z-q-map "Q" #'eon-quit-clear-kill-ring)

;; History length for various histories
(setopt history-length 1024)
;; Delete duplicates from the command history?
(setopt history-delete-duplicates t)

;; Remember where the cursor was, the last time you visited that file?
(setopt save-place-limit 1024)
(save-place-mode 1)

;; _____________________________________________________________________________
;;; FILE MANAGEMENT

;; Open arbitrary file: "<leader> f f"
(keymap-set ctl-z-f-map "f" #'find-file)
;; Open another file that has common file base name: "<leader> f a"
(keymap-set ctl-z-f-map "a" #'find-alternate-file)
;; Open the file in another window: "<leader> f A"
(keymap-set ctl-z-f-map "A" #'find-alternate-file-other-window)

;; Open any resource under the cursor: "<leader> f p"
(keymap-set ctl-z-f-map "p" #'find-file-at-point)
;; Display a list of all resources mentioned in this buffer: "<leader> f P"
(keymap-set ctl-z-f-map "P" #'ffap-menu)

;; Open file in another window: "<leader> f o"
(keymap-set ctl-z-f-map "o" #'find-file-other-window)

;; Save buffer if modified: "<leader> f s"
(keymap-set ctl-z-f-map "s" #'save-buffer)
;; Save some modified file-visiting buffers, but ask first: "<leader> f S"
(keymap-set ctl-z-f-map "S" #'save-some-buffers)
;; Write buffer to file ("save file as ..."): "<leader> f w"
(keymap-set ctl-z-f-map "w" #'write-file)

;; Rename a file via "<leader> f r"
;; Reach the VC/Git aware renaming command via "<leader> v r"
(keymap-set ctl-z-f-map "r" #'rename-visited-file)
(keymap-set ctl-z-f-map "R" #'rename-file)

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

;; Set the default
(eon-trash-on)

;; Suppress moving remote files to the local trash when deleting?
(setopt remote-file-name-inhibit-delete-by-moving-to-trash t)

;; Resolve symlinks so that operations are conducted from the file's directory?
(setopt find-file-visit-truename t
        vc-follow-symlinks t)

;; Auto refresh buffers when contents change?
(setopt global-auto-revert-non-file-buffers t
        auto-revert-stop-on-user-input t
        auto-revert-verbose t)
(global-auto-revert-mode 1)

;; Configure Ediff to use a single frame and split windows horizontally
(setopt ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)

;; Emacs knows where your init file is (open and edit '.emacs' or 'init.el')
(defun eon-goto-init-file ()
  "Visit the init file."
  (interactive)
  (when user-init-file (find-file user-init-file)))
(keymap-set ctl-z-x-map "i" #'eon-goto-init-file)

;; _____________________________________________________________________________
;;; RECENT FILES

(setopt recentf-max-menu-items 10
        recentf-max-saved-items 256
        recentf-auto-cleanup 'mode)

;; Turn on recent file mode to visit recently edited files
(recentf-mode 1)

;; Ignore some recently visited files, eg. to prevent them from showing up
;; amongst recent files after package upgrades
(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

;; Select from recently opened files via "<leader> f r"
(keymap-set ctl-z-f-map "h" #'recentf-open)

;; _____________________________________________________________________________
;;; BACKUP FILES
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Backup>

;; ---> CAUTION: This mode makes copies of the files you are editing.
;; If you're editing files with sensitive data (e.g. on temporally mounted,
;; encrypted devices), either disable this mode or specify the location
;; via regexp where to save (or not to save) backup copies of these files.

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
;;; AUTO-SAVE FILES
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Auto-Save>

;; Enable auto-save to safeguard against data loss
;; Use `recover-file' or `recover-session' commands to restore auto-saved data

(setopt auto-save-default t
        auto-save-no-message t
        auto-save-include-big-deletions t
        kill-buffer-delete-auto-save-files t)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Location for auto-save files
;; Per default, auto-save files are stored alongside their original file.
;; Let's not clutter the file system with auto-save "#filename.ext#" files,
;; but place them into separate directories for remote and local files under
;; the Emacs init directory.

;; Ensure auto-save directories exist
(make-directory (expand-file-name "auto-save/" user-emacs-directory) t)

;; Directory for auto-save files accessed via Tramp (mostly remote, but also
;; local files accessed via Tramp, e.g. when sudo functionality used)
(setopt tramp-auto-save-directory
        (expand-file-name "auto-save/" user-emacs-directory))

;; Where the actual auto-saved files go
(setopt auto-save-file-name-transforms
        `(;; Tramp (remote) files; pattern: /method:host:/path/to/file
          ("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
           ,(concat (expand-file-name "auto-save/" user-emacs-directory)
                    "\\2")
           t)
          ;; Everything else (local files)
          (".*"
           ,(expand-file-name "auto-save/" user-emacs-directory)
           t)))

;; _____________________________________________________________________________
;;; LOCKFILES
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Interlocking>

;; Let Emacs keep track of files currently visited?
(setopt create-lockfiles nil)

;; _____________________________________________________________________________
;;; DIRED FILE MANAGER
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Dired>

;; Define localleader keymap for `dired-mode'
(eon-localleader-defkeymap dired-mode eon-localleader-dired-map
  :doc "Local leader keymap for Dired buffers.")

;; The `dired' keybinding is "C-x d". This new keybinding is in accordance
;; with "C-x C-f" for visiting files
(keymap-global-set "C-x C-d" #'dired)
;; Open directory of the currently visited file in Dired
(keymap-global-set "C-x d" #'dired-jump)

;; Open the Dired file manager via leader menu: "<leader> d"
(keymap-set ctl-z-map "d" #'dired)
;; Open directory of the currently visited file via leader menu: "<leader> f d"
(keymap-set ctl-z-f-map "d" #'dired-jump)

;; Keybindings in Dired
(with-eval-after-load 'dired
  ;; Switch to wdired-mode and edit directory content like a text buffer
  (keymap-set dired-mode-map "e" #'wdired-change-to-wdired-mode)
  ;; "f" opens file/directory; bring forward/backward pattern to Dired
  (keymap-set dired-mode-map "b" #'dired-up-directory))

(with-eval-after-load 'dired
  (setopt
   ;; Don't accumulate useless Dired buffers
   dired-kill-when-opening-new-dired-buffer t
   ;; Listing columns; Switch arguments with "C-u s" e.g. hide backups with -B
   dired-listing-switches "-lhFA -v --group-directories-first"
   ;; Copy files/directories with sub-directories?
   dired-recursive-copies 'always
   ;; Create directories if they don't exist?
   dired-create-destination-dirs 'ask
   ;; Register file renaming in underlying version control system?
   dired-vc-rename-file t
   ;; Mimic dual-pane file managers?
   dired-dwim-target t
   ;; Check for directory modifications?
   dired-auto-revert-buffer #'dired-directory-changed-p
   dired-do-revert-buffer #'dired-directory-changed-p))

;; Hide details in file listings? Toggle via "(" or "<localleader> d"
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(keymap-set eon-localleader-dired-map "d" #'dired-hide-details-mode)

;; Also omit dotfiles in listings? Toggle via "<localleader> o"
(with-eval-after-load 'dired-x
  (setopt dired-omit-files (concat dired-omit-files "\\|\\`\\.[^.].*")))
(keymap-set eon-localleader-dired-map "o" #'dired-omit-mode)

;; Highlight current line in Dired?
(add-hook 'dired-mode-hook #'hl-line-mode)

;; Images
(with-eval-after-load 'image-dired
  (setopt
   image-dired-thumb-margin 1
   image-dired-thumb-relief 0
   ;; Store thumbnails in the system-wide thumbnail location
   ;; e.g. ~/.local/cache/thumbnails to make them reusable by other programs
   image-dired-thumbnail-storage 'standard-large))

;; Open '~/.emacs.d' directory in Dired
(defun eon-goto-init-dir ()
  "Open the Emacs directory in Dired, which is usually '~/.emacs.d'."
  (interactive)
  (dired user-emacs-directory))

;; Open home directory in Dired
(defun eon-goto-home-dir ()
  "Open the user's home directory in `dired'."
  (interactive)
  (dired "~/"))
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "h" #'eon-goto-home-dir))

;; _____________________________________________________________________________
;;; COMINT

(setopt comint-input-ignoredups t
        comint-prompt-read-only t
        comint-buffer-maximum-size 65536)

;; _____________________________________________________________________________
;;; ESHELL
;; <https://www.gnu.org/software/emacs/manual/html_mono/eshell.html>
;; <https://www.masteringemacs.org/article/complete-guide-mastering-eshell>

;; Eshell is not a terminal emulator, but a shell equivalent to Bash or Fish
;; that runs within Emacs. It is independent from the OS. Eshell looks like
;; a POSIX shell superficially, but is also a REPL for Emacs Lisp expressions
;; - meaning you get the full Emacs power.
;;
;; Eshell scripts can also run in batch mode:
;; By adding the following interpreter directive to an Eshell script, you
;; can make it executable like other shell scripts:
;; #!/usr/bin/env -S emacs --batch -f eshell-batch-file
;;
;; Subshells that evaluate Emacs list are created with $( ... ) or ( ... ).
;; You can use $( ... ) to in-line an elisp form and use its output in much
;; the same way as you would in bash, e.g. echo $(+ 1 2 3).
;;
;; There's another one, which is more like an actual subshell: ${ ... }
;; but this is not as versatile as a regular subshell you may know from
;; bash and others.

;; Create Eshell loacal leader keymap
(eon-localleader-defkeymap eshell-mode eon-localleader-eshell-map
  :doc "Local leader keymap for `eshell-mode'.")

(setopt eshell-banner-message ""
        eshell-scroll-to-bottom-on-input 'this
        eshell-buffer-maximum-lines 65536
        eshell-history-size 1024
        eshell-history-append t
        eshell-hist-ignoredups t
        eshell-cmpl-ignore-case t
        eshell-list-files-after-cd t
        eshell-destroy-buffer-when-process-dies t)

;; Launch an Eshell buffer: "<leader> e e"; re-visit the buffer by repeating
(keymap-set ctl-z-e-map "e" #'eshell)

;; Use Outline commands with Eshell prompts/buffers
(add-hook 'eshell-mode-hook
          (lambda () (setq outline-regexp eshell-prompt-regexp)))

;; Launch a fresh Eshell buffer: "<leader> e E"
(defun eon-eshell-new ()
  "Open a new eshell instance."
  (interactive)
  (eshell 't))
(keymap-set ctl-z-e-map "E" #'eon-eshell-new)

;; _____________________________________________________________________________
;;; SHELL
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Shell-Mode>

;; This is also no terminal emulator, but a buffer to issue shell commands
;; and display their output

;; Create Shell local leader keymap
(eon-localleader-defkeymap shell-mode eon-localleader-shell-map
  :doc "Local leader keymap for `shell-mode'.")

;; Set another shell than your default one?
;; (setopt shell-file-name "/usr/bin/bash")

;; Make `shell' behave like `eshell'
(setopt shell-kill-buffer-on-exit t)

;; Launch a Shell buffer: "<leader> e s"; re-visit the buffer by repeating
(keymap-set ctl-z-e-map "s" #'shell)

;; Launch a fresh Shell buffer: "<leader> e S"
(defun eon-shell-new ()
  "Open a new shell instance."
  (interactive)
  (shell (generate-new-buffer-name "*shell*")))
(keymap-set ctl-z-e-map "S" #'eon-shell-new)

;; _____________________________________________________________________________
;;; TERMINAL EMULATOR

(with-eval-after-load 'term
  (setopt term-input-ignoredups t
          term-buffer-maximum-size 65536))

(keymap-set ctl-z-e-map "t" #'term)

(defcustom eon-term-kill-on-exit t
  "Kill `term' and `ansi-term' buffers when their process exits."
  :type 'boolean
  :group 'eon-misc)

(defun eon-term--kill-buffer-on-exit (proc _event)
  "Kill buffer of PROC when it exits."
  (when (and eon-term-kill-on-exit
             (memq (process-status proc) '(exit signal)))
    (let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun eon-term-init (&rest _ignore)
  "Initialize `term' and `ansi-term' buffers."
  (when (derived-mode-p 'term-mode)
    (eon-cursor-update)
    (let ((proc (get-buffer-process (current-buffer))))
      (when proc
        (set-process-sentinel proc
                              #'eon-term--kill-buffer-on-exit)))))

;; Trigger initialization
(advice-add 'term :after #'eon-term-init)
(advice-add 'ansi-term :after #'eon-term-init)

;; _____________________________________________________________________________
;;; PROCED

;; Show and manage OS processes, like the command line programs top and htop

(with-eval-after-load 'proced
  (setopt proced-auto-update-interval 2
          proced-auto-update-flag t
          proced-enable-color-flag t
          proced-descend t))

;; _____________________________________________________________________________
;;; PINENTRY

(setopt epg-pinentry-mode 'loopback)

;; _____________________________________________________________________________
;;; WEB BROWSERS

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;; - EWW, a built-in web browser
;; <https://www.gnu.org/software/emacs/manual/html_mono/eww.html#Top>

(setopt url-privacy-level '(email lastloc cookies))
(url-setup-privacy-info)

;; TODO Refactor to make the user-agent string customizable/selectable
(defun eon-user-agent (browser-name)
  "Accepts a symbol in order to return a pre-defined user-agent string.
BROWSER-NAME can be either \='safari-macos, \='safari-iphone, \='w3m or t -
which sets the default `eww' user-agent according to `url-privacy-level'."
  (pcase browser-name
    ('safari-macos
     (setopt url-user-agent
             "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/603.3.8 (KHTML, like Gecko) Version/11.0.1 Safari/603.3.8"))
    ('safari-iphone
     (setopt url-user-agent
             "Mozilla/5.0 (iPhone; CPU iPhone OS 18_2 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.2 Mobile/15E148 Safari/604.1"))
    ('w3m
     (setopt url-user-agent
             "w3m/0.5.3+git2020050"))
    (_
     (setopt url-user-agent 'default))))

;; Set the user agent for the internal web browser
(eon-user-agent 'safari-iphone)

;; Browse web with EWW, the internal web browser
(keymap-set ctl-z-g-map "w" #'browse-web)

(eon-localleader-defkeymap eww-mode eon-localleader-eww-map
  :doc "Local leader keymap for the Emacs Web Wowser"
  "e" #'eww-browse-with-external-browser
  "r" #'eww-readable)

;; _____________________________________________________________________________
;;; EMAIL
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Sending-Mail>

;; Send emails directly from Emacs using SMTP – example template

;; Should be defined first
;; (setopt user-mail-address "mail@example.org")

;; To avoid typing in the password for each email, specify SMTP account(s)
;; in '~/.authinfo.gpg'. Here's a content template for authinfo.gpg:
;; machine mail.example.org port 587 login myusername password mypassword

;; Emacs email variables
(with-eval-after-load 'smtpmail
  (setopt send-mail-function #'smtpmail-send-it
          smtpmail-smtp-server "localhost"
          smtpmail-stream-type 'starttls
          smtpmail-smtp-service 1025  ; default port: 587
          smtpmail-queue-dir "~/.mail/queued-mail/"
          smtpmail-smtp-user user-mail-address
          smtpmail-debug-info nil))

(setopt message-kill-buffer-on-exit t)

;; _____________________________________________________________________________
;;; CALENDAR
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Calendar_002fDiary>

(with-eval-after-load 'calendar
  (setopt calendar-date-style 'iso      ; Year-Month-Day
          calendar-week-start-day 1     ; Monday
          calendar-weekend-days '(6 0)  ; Saturday and Sunday
          ))

;; _____________________________________________________________________________
;;; EDITING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Basic>

;; UTF-8
(prefer-coding-system 'utf-8)

;; Set desired line length in characters.
;; Can be changed temporarily via `set-fill-column' "C-x f"
(setopt fill-column 80)

;; While a text selection is active, typing characters will replace
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

;; Define a keymap in order to group formatting commands
(defvar-keymap ctl-z-c-f-map :doc "Formatting"
               "a" #'align-regexp
               ;; Tipps: <https://susam.github.io/sorting-in-emacs.html>
               "s" #'sort-lines
               "S" #'sort-fields)
;; Hook the keymap into the "Code" sub-keymap under the leader
;; in order to make it available via "<leader> c f"
(keymap-set ctl-z-c-map "f" `("Format" . ,ctl-z-c-f-map))

;; _____________________________________________________________________________
;;; TEXT / PROSE

;; Sentences end with a single or double spaces?
(setopt sentence-end-double-space nil)

;; TODO Add Flyspell / Ispell presets here

;; _____________________________________________________________________________
;;; LINE NUMBERS
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Display-Custom>

;; Line numbers on or off? Toggle with "M-x display-line-numbers-mode",
;; or set it for all programming modes. Goto line via "M-g M-g"
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode -1)))

;; If line numbers are enabled, we don't need to show them in the mode line;
;; so only the column number still appears there.
(add-hook 'display-line-numbers-mode-hook
          (lambda ()
            (if display-line-numbers-mode
                (line-number-mode -1)
              (line-number-mode 1))))

;; _____________________________________________________________________________
;;; LINE WRAPPING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Line-Truncation>

;; No line wrapping: truncate long lines in programming modes by default.
(add-hook 'prog-mode-hook (lambda () (setq-local truncate-lines t)))

;; If you prefer to see all text within a window in programming modes, enable
;; visual line breaks, a.k.a soft wrapping
;; (add-hook 'prog-mode-hook (lambda () (visual-line-mode 1)))

;; Wrap lines at whitespace, rather than in the middle of a word
(setopt word-wrap t)

;; Visual line wrapping in text mode
(add-hook 'text-mode-hook #'visual-line-mode)

;; Toggle `visual-line-mode' via "<leader> x l"
(keymap-set ctl-z-x-map "l" #'visual-line-mode)

;; _____________________________________________________________________________
;;; FOLDING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Hideshow>

;; Code folding on or off? Show available commands: "M-x hs-"
;; (add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1)))

;; _____________________________________________________________________________
;;; INDENTATION
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Indentation>

(setopt indent-tabs-mode nil  ; don't use tabs but spaces
        tab-width 4)          ; set display width for tab characters

;; Delete the whole indentation instead spaces one-by-one via <backspace>?
;; (Possibly shadowed by 3rd-party packages like `smartparens-mode'
(setopt backward-delete-char-untabify-method 'hungry)

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
        show-paren-when-point-inside-paren nil
        show-paren-when-point-in-periphery t)

;; Auto-close parens, brackets and quotes?
(electric-pair-mode 1)

;; _____________________________________________________________________________
;;; WHITESPACE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Useless-Whitespace>

;; Indicate trailing whitespace in programming modes?
(add-hook 'prog-mode-hook (lambda () (setopt show-trailing-whitespace nil)))

;; Indicate trailing whitespace in "text" modes?
(add-hook 'text-mode-hook (lambda () (setopt show-trailing-whitespace nil)))

;; Cleanup trailing whitespace
(keymap-set ctl-z-c-map "w" #'whitespace-cleanup)

;; _____________________________________________________________________________
;;; SYNTAX CHECK / LINTER
;; <https://www.gnu.org/software/emacs/manual/html_mono/flymake.html>

;; There are various syntax-checkers coming with the built-in Flymake mode,
;; and additional checkers can be installed as 3rd-party packages via
;; "M-x package-install <RET> flymake-".

(with-eval-after-load 'flymake
  ;; Style the Flymake widget in the modeline
  (setopt flymake-mode-line-format
          '(" " "FlyM" flymake-mode-line-exception flymake-mode-line-counters))
  ;; Stop when first/last error is reached
  (setopt flymake-wrap-around nil))

(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-g E" #'flymake-show-project-diagnostics)
  (keymap-set flymake-mode-map "M-g e" #'flymake-show-buffer-diagnostics)
  (keymap-set flymake-mode-map "M-g n" #'flymake-goto-next-error)  ; default
  (keymap-set flymake-mode-map "M-g p" #'flymake-goto-prev-error))  ; default

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
;;; PROJECT MANAGEMENT
;; Setup for Emacs' built-in project management

;; Switch to current project buffers: "<leader> n"
(keymap-set ctl-z-map "SPC" #'project-switch-to-buffer)

;; "<leader> p" inherits all commands from the `project-prefix-map'
(set-keymap-parent ctl-z-p-map project-prefix-map)

;; Show all project keybindings in the selection?
(setopt project-switch-use-entire-map nil)

;; Show these instead?
(setopt project-switch-commands '((project-find-file   "File"   ?f)
                                  (project-find-dir    "Dired"  ?d)
                                  (project-find-regexp "Grep"   ?g)
                                  (project-vc-dir      "VC/Git" ?v)
                                  (project-eshell      "Eshell" ?e)
                                  (project-shell       "Shell"  ?s)))

;; _____________________________________________________________________________
;;; VERSION CONTROL / GIT
;; Setup for Emacs' built-in VC management

(keymap-set ctl-z-v-map "v" #'vc-dir)
(keymap-set ctl-z-v-map "V" #'project-vc-dir)
(keymap-set ctl-z-v-map "g" #'vc-git-grep)
(keymap-set ctl-z-v-map "r" #'vc-rename-file)
(keymap-set ctl-z-v-map "," `("..." . ,vc-prefix-map))

;; _____________________________________________________________________________
;;; CODE NAVIGATION AND LOOKUP

(keymap-set ctl-z-c-map "d" #'xref-find-definitions)
(keymap-set ctl-z-c-map "a" #'xref-find-apropos)
(keymap-set ctl-z-c-map "r" #'xref-find-references)
(keymap-set ctl-z-c-map "R" #'xref-find-references-and-replace)

;; _____________________________________________________________________________
;;; EGLOT LANGUAGE SERVER CLIENT (LSP)
;; <https://github.com/joaotavora/eglot/blob/master/MANUAL.md/>

(with-eval-after-load 'eglot
  ;; Shutdown language server after closing last file?
  (setopt eglot-autoshutdown t)
  ;; Allow edits without confirmation?
  (setopt eglot-confirm-server-initiated-edits nil)
  ;; Show code action indicators?
  (setopt eglot-code-action-indications nil)
  ;; Activate Eglot in cross-referenced non-project files?
  (setopt eglot-extend-to-xref t)
  ;; Common keybindings
  ;; TODO Don't set keybindings after load, but buffer-locally where Eglot is
  ;; active. If it works, maybe generalize that behavior for other modes.
  (keymap-set ctl-z-c-map   "R" #'eglot-rename)
  (keymap-set ctl-z-c-f-map "f" #'eglot-format)
  (keymap-set ctl-z-c-f-map "F" #'eglot-format-buffer)
  (keymap-set ctl-z-c-map   "c" #'eglot-code-actions))

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
Use this if you want to register SPECS, but not yet build/install them.

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
;;; ORG MODE
;; <https://orgmode.org/>
;; <https://orgmode.org/org.html>
;; Org provides functionality far beyond that of computational notebooks
;; such as Jupyter or R Markdown.

;; Create the local leader keymap
(eon-localleader-defkeymap org-mode eon-localleader-org-mode-map
  :doc "Local leader map for `org-mode'.")

;; Set a default location to look for Org files; but you can have
;; that directory anywhere you like
(setopt org-directory (expand-file-name "~/Documents/org/"))

(defun eon-goto-org-dir ()
  "Show the Org directory in Dired."
  (interactive)
  (dired org-directory))
;; Visit the `org-directory' in Dired via "<leader> o d"
(keymap-set ctl-z-o-map "d" #'eon-goto-org-dir)

;; Turn on visual word wrapping
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Visual-Line-Mode>
(add-hook 'org-mode-hook #'visual-line-mode)

;; Alignment of tags at the end of headlines
(setopt  org-auto-align-tags t
         org-tags-column 0)

;; Toggle indicator for headlines
(setopt org-ellipsis "▼")

;; Don't add leading indentation to code blocks, remove them during export
(setopt org-edit-src-content-indentation 0
        org-src-preserve-indentation nil)

;; Context-dependent action
(keymap-set eon-localleader-org-mode-map "c" #'org-ctrl-c-ctrl-c)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Org capture
;; <https://orgmode.org/org.html#Capture>

;; Capture a note via "<leader> o c"
(keymap-set ctl-z-o-map "c" #'org-capture)
;; Put newer notes on top of the file
(setopt org-reverse-note-order t)

;; Set a default target for storing notes
(setopt org-default-notes-file (concat org-directory "notes.org"))

(defun eon-goto-org-notes ()
  "Visit the Org notes file."
  (interactive)
  (find-file org-default-notes-file))
;; Visit the default notes file via "<leader> o o"
(keymap-set ctl-z-o-map "o" #'eon-goto-org-notes)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Org todo
;; <https://orgmode.org/org.html#TODO-Items>

;; Set some sensible default states for todo-items
(setopt org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Org agenda
;; <https://orgmode.org/org.html#Agenda-Views>

(setopt org-agenda-files (list org-directory))

;; Visit your Org agenda via "<leader> o a"
(keymap-set ctl-z-o-map "a" #'org-agenda)

(add-hook 'org-agenda-mode-hook
          (lambda ()
            ;; Highlight current line in Org agenda?
            (hl-line-mode 1)))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Org links
;; <https://orgmode.org/org.html#Hyperlinks>

;; Store a link via "<localleader> L"
(keymap-set eon-localleader-org-mode-map "L" #'org-store-link)

;; Insert a link into an Org file via "<localleader> l"
(keymap-set eon-localleader-org-mode-map "l" #'org-insert-link)

;; Toggle visible/hidden links via "<localleader> M-l"
(keymap-set eon-localleader-org-mode-map "M-l" #'org-toggle-link-display)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Org publish

;; Select a project to publish a project via "<leader> o p";
;; This can be used to generate and publish a static blog, ebooks, etc.
(keymap-set ctl-z-o-map "p" #'org-publish)
(keymap-set eon-localleader-org-mode-map "p" #'org-publish-current-project)

;; Speed up publishing by skipping files that haven't been changed
(setopt org-publish-list-skipped-files nil)

;; Where to place the directory containing the timestamps about changed files
(setopt org-publish-timestamp-directory
        (concat user-emacs-directory "org-timestamps/"))

(defun eon-org-publish-unchanged-files-toggle ()
  "Toggle whether to re-export Org files that haven't been changed."
  (interactive)
  (if org-publish-use-timestamps-flag
      (progn (setopt org-publish-use-timestamps-flag nil)
             (message "Re-export unchanged files"))
    (progn (setopt org-publish-use-timestamps-flag t)
           (message "Don't re-export unchanged files (default)"))))
(keymap-set ctl-z-o-map "C-p" #'eon-org-publish-unchanged-files-toggle)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Org export

;; HTML export
(setopt org-html-checkbox-type 'unicode
        org-html-prefer-user-labels t
        org-html-self-link-headlines t)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Org code blocks / literate programming
;; <https://orgmode.org/worg/org-contrib/babel/intro.html>

;; Activate code blocks via Babel languages
(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))

;; _____________________________________________________________________________
;;; LISP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Executing-Lisp>

;; Make local leader binding behave like "C-x C-e" when Edebug is active
(defun eon-eval-last-sexp (&optional arg)
  "Eval last sexp; use Edebug variant when active.
With prefix ARG, pass it through to the underlying command."
  (interactive "P")
  (if (and (bound-and-true-p edebug-active)
           (fboundp 'edebug-eval-last-sexp))
      (call-interactively #'edebug-eval-last-sexp)
    (call-interactively #'eval-last-sexp)))

;; Define local leader keymap for `emacs-lisp-mode'
(eon-localleader-defkeymap emacs-lisp-mode eon-localleader-elisp-map
  :doc "Local leader keymap for Emacs Lisp buffers."
  "d"   #'edebug-defun
  "e"   #'eon-eval-last-sexp
  "E"   #'pp-eval-last-sexp
  "h"   #'describe-symbol
  "l"   #'load-file
  "m"   #'pp-macroexpand-last-sexp
  "M"   #'emacs-lisp-macroexpand
  "x"   #'eval-defun
  "C-b" #'elisp-byte-compile-buffer
  "C-e" #'elisp-eval-region-or-buffer
  "C-f" #'elisp-byte-compile-file
  "C-n" #'emacs-lisp-native-compile)

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Lisp modes registry

;; Collection of known Lisp-related modes (might be incomplete).
;; Probably useful to pre-configure Lisp-related modes when `prog-mode-hook' or
;; targeting parent modes are insufficient.
;;
;; Comes with 2 functions that return a list which of these modes are installed:
;; `eon-lisp-src-modes' and `eon-lisp-repl-modes'
;; If called with the argument 'hook, both functions return ...-hook symbols.
;;
;; Example how to use it:
;; (mapc (lambda (mode) (add-hook mode #'eon-check-parens-mode))
;;       (eon-lisp-src-modes 'hook))

(defvar eon-lisp-src-modes-registry
  '(;; Built-in modes
    common-lisp-mode
    emacs-lisp-mode
    lisp-interaction-mode
    lisp-data-mode
    lisp-mode
    scheme-mode
    ;; 3rd-party packages
    clojure-mode
    clojurec-mode
    clojurescript-mode
    clojurex-mode
    clojure-ts-mode
    clojurescript-ts-mode
    clojurec-ts-mode
    dune-mode
    fennel-mode
    gerbil-mode
    janet-ts-mode
    lfe-mode
    racket-mode
    stumpwm-mode)
  "Registry of Lisp-related source modes.")

(defvar eon-lisp-repl-modes-registry
  '(;; Built-in modes
    eshell-mode
    inferior-emacs-lisp-mode
    inferior-lisp-mode
    inferior-scheme-mode
    ;; 3rd-party packages
    ajrepl-mode
    cider-repl-mode
    fennel-repl-mode
    geiser-repl-mode
    inf-clojure-mode
    inferior-lfe-mode
    monroe-mode
    racket-repl-mode
    scheme-interaction-mode
    slime-repl-mode
    sly-mrepl-mode)
  "Registry of Lisp-related REPL modes.")

(defun eon-lisp--modes-transform (modes switch)
  "Transform MODES according to SWITCH.
Calling SWITCH with \='hook returns corresponding ...-hook symbols."
  (pcase switch
    ('hook (mapcar (lambda (m)
                     (intern (concat (symbol-name m) "-hook")))
                   modes))
    (_ modes)))

(defun eon-lisp-src-modes (&optional switch)
  "Return installed Lisp-related source modes from the registry.
With SWITCH = \='hook, return ...-hook variables."
  (eon-lisp--modes-transform
   (seq-filter #'fboundp eon-lisp-src-modes-registry)
   switch))

(defun eon-lisp-repl-modes (&optional switch)
  "Return installed Lisp-related REPL modes from the registry.
With SWITCH = \='hook, return ...-hook variables."
  (eon-lisp--modes-transform
   (seq-filter #'fboundp eon-lisp-repl-modes-registry)
   switch))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Check-parens mode

;; Minor mode that prevents accidentally saving files with mismatched
;; parentheses and quotes.

(defun eon-check-parens--ask ()
  "Check parens; prompt to proceed on mismatch."
  (if (condition-case nil
          (progn (check-parens) t)
        (error nil))
      nil
    (if (y-or-n-p
         "Buffer contains unmatched parens or quotes. Save anyway? ")
        nil
      (user-error "OK, the file has not been saved"))))

(define-minor-mode eon-check-parens-mode
  "Ask before saving with mismatching parens or quotes.

When enabled, `check-parens' is run before saving. If mismatches
are found, ask whether to save anyway; answering no aborts the
save with a `user-error'."
  :group 'eon-misc
  :init-value t
  (if eon-check-parens-mode
      (add-hook 'write-contents-functions
                #'eon-check-parens--ask
                nil t)
    (remove-hook 'write-contents-functions
                 #'eon-check-parens--ask
                 t)))

;; Enable minor mode per default; toggle via "M-x eon-check-parens-mode".
;; How to remove the hook permanently from a specific lisp major mode:
;; (remove-hook 'emacs-lisp-mode-hook #'eon-check-parens-mode)
(mapc (lambda (mode) (add-hook mode #'eon-check-parens-mode))
      (eon-lisp-src-modes 'hook))

;; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Further settings

;; Enable Flymake for Emacs Lisp
(defun eon-elisp-flymake-maybe ()
  "Conditionally enable `flymake-mode' for Emacs Lisp.
Don't enable in:
- buffers without a file, e.g. created by `pp-eval-last-sexp';
- `lisp-interaction-mode', e.g. the *scratch* buffer."
  (when (and buffer-file-name
             (not (derived-mode-p 'lisp-interaction-mode)))
    (flymake-mode 1)))

(add-hook 'emacs-lisp-mode-hook #'eon-elisp-flymake-maybe)

;; Emacs Lisp evaluation: don't truncate printed lists
(setopt eval-expression-print-length nil
        eval-expression-print-level nil)

;; Reach eval-expression via "<leader> e x"
(keymap-set ctl-z-e-map "x" #'eval-expression)

;; Load an Emacs Lisp file (executes the code)
(keymap-set ctl-z-f-map "l" #'load-file)

;; Additional keybinding resembling other sexp-related keybindings
(keymap-global-set "C-M-DEL" #'backward-kill-sexp)

;; _____________________________________________________________________________
;;; SERVER
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Emacs-Server>
;; ... or do "M-x info-emacs-manual s server RET" to read it within Emacs

;; Display the name of the Emacs server process in the frame title
;; to see easily to which server process a client is connected to.

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
              (eon-frame-title))))

(defun eon-server-stop ()
  "Save buffers, quit and shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))
(keymap-set ctl-z-q-map "s" #'eon-server-stop)

;; Start the server?
(unless (daemonp)
  (add-hook 'after-init-hook #'server-start))

;; _____________________________________________________________________________
(provide 'eon)
;;; eon.el ends here
