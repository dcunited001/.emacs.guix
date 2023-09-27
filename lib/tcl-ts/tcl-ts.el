;;; tcl-ts.el --- A tree-sitter mode for TCL         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  David Conner

;; Author: David Conner <aionfork@gmail.com>
;; Authors: David Conner <aionfork@gmail.com>
;; Maintainer: David Conner <aionfork@gmail.com>
;; URL: http://github.com/dcunited001/.emacs.g/blob/master/lib/tcl-ts
;; Keywords: languages tcl
;; Version: 0.0.1
;; Package-Requires: ((emacs "29"))


;;; License:
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

;;* tcl-ts
(require 'treesit)
(require 'tcl)

;; based on:
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/tcl.el
;; https://github.com/clojure-emacs/clojure-ts-mode/blob/main/clojure-ts-mode.el

;;** Customizations

(defgroup tcl-ts nil
  "Major mode for TCL, powered by tree-sitter."
  :prefix "tcl-ts-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/dcunited001/.emacs.g/blob/master/lib/tcl-ts.el")
  :link '(emacs-commentary-link :tag "Commentary" "tcl-ts-mode"))

(defcustom tcl-ts-ensure-grammars nil
  "When non-nill, ensure required tree-sitter grammars are installed."
  :safe #'booleanp
  :type 'boolean
  :package-version '(tcl-ts-mode . "0.0.1"))

(defvar tcl-ts--debug nil
  "Enables debugging messages, shows current node in mode-line. Only
intended for use during development.")

;;** Meta

(defconst tcl-ts-mode-version
  (eval-when-compile
    (lm-version (or load-file-name buffer-file-name)))
  "The current version of `tcl-ts-mode'.")

;;** Language

;;*** Syntax Table

;; just set the mode to inherit from 'tcl-mode where possible
(defvar tcl-ts-mode-syntax-table
  tcl-mode-syntax-table
  ;; (let ((table (make-syntax-table)))
  ;;   ;; initialize ASCII charset as symbol syntax
  ;;   (modify-syntax-entry '(0 . 127) "_" table))
  "Syntax table for `tcl-ts-mode'. Set to the same syntax table as
`tcl-mode' for now.")

;;*** Grammar Setup

(defun tcl-ts--ensure-grammars ()
  "Stub to install language grammars required for TCL."
  (when tcl-ts-ensure-grammars
    (user-error "tcl-ts--ensure-grammars is not implemented.")))

;;** Parsing

;;*** Font Lock

(defun tcl-ts--font-lock-settings ()
  "fdsa"
  (warn "tcl-ts-mode: font-lock-settings is not implemented"))

;;*** Indentation

;; customs in tcl-mode
;; tcl-indent-level
;; tcl-continued-indent-level

;; functions in tcl-mode
;; tcl-comment-indent
;; tcl-calculate-indent
;; tcl-indent-line
;; tcl-indent-line
;; tcl-indent-exp

(defun tcl-ts--indent-rules ()
  "Return a list of indentation rules for `treesit-simple-indent-rules'."
  `((tcl
     ((parent-is "source") parent-bol 0))))

;;** tcl-ts-mode

;;*** Mode Line

;;*** Setup

(defun tcl-ts-mode-variables ()
  "Initialize buffer-local variables for tcl-ts-mode"
  (setq-local comment-start "# ")
  (setq-local comment-start-skip
              "\\(\\(^\\|[;{[]\\)\\s-*\\)#+ *")
  (setq-local comment-end "")
  )

;;*** Map

(defvar tcl-ts-mode-map
  (let ((map (make-sparse-keymap)))
    ;; potentially inherit from other keymaps
    map))

;; (defvar tcl-dialect-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map tcl-ts-mode-map)
;;     map))

;;*** Mode


(defun tcl-ts-mode-display-version ()
  "Display the current `tcl-mode-version' in the minibuffer."
  (interactive)
  (message "tcl-ts-mode (version %s)" tcl-ts-mode-version))


(defvar outline-regexp)
(defvar outline-level)

;; treesit-major-mode-setup

;; "Activate tree-sitter to power major-mode features.

;; If `treesit-font-lock-settings' is non-nil, set up fontification
;; and enable `font-lock-mode'.

;; If `treesit-simple-indent-rules' is non-nil, set up indentation.

;; If `treesit-defun-type-regexp' is non-nil, set up
;; `beginning-of-defun-function' and `end-of-defun-function'.

;; If `treesit-defun-name-function' is non-nil, set up
;; `add-log-current-defun'.

;; If `treesit-simple-imenu-settings' is non-nil, set up Imenu.

;; Make sure necessary parsers are created for the current buffer
;; before calling this function.

(define-derived-mode tcl-ts-mode prog-mode "TCL[TS]"
  "Major mode for TCL, powered by tree-sitter."
  :syntax-table tcl-ts-mode-syntax-table
  (tcl-ts--ensure-grammars)
  (when (treesit-ready-p 'tcl)
    (treesit-parser-create 'tcl)
    (tcl-ts-mode-variables)
    (when tcl-ts--debug
      (setq-local treesit--indent-verbose t)
      (when (eq tcl-ts--debug 'font-lock)
        (setq-local treesit--font-lock-verbose))
      (treesit-inspect-mode))
    (treesit-major-mode-setup))

  ;; req. #'tcl-outline-level
  ;; (setq-local outline-regexp ".")
  ;; (setq-local outline-level 'tcl-outline-level)

  ;; (setq-local treesit-defun-prefer-top-level t)
  ;; (setq-local treesit-defun-tactic 'top-level)

  )

(add-to-list 'major-mode-remap-alist '(tcl-mode . tcl-ts-mode))

(provide 'tcl-ts)

;;; tcl-ts-mode.el ends here
