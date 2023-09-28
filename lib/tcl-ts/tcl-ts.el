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

;;*** Keyword lists

(defvar tcl-ts-proc-list
  tcl-proc-list
  "")
(defvar tcl-ts-proc-regexp
  tcl-proc-regexp
  "")
(defvar tcl-ts-typeword-list
  tcl-typeword-list
  "")
(defvar tcl-ts-keyword-list
  tcl-keyword-list
  "")
(defvar tcl-ts-keyword-list
  tcl-keyword-list
  "")
(defvar tcl-ts-builtin-list
  tcl-builtin-list
  "")

;;**** Keyword Regexps

;; clojure-ts-mode uses defconst for these and runs (eval-and-compile...)
;;
;; for TCL, i think ideally, you could modify the builtin/etc lists either for a
;; mode that extends from it or to append to the builtin keywords
(defvar tcl-ts--proc-list-regexp
  (rx-to-string `(seq bol (or ,@tcl-ts-proc-list) eol)))

(defvar tcl-ts--keyword-list-regexp
  ;; (concat "^" (regexp-opt tcl-ts-keyword-list) "$")
  (rx-to-string `(seq bol (or ,@tcl-ts-keyword-list) eol)))

(defvar tcl-ts--builtin-list-regexp
  ;; (concat "^" (regexp-opt tcl-ts-builtin-list) "$")
  (rx-to-string `(seq bol (or ,@tcl-ts-builtin-list) eol)))


;;*** Grammar Setup

(defun tcl-ts--ensure-grammars ()
  "Stub to install language grammars required for TCL."
  (when tcl-ts-ensure-grammars
    (user-error "tcl-ts--ensure-grammars is not implemented.")))

;;** Parsing

;;*** Font Lock

;; TODO variable assignment: command/braced, where first word is set. how 2 query?

;; TODO fix 'command-name for braced_word (only match words after "\n"

;; - this is a parser limitation (it might be difficult bc of recursion & types
;;   or something)

;; TODO: number_word matches hex strings incorrectly

;; TODO: proc {} {  ... } blocks are not interpreted correctly


(defvar tcl-ts-font-lock-rules


  "Font Lock Rules for TCL TS")

(defun tcl-ts--font-lock-settings ()
  "fdsa"
  (warn "tcl-ts-mode: font-lock-settings is not implemented"))

(defun tcl-ts-font-lock-rules-for (tcl-lang)
  "Generates the font-lock queries for `tcl-lang'."
  (append
   (treesit-font-lock-rules
    :language tcl-lang
    :feature 'comment
    '((comment) @font-lock-comment-face)

    :language tcl-lang
    :feature 'variable
    '((variable_substitution) @font-lock-variable-name-face)

    :language tcl-lang
    :feature 'quoted-word
    '((quoted_word) @font-lock-string-face)

    :language tcl-lang
    :feature 'escape-sequence
    :override t
    '((escape_sequence) @font-lock-escape-face)

    :language tcl-lang
    :feature 'command-name
    `(((command :anchor ((word) @font-lock-function-name-face))
       ;; (:match ,tcl-ts--builtin-list-regexp @font-lock-function-name-face)
       )
      )

    ;; adding this to command-name results in every word being
    ;;
    ;; only the first word on each line should be highlighted.
    ;;
    ;; ((braced_word :anchor ((word) @font-lock-function-name-face))
    ;;  ;; (:match ,tcl-ts--builtin-list-regexp @font-lock-function-name-face)
    ;;  )

    :language tcl-lang
    :feature 'builtin
    :override t
    `(((command :anchor ((word) @font-lock-builtin-face))
       (:match ,tcl-ts--builtin-list-regexp @font-lock-builtin-face))
      ((braced_word :anchor ((word) @font-lock-builtin-face))
       (:match ,tcl-ts--builtin-list-regexp @font-lock-builtin-face)))

    :language tcl-lang
    :feature 'keyword
    :override t
    `(((command :anchor ((word) @font-lock-keyword-face))
       (:match ,tcl-ts--keyword-list-regexp @font-lock-keyword-face))))

   (when (eq tcl-lang 'tclsh)
     (treesit-font-lock-rules
      :language tcl-lang
      :feature 'set-command
      :override t
      '(((set_command "set" @font-lock-builtin-face))
        ((set_command name: (word) @font-lock-variable-name-face))
        ;; ((set_command value: (word) @font-lock-variable-name-face))
        )
      ;; ((set_command @font-lock-builtin-face))

      :language tcl-lang
      :feature 'if-command
      :override t
      '(((if_clause "if" @font-lock-builtin-face))
        ((if_clause "then" @font-lock-builtin-face))
        ;; elseif doesn't do "then"
        ;; ((elseif_clause "then" @font-lock-builtin-face))
        ((elseif_clause "elseif" @font-lock-builtin-face))
        ((else_clause "else" @font-lock-builtin-face)))

      :language tcl-lang
      :feature 'number
      :override t
      '((number_word) @font-lock-constant-face)
      ;; looks better than @font-lock-number-face

      :language tcl-lang
      :feature 'boolean
      :override t
      '((boolean_word) @font-lock-constant-face)))))

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
  (setq-local comment-end ""))

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

(define-derived-mode tcl-ts-mode prog-mode "tcl[TS]"
  "Major mode for TCL, powered by tree-sitter."
  :syntax-table tcl-ts-mode-syntax-table
  (tcl-ts--ensure-grammars)
  (when (treesit-ready-p 'tcl)
    (treesit-parser-create 'tcl)
    (tcl-ts-mode-variables)

    (setq-local treesit-font-lock-settings
                (tcl-ts-font-lock-rules-for 'tcl))
    (setq-local treesit-font-lock-feature-list
                '((comment variable quoted-word)
                  (builtin keyword command-name escape-sequence)))

    (when tcl-ts--debug
      (setq-local treesit--indent-verbose t)
      (when (eq tcl-ts--debug 'font-lock)
        (setq-local treesit--font-lock-verbose t))
      (treesit-inspect-mode))
    (treesit-major-mode-setup))

  ;; req. #'tcl-outline-level
  ;; (setq-local outline-regexp ".")
  ;; (setq-local outline-level 'tcl-outline-level)

  ;; (setq-local treesit-defun-prefer-top-level t)
  ;; (setq-local treesit-defun-tactic 'top-level)

  )

(define-derived-mode tclsh-ts-mode prog-mode "tclsh[TS]"
  "Major mode for TCL, powered by tree-sitter."
  :syntax-table tcl-ts-mode-syntax-table
  (tcl-ts--ensure-grammars)
  (when (treesit-ready-p 'tclsh)
    (treesit-parser-create 'tclsh)
    (tcl-ts-mode-variables)

    (setq-local treesit-font-lock-settings
                (tcl-ts-font-lock-rules-for 'tclsh))
    (setq-local treesit-font-lock-feature-list
                '((comment variable quoted-word number boolean)
                  (builtin keyword set-command if-command
                           command-name escape-sequence)))

    (when tcl-ts--debug
      (setq-local treesit--indent-verbose t)
      (when (eq tcl-ts--debug 'font-lock)
        (setq-local treesit--font-lock-verbose t))
      (treesit-inspect-mode))
    (treesit-major-mode-setup)))

;; https://www.tcl.tk/man/tcl/UserCmd/tclsh.html
;;
;; default to tclsh for now
(add-to-list 'major-mode-remap-alist
             '(tcl-mode . tclsh-ts-mode))

(provide 'tcl-ts)

;;; tcl-ts-mode.el ends here
