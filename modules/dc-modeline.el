;;; dc-modeline.el --- Code for my custom mode line -*- lexical-binding: t -*-

;; Copyright (C) 2023  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

;; https://en.wikipedia.org/wiki/Code_page_437#Character_set

;; (min-width 10.0) is specified in (:propertize)
;; "║%l↔%c║" ;; "|%l↓%c→|" " 
(setq mode-line-position-column-line-format '("↓%l →%c ║ ")
      mode-line-compact nil
      mode-line-percent-position nil)

;; TODO: header-line-format?
;; (setq-default
;;  header-line-format
;;  '(mode-line-buffer-identification
;;    "║"
;;    mode-line-misc-info
;;    mode-line-modes))

(setq-default
 header-line-format
 '("%e"
   mode-line-front-space
   (:propertize (""
                 mode-line-mule-info
                 mode-line-client
                 mode-line-modified
                 mode-line-remote)
                display
                (min-width (5.0)))
   mode-line-frame-identification
   "║ "
   mode-line-buffer-identification
   " ║"
   (vc-mode vc-mode)
   mode-line-end-spaces))

(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-position
   mode-line-misc-info
   mode-line-modes
   mode-line-end-spaces))

;; TODO: ensure this doesn't become incompatible with what's underneath
(defun eglot--mode-line-format ()
  "Compose Eglot's mode-line."
  (let* ((server (eglot-current-server))
         ;; NOTE: this just limits the server nick to 7 chars
         (nick (and server (substring (eglot-project-nickname server) 0 6)))
         (pending (and server (hash-table-count
                               (jsonrpc--request-continuations server))))
         (last-error (and server (jsonrpc-last-error server))))
    (append
     `(,(propertize
         eglot-menu-string
         'face 'eglot-mode-line
         'mouse-face 'mode-line-highlight
         'help-echo "Eglot: Emacs LSP client\nmouse-1: Display minor mode menu"
         'keymap (let ((map (make-sparse-keymap)))
                   (define-key map [mode-line down-mouse-1] eglot-menu)
                   map)))
     (when nick
       `(":"
         ,(propertize
           nick
           'face 'eglot-mode-line
           'mouse-face 'mode-line-highlight
           'help-echo (format "Project '%s'\nmouse-1: LSP server control menu" nick)
           'keymap (let ((map (make-sparse-keymap)))
                     (define-key map [mode-line down-mouse-1] eglot-server-menu)
                     map))
         ,@(when last-error
             `("/" ,(eglot--mode-line-props
                     "error" 'compilation-mode-line-fail
                     '((mouse-3 eglot-clear-status  "Clear this status"))
                     (format "An error occurred: %s\n" (plist-get last-error
                                                                  :message)))))
         ,@(when (cl-plusp pending)
             `("/" ,(eglot--mode-line-props
                     (format "%d" pending) 'warning
                     '((mouse-3 eglot-forget-pending-continuations
                                "Forget pending continuations"))
                     "Number of outgoing, \
still unanswered LSP requests to the server\n")))
         ,@(cl-loop for pr hash-values of (eglot--progress-reporters server)
                    when (eq (car pr)  'eglot--mode-line-reporter)
                    append `("/" ,(eglot--mode-line-props
                                   (format "%s%%%%" (or (nth 4 pr) "?"))
                                   'eglot-mode-line
                                   nil
                                   (format "(%s) %s %s" (nth 1 pr)
                                           (nth 2 pr) (nth 3 pr))))))))))

(provide 'dc-modeline)
