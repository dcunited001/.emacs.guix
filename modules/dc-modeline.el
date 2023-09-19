;;; dc-modeline.el -*- lexical-binding: t -*-
;;
;; Copyright © 2023 David Conner
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

;;* Modeline

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

;; ** Headerline

;; NOTE: most of the mouse click events don't work in header-line-format
(setq-default
 header-line-format
 '("║ %e"
   mode-line-front-space
   mode-line-position
   mode-line-buffer-identification
   " ║ "
   mode-line-frame-identification
   " ║ "
   (vc-mode vc-mode)
   mode-line-end-spaces))

(setq-default
 mode-line-format
 '("║ %e"
   mode-line-front-space
   (:propertize (""
                 mode-line-mule-info
                 mode-line-client
                 mode-line-modified
                 mode-line-remote)
                display
                (min-width (5.0)))
   " ║ "
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
