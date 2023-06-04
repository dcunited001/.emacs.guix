;; -*- lexical-binding: t; -*-
;;; dc-popup.el --- Description
;;
;; Copyright © 2023 David Conner
;; Copyright © 2014-2022 Henrik Lissner.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;;; Commentary:
;;
;;; Code:

;;* Popups

(require 'a)

;; TODO: setup shackle and set popper-display-control nil

(defun dc/popper-shell-output-empty-p (buf)
  (and (string-match-p "\\*Async Shell Command\\*" (buffer-name buf))
       (= (buffer-size buf) 0)))

(defun dc/popper-fit-window-height (win)
  (fit-window-to-buffer
   win
   (floor (frame-height) 3)             ; max-height (default divisor: 3)
   (floor (frame-height) 5)             ; min-height (default divisor: 6)
   ))

;; TODO: handle other popups for docker?
(setq dc/popper-rx-docker
      (rx (and line-start "*docker-"
               (or "containers" "images" "networks" "volumes") "*")))

(setq popper-reference-buffers
      '(("Output\\*$" . hide)
        ;; ("\\*Messages\\*" . hide)
        "\\*Messages\\*"
        "^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\)"
        "^\\*eshell.*\\*$" eshell-mode  ;eshell as a popup
        "^\\*shell.*\\*$"  shell-mode   ;shell as a popup
        "^\\*term.*\\*$"   term-mode    ;term as a popup
        "^\\*vterm.*\\*$"  vterm-mode   ;vterm as a popup
        dc/popper-rx-docker
        docker-image-mode
        docker-container-mode
        docker-volume-mode
        docker-network-mode
        "^\\*envrc\\*"
        "^\\*Customize"
        "^ \\*undo-tree\\*"
        "^\\*\\([Hh]elp\\|Apropos\\)"
        "^\\*Warnings"
        "^\\*Backtrace"
        "^\\*Process List\\*"
        "^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\)\\*"
        "^\\*\\(?:Occur\\|unsent mail.*?\\|message\\)\\*"
        "^\\*Flycheck errors\\*"
        "^\\*Flycheck errors\\*"
        "^\\*vc-diff"
        ;; NOTE: what are outline, annotation.el? can't get outline to work.
        ;; i've used outline as minor mode (org inherits from it, lispy uses it)
        "^\\*Outline*"
        "^\\*Edit Annotation "
        "\\(?:^\\*Contents\\|'s annots\\*$\\)"
        "^\\*[gG]eiser \\(dbg\\|xref\\|messages\\)\\*$"
        "^\\*Geiser documentation\\*$"
        "^\\* [A-Za-z0-9_-]+ REPL \\*"
        "^\\*sly-mrepl"
        "^\\*sly-traces"
        "^\\*sly-description"
        "^\\*sly-\\(?:db\\|inspector\\)"
        "^\\*cider-error*"
        "^\\*cider-repl"
        "^\\*cider-repl-history"
        "^\\*Org Links"
        "^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org Export Dispatcher\\)"
        "^\\*Org Agenda"
        "^\\*Org Src"
        "^\\*Org-Babel"
        "^\\*Capture\\*$\\|CAPTURE-.*$"
        "\\*ob-ipython.*"
        "^ \\*Python"
        " output\\*$"
        "^\\*TeX \\(?:Help\\|errors\\)"
        "^\\*Ledger Report"
        "^\\*Ledger Error"))

(add-to-list 'popper-reference-buffers
             '(dc/popper-shell-output-empty-p . hide))

;; (require 'dc-popper-popup-rules)
(setup (:pkg popper)
  (:option popper-group-function #'popper-group-by-project
           popper-display-control t
           popper-window-height #'dc/popper-fit-window-height)
  (popper-mode))

(provide 'dc-popup)
