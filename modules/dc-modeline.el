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

;;** Minions

;; '("╒╛" . "╒╛")
(setup (:pkg minions)
  (:with-hook window-setup-hook
    (:hook minions-mode))
  (:option minions-mode-line-lighter "⌠≡⌡"
           minions-mode-line-delimiters '("╔╦╗ " . "")))

;; To list a mode even though the defining library has not been loaded yet, you
;; must add it to minor-mode-list yourself. Additionally it must be
;; autoloaded. For example:
(defun dc/minions-push (modesym)
  "Add a placeholder for `modesym' in "
  (when (autoloadp (symbol-function sym))
    (cl-pushnew sym minor-mode-list)))

;;** Icons

;; TODO: https://github.com/domtronn/all-the-icons.el/wiki/Mode-Line

;;** Modeline

;; (min-width 10.0) is specified in (:propertize)
;; "║%l↔%c║"
;; "|%l↓%c→|"
;; "↓%l →%c "
(setq mode-line-position-column-line-format '("│%l ► %c│")
      mode-line-compact nil
      mode-line-percent-position nil)

;; TODO: header-line-format?
;; (setq-default
;;  header-line-format
;;  '(mode-line-buffer-identification
;;    "║"
;;    mode-line-misc-info
;;    mode-line-modes))

(setq
 mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-remote
   "│"
   mode-line-client
   "║"
   mode-line-mule-info
   mode-line-position
   mode-line-frame-identification
   mode-line-buffer-identification
   " │"

   (:propertize (""
                 mode-line-modified)
                display
                (min-width (5.0)))
   "║"
   (vc-mode vc-mode)
   " "
   mode-line-misc-info
   minions-mode-line-modes
   mode-line-end-spaces))

;; ** Headerline
;; NOTE: most of the mouse click events don't work in header-line-format

;; TODO: fix minor-mode-alist, so i can put the file name back in the modeline


;; TODO: ensure this doesn't become incompatible with what's underneath

(provide 'dc-modeline)
