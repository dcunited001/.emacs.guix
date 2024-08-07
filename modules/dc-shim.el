;; -*- lexical-binding: t; -*-
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

;;* Shim

;;** Auto Mode Alist

;;*** Fix config files

;; (add-to-list 'auto-mode-alist '("config\\.scm\\'" . scheme-mode))

;;*** Fix cmake files
;; necessary?
;; (add-to-list 'auto-mode-alist '("^CMakeLists\\.txt$\\'" . cmake-ts-mode))

;;** PDF View

;; TODO: figure out what generated this in auto-mode-alist
;; PDF's need to open in pdf-view-mode to generate skeletons

;; ("\\.\\(?:PDF\\|EPUB\\|CBZ\\|FB2\\|O?XPS\\|DVI\\|OD[FGPST]\\|DOCX\\|XLSX?\\|PPTX?\\|pdf\\|epub\\|cbz\\|fb2\\|o?xps\\|djvu\\|dvi\\|od[fgpst]\\|docx\\|xlsx?\\|pptx?\\)\\'" . doc-view-mode-maybe)

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;;** Treesitter Auto Mode Alist

;; TODO: perhaps add yaml-ts-mode to extant snippets

;; =============================================
;;
;; ... omfg it /WAS/ changing the order ... !@#$
;;
;; ... causing auto-mode-alist adjustments to elicit paradoxical behavior
;;
;; ---------------------------------------------
;; something is automatically setting up major-mode-remap-alist (I think!)
;; (with-eval-after-load 'treesit
;;   ;; this doesn't work bc it uses eq
;;   ;; (delq "\\.ya?ml\\'" auto-mode-alist)

;;   ;; this is complicated because it changes the order
;;   (setq auto-mode-alist (a-dissoc auto-mode-alist "\\.ya?ml\\'")))
;; ---------------------------------------------

;;** Babel

;; NOTE: maybe not the best place for this
(add-hook 'org-mode-hook #'dc/org-babel-do-load-languages -25)
(add-hook 'org-mode-hook
          (lambda () (remove-hook
                      'org-mode-hook
                      #'dc/org-babel-do-load-languages +25)))

;;* Report

;; TODO: make this a notification, if DBUS is there
(message "init.el finished loading")

(provide 'dc-shim)
