;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2014-2022 Henrik Lissner
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

;; * Latex

(require 'tex-site)
(require 'latex)                        ; requires 'tex

;; from doomemacs modules/lang/latex/config.el
;;
;; Do not prompt for a master file.
;; (setq-default TeX-master t)

;; (setq-hook! 'TeX-mode-hook
;;             ;; Tell Emacs how to parse TeX files.
;;             ispell-parser 'tex
;;             ;; Don't auto-fill in math blocks.
;;             fill-nobreak-predicate (cons #'texmathp fill-nobreak-predicate))

;; TODO smartparens-latex package?
;; TODO (setq LaTeX-section-hook '(...))
;; TODO Provide proper indentation for LaTeX "itemize", "enumerate", and
;;   "description" environments. See
;;   http://emacs.stackexchange.com/questions/3083/how-to-indent-items-in-latex-auctex-itemize-environments.
;; Set `+latex-indent-item-continuation-offset' to 0 to disable this.
;; (dolist (env '("itemize" "enumerate" "description"))
;;   (add-to-list 'LaTeX-indent-environment-list `(,env +latex-indent-item-fn)))

(defun dc/setup-smartparens-latex ()
  (with-eval-after-load 'smartparens-latex
    (let ((modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)))
      ;; All these excess pairs dramatically slow down typing in LaTeX buffers,
      ;; so we remove them. Let snippets do their job.
      (dolist (open '("\\left(" "\\left[" "\\left\\{" "\\left|"
                      "\\bigl(" "\\biggl(" "\\Bigl(" "\\Biggl(" "\\bigl["
                      "\\biggl[" "\\Bigl[" "\\Biggl[" "\\bigl\\{" "\\biggl\\{"
                      "\\Bigl\\{" "\\Biggl\\{"
                      "\\lfloor" "\\lceil" "\\langle"
                      "\\lVert" "\\lvert" "`"))
        (sp-local-pair modes open nil :actions :rem))
      ;; And tweak these so that users can decide whether they want use LaTeX
      ;; quotes or not, via `+latex-enable-plain-double-quotes'.
      (sp-local-pair modes "``" nil :unless '(:add sp-in-math-p)))))

(defun dc/setup-tex ()
  ;; M-x TeX-engine-set to xetex
  ;; (add-to-list 'TeX-command-list
  ;;              '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))

  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)

  ;; TODO setup tex to preview with pdf-tools
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)


  ;; LaTeX-fill-break-at-separators nil
  ;; LaTeX-item-indent 0
  (setq LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label)))

;; from tecosaur

(with-eval-after-load 'tex
  (dc/setup-tex)
  (dc/setup-smartparens-latex))

(setup (:pkg cdlatex)
  (:hook-into latex-mode LaTeX-mode)
  (:with-hook org-cdlatex-mode
    (:hook-into org-mode))
  (:option cdlatex-use-dollar-to-ensure-math nil)
  ;; Smartparens takes care of inserting closing delimiters, and if you
  ;; don't use smartparens you probably don't want these either.

  ;; Smartparens takes care of inserting closing delimiters
  (:bind "$" nil                        ; cdlatex-dollar
         "(" nil                        ; cdlatex-pbb
         "{" nil                        ; cdlatex-pbb
         "[" nil                        ; cdlatex-pbb
         "|" nil                        ; cdlatex-pbb
         "<" nil                        ; cdlatex-pbb

         ;; TAB is used for CDLaTeX's snippets and navigation. But we have yasnippet
         ;; (:when (modulep! :editor snippets)
         ;;        "TAB" nil)           ; cdlatex-tab

         "^" nil                        ; cdlatex-sub-superscript
         "_" nil                        ; cdlatex-sub-superscript

         ;; cdlatex-item: AUCTeX already provides this with `LaTeX-insert-item'.
         [(control return)] nil))

(provide 'dc-latex)
