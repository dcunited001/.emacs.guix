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

;;* Info

;; Load the info system for info files
(add-to-list 'auto-mode-alist '("\\.info\\'" . Info-on-current-buffer))

;; (setup (:pkg info-colors :straight t))

;; this loads a version from 2020
;; (with-eval-after-load "info"
;;   (require 'info+))

;; dirmngr in gnupg: X.509 CRL/OSCP
;; libksba: X.509
;; libtasn1: ASN.1 spec
;; gpgrt: handle GPG errors
;; gmp: GNU multiple precision
;; mpfr: multiple-precision floating-point reliable lib

;;** Narrow Info Search

;; things i have a working knowledge of
;; (by which i mean namedrop-level familiarity...)
;;
;; dups are removed, req. strings

(let ((eld-file (expand-file-name "Info-manuals-by-category.eld" dc/eld-path)))
  (setq dc/Info-manuals-by-category (dc/eld-unserialize eld-file)))

;; TODO: add gpm, gettext, libc?, basics, rest of software-dev, localization
;; TODO: setup info-path? (this only includes manuals on arch profiles)
;; TODO: find out which info-manuals arent contained in the uniqued keys

;; TODO: convert to string
(setq dc/Info-manual-default-categories
      '(emacs guile guix shell disk boot org magit emacs-ui emacs-completion make))
(defun dc/Info-manuals (&optional categories)
  (let ((categories (or categories dc/Info-manual-default-categories)))
    (thread-last categories
                 (seq-mapcat (lambda (k) (a-get dc/Info-manuals-by-category k)))
                 (seq-uniq)
                 (seq-map #'prin1-to-string))))

;;** Info+

;; (prin1-to-string 'emacs)
(setup (:pkg info+ :straight t :type git :host github :repo "emacsmirror/info-plus")
  (:option Info-breadcrumbs-depth 4
           Info-breadcrumbs-depth-internal 6
           Info-breadcrumbs-in-header-flag t
           Info-saved-history-file (expand-file-name "info-history"
                                                     no-littering-var-directory)
           Info-apropos-manuals (dc/Info-manuals))


  ;; TODO: (setq Info-apropos-manuals)
  (require 'info+)
  (Info-breadcrumbs-in-mode-line-mode +1)
  (Info-persist-history-mode +1))

(provide 'dc-info)
