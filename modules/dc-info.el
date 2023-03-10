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

;; TODO: load this from an eld file

;; things i have a working knowledge of
;; (by which i mean namedrop-level familiarity...)
;;
;; dups are removed, req. strings

(setq dc/Info-manuals-by-category
      (let ((eld-data
             (expand-file-name "dc/Info-manuals-by-category.eld" no-littering-etc-directory)))
        (dc/eld-unserialize eld-data)))

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
