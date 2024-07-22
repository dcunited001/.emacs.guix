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

;; config examples

;; https://lucidmanager.org/productivity/emacs-bibtex-mode/

;; https://github.com/jkitchin/org-ref/tree/master#configuration

;; [[file:/data/ecto/x.files/plattfot/emacs/init.el]]
;; - https://git.sr.ht/~plattfot/emacs-plt/tree
;; - also has useful embark functionality

;; (string-join (mapcar #'file-name-directory dc/aca-bibtex-files) ":")

(use-package oc :straight (:type built-in) :demand t
  :custom
  (org-cite-global-bibliography dc/aca-bibtex-files))

;; also: oc-basic, oc-csl, oc-bibtex, oc-natbib, oc-biblatex

(use-package bibtex :striaght (:type built-in) :demand t
  :custom
  (bibtex-completion-bibliography dc/aca-bibtex-files)
  (bibtex-completion-library-path dc/aca-library-paths)
  (bibtex-completion-notes-path dc/aca-notes-path)
  (bibtex-file-path dc/aca-notes-path)

  ;; https://bibtex.eu/types/
  ;; TODO org-bibtex-type templates :book, :text, :techmanual (maybe)
  (bibtex-completion-display-formats
   '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
     (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
     (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
     (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
     (manual        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${edition:4}")
     (t . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))

   (bibtex-completion-notes-template-multiple-files "\
#+TITLE: ${title}
#+ROAM_KEY: cite:${=key=}
* Notes
:PROPERTIES:
:Custom_ID: ${=key=}
:AUTHOR: ${author-abbrev}
:JOURNAL: ${journaltitle}
:DATE: ${date}
:YEAR: ${year}
:DOI: ${doi}
:URL: ${url}
:END:

# :NOTER_DOCUMENT: %s${=key=}.pdf
")
   ;; (file-name-as-directory dc/aca-articles-directory)
   (bibtex-autokey-year-length 4)
   (bibtex-autokey-names 2)
   (bibtex-autokey-names-stretch 1)
   (bibtex-autokey-name-year-separator "-")
   (bibtex-autokey-year-title-separator "-")
   (bibtex-autokey-titleword-separator "-")
   (bibtex-autokey-titlewords 3)
   ;; (remove colon from default: [.!?:;]\|--)
   (bibtex-autokey-title-terminators "[.!?;]\\|--")
   (bibtex-autokey-titlewords-stretch 1)
   (bibtex-autokey-titleword-length 5)))

;; TODO: finish setting up bibtex: (add other completion directories)

;; - i'm not quite sure which packages/configs are req. generally or
;;   specifically for me

;; org-ref has removed its variable prefix:
;; org-ref-bibliography-notes org-roam-directory
;; org-ref-default-bibliography (list dc/aca-articles-bibtex)
;; org-ref-pdf-directory dc/aca-articles-directory
;; reftex-default-bibliography org-ref-default-bibliography

(defun dc/reload-org-ref-hydras ()
  "Setup/Straight aren't building/loading these properly. They must
be explicitly required after loading"

  ;; (with-eval-after-load 'org-ref)

  ;; doi-utils overrides the more complete link export in ol-doi
  (require 'doi-utils)
  (require 'nist-webbook)
  (require 'org-ref-arxiv)
  (require 'org-ref-bibtex)
  (require 'org-ref-isbn)
  (require 'org-ref-pubmed)
  (require 'org-ref-scifinder)
  (require 'org-ref-scopus))

(use-package ox-pandoc :straight ox-pandoc :demand t)

(use-package org-ref
  :straight (:type git :flavor git :inherit nil
		               :host github :repo "jkitchin/org-ref" :branch "master"
		               :files (:defaults "doi-utils.el"
                                     "nist-webook.el"
                                     "org-ref-arxiv.el"
				                             "org-ref-bibtex.el"
                                     "org-ref-isbn.el"
                                     "org-ref-pubmed.el"
				                             "org-ref-scifinder.el"
                                     "org-ref-scopus.el"
 				                             "org-ref.bib"
                                     "citeproc"))
  :demand t
  ;; :config
  ;; TODO: test without hook
  ;; append with depth? (orig. set to t?)
  ;; (add-hook 'emacs-startup-hook #'dc/reload-org-ref-hydras 30)
  )

;; https://github.com/emacs-citar/citar
;; (parsebib "4.2") (org "9.5") (citeproc "0.9")
(use-package citar :straight t :demand t
  :custom
  (citar-org-roam-subdir (file-name-base dc/aca-notes-path))
  (citar-library-paths dc/aca-library-paths)

  ;; citar-library-file-extensions: filters possible files more quickly
  (citar-bibliography dc/aca-bibtex-files)
  (citar-notes-paths (list dc/aca-notes-path))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)

  :hook
  (latex-mode-hook . citar-capf-setup)
  (org-mode-hook . citar-capf-setup))

;; TODO: citar indicator icons?
;; https://github.com/emacs-citar/citar/tree/main#indicators

(defun dc/citar-get-citekey-files (citekey)
  (gethash citekey
           (citar--get-resources citekey
                                 (mapcar (lambda (source)
                                           (plist-get source :items))
                                         citar-file-sources))))

(defun dc/org-roam-citar-add-noter-document ()
  (if (bound-and-true-p citekey)
      (pp citekey)
    (message "citekey not found"))
  (when-let ((citekey (bound-and-true-p citekey))
             ;; nil if not found, list if found
             (citefiles (dc/citar-get-citekey-files citekey)))
    ;; (cl-dolist 'cf citefiles)
    ;; only add first in list
    (org-roam-property-add "NOTER_DOCUMENT" (car citefiles))))

;; `("n" topics
;;   plain "%?" :unnarrowed t
;;   :target (file+head+olp
;;            "notes/${slug}.org"
;;            "#+TITLE: ${title}\n#+DESCRIPTION: ${description}\n#+TAGS:\n\n"
;;            ("Roam" "Docs" "Resources" "Topics" "Issues")))

;; https://github.com/emacs-citar/citar-org-roam
(use-package citar-org-roam  :straight t
  :after (citar org-roam)
  :demand t
  ;; :branch "main"
  :delight
  :custom
  (citar-org-roam-note-title-template "${title} - ${author}")
  (citar-org-roam-capture-template-key "nc")

  :config
  (add-to-list
   'org-roam-capture-templates
   `("nc" "Note: Citar" plain "%?" :unnarrowed t
     :target
     (file+head
      "noter/${citar-citekey}.org"
      ;; NOTE: The NOTER_DOCUMENT must be under a heading
      "#+title: (${citar-date}) ${note-title}.
#+created: %U
#+last_modified: %U

+ file :: ${citar-citekey}.pdf

* Notes
:PROPERTIES:
# :NOTER_DOCUMENT: %(dc/citar-get-citekey-files \"%{citar-citekey}\")
:END:
"
      ;; (citar-org-roam-mode)
      )))
  (citar-org-roam-mode))

(use-package citar-embark-mode :straight t
  :after (embark citar)
  :demand t
  :delight
  :config (citar-embark-mode))

;; TODO: get citar template to supply the PDF to the :NOTER_DOCUMENT: tag

;; alternative hooks (these won't work because of lexical binding (i think)
;; 'org-capture-before-finalize-hook -- citekey not in closure
;; 'org-roam-capture-preface-hook -- ominous docstring
;; 'org-roam-capture-new-node-hook -- just right
;; (add-hook 'org-roam-capture-new-node-hook
;;           #'dc/org-roam-citar-add-noter-document)


;; TODO: add citar-embark (from straight)

(provide 'dc-bibtex)
