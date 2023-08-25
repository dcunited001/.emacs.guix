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

(setup bibtex
  (:option bibtex-completion-bibliography (list dc/aca-papers-bibtex)
           bibtex-completion-library-path (list dc/aca-papers-directory)
           bibtex-completion-notes-path dc/aca-notes-path
           bibtex-completion-display-formats

           '((article
              . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
             (inbook
              . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
             (incollection
              . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
             (inproceedings
              . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
             (t
              . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))

           bibtex-completion-notes-path (expand-file-name citar-org-roam-subdir org-roam-directory)
           bibtex-completion-notes-template-multiple-files "\
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
" ;; (file-name-as-directory dc/aca-papers-directory)

           bibtex-autokey-year-length 4
           bibtex-autokey-name-year-separator "-"
           bibtex-autokey-year-title-separator "-"
           bibtex-autokey-titleword-separator "-"
           bibtex-autokey-titlewords 3
           bibtex-autokey-titlewords-stretch 1
           bibtex-autokey-titleword-length 5))

;; TODO: finish setting up bibtex: (add other completion directories)

;; - i'm not quite sure which packages/configs are req. generally or
;;   specifically for me

;; org-ref has removed its variable prefix:
;; org-ref-bibliography-notes org-roam-directory
;; org-ref-default-bibliography (list dc/aca-papers-bibtex)
;; org-ref-pdf-directory dc/aca-papers-directory
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

(setup (:pkg org-ref :straight t :type git :flavor git :inherit nil
             :host github :repo "jkitchin/org-ref" :branch "master"
             :files (:defaults "doi-utils.el" "nist-webook.el" "org-ref-arxiv.el"
                               "org-ref-bibtex.el" "org-ref-isbn.el" "org-ref-pubmed.el"
                               "org-ref-scifinder.el" "org-ref-scopus.el"))
  (:with-hook emacs-startup-hook
    ;; ok now they're loading
    (:hook dc/reload-org-ref-hydras)))

;; https://github.com/emacs-citar/citar
(setup (:pkg citar)
  (:option citar-library-paths dc/aca-library-paths
           citar-bibliography dc/aca-bibtex-paths
           org-cite-insert-processor 'citar
           org-cite-follow-processor 'citar
           org-cite-activate-processor 'citar)
  (:with-hook latex-mode-hook
    (:hook citar-capf-setup))
  (:with-hook org-mode-hook
    (:hook citar-capf-setup)))

(defun dc/setup-citar ()
  (add-to-list
   'org-roam-capture-templates
   `("n" "notes" plain "%?" :unnarrowed t
     :target
     (file+head
      "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
      "#+title: ${citar-citekey} (${citar-date}). ${note-title}.\n#+created: %U\n#+last_modified: %U\n\n")))
  (citar-org-roam-mode))


;; `("n" topics
;;   plain "%?" :unnarrowed t
;;   :target (file+head+olp
;;            "notes/${slug}.org"
;;            "#+TITLE: ${title}\n#+DESCRIPTION: ${description}\n#+TAGS:\n\n"
;;            ("Roam" "Docs" "Resources" "Topics" "Issues")))

;; https://github.com/emacs-citar/citar-org-roam
(setup (:pkg citar-org-roam)
  (:option citar-org-roam-note-title-template "${title} - ${author}"
           citar-org-roam-capture-template-key "n"))

(with-eval-after-load 'citar
  (with-eval-after-load 'org-roam
    (dc/setup-citar)))

;; TODO: add citar-embark (from straight)

(provide 'dc-bibtex)
