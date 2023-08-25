
(require 'bibtex)

;; https://lucidmanager.org/productivity/emacs-bibtex-mode/

;; TODO: finish setting up bibtex: (add other completion directories)

;; - i'm not quite sure which packages/configs are req. generally or
;;   specifically for me

;; org-ref has removed its variable prefix:
;; org-ref-bibliography-notes org-roam-directory
;; org-ref-default-bibliography (list dc/aca-papers-bibtex)
;; org-ref-pdf-directory dc/aca-papers-directory
;; reftex-default-bibliography org-ref-default-bibliography

(setup (:pkg org-ref :straight t :type git :flavor git
             :host github :repo "jkitchin/org-ref" :branch "master")

  (:option bibtex-completion-bibliography (list dc/aca-papers-bibtex)
           bibtex-completion-library-path (list dc/aca-papers-directory)
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
           ))

;; https://github.com/jkitchin/org-ref/tree/master#configuration

;; (setq bibtex-autokey-year-length 4
;;       bibtex-autokey-name-year-separator "-"
;;       bibtex-autokey-year-title-separator "-"
;;       bibtex-autokey-titleword-separator "-"
;;       bibtex-autokey-titlewords 2
;;       bibtex-autokey-titlewords-stretch 1
;;       bibtex-autokey-titleword-length 5)

;; (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)


;; [[file:/data/ecto/x.files/plattfot/emacs/init.el]]
;; https://git.sr.ht/~plattfot/emacs-plt/tree

;; also has useful embark functionality

;; https://github.com/emacs-citar/citar
(setup (:pkg citar)
  (:option citar-library-paths (list dc/aca-papers-directory
                                     dc/aca-doi-directory
                                     dc/aca-texts-directory
                                     dc/aca-books-directory)
           citar-bibliography (list dc/aca-texts-bibtex
                                    dc/aca-papers-bibtex
                                    dc/aca-books-bibtex
                                    dc/aca-doi-bibtex)
           org-cite-insert-processor 'citar
           org-cite-follow-processor 'citar
           org-cite-activate-processor 'citar)
  (:with-hook latex-mode-hook
    (:hook citar-capf-setup))
  (:with-hook org-mode-hook
    (:hook citar-capf-setup)))

(defun dc/setup-citar ()
  (add-to-list 'org-roam-capture-templates
               `("n" "literature note" plain
                 "%?"
                 :target
                 (file+head
                  "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
                  "#+title: ${citar-citekey} (${citar-date}). ${note-title}.\n#+created: %U\n#+last_modified: %U\n\n")
                 :unnarrowed t))
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
