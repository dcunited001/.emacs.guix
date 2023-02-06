;; -*- lexical-binding: t; -*-

;;* Org

(setq org-default-notes-file (dw/org-path "notes.org"))

(straight-use-package '(org :type built-in))

   ;; (setq org-capture-templates
   ;;   '((?b "* READ %?\n\n%a\n\n%:author (%:year): %:title\n   \
   ;;          In %:journal, %:pages.")))

(setup (:pkg org)
  (:also-load org-tempo)
  (:hook dw/org-mode-setup)
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-capture-bookmark nil)

  (setq org-modules
        '(ol-info         ;links to info nodes (nice ,thought i couldn 't do it)
          ol-man          ;links to man pages
          ol-doi          ;links to doi

          ;; https://fossies.org/linux/emacs/lisp/org/ol-bibtex.el
          ;; https://www.andy-roberts.net/res/writing/latex/bibentries.pdf
          ol-bibtex        ;link to entries in a bibtex database file
          ;; ol-bbdb                     ;links to bbdb entries

          ;; ol-irc                        ;links to irc
          ;; ol-eshell                     ;links to eshell
          ;; ol-eww
                                        ;store a link to an EWW buffer

          ;; https://w3m.sourceforge.net/MANUAL
          ;; ol-w3m                        ;links to w3m browser
          ;; ol-docview                    ;open files in docview-mode

          ol-gnus                       ;links to GNUS
          ;; ol-rmail                      ;links to rmail
          ;; ol-mhe                        ;links to MH-E messages

          ;; some security caveats of org-crypt: not impervious to analysis by volatile
          org-crypt                     ;transparent decryption of org-buffers
          ;; org-habit
          org-bookmark))

  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1)))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  ;; (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  ;; (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  ;; (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  ;; (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))


(provide 'dc-org)

;; TODO: DOOM: configure org-crypt
;; (use-package! org-crypt ; built-in
;;   :commands org-encrypt-entries org-encrypt-entry org-decrypt-entries org-decrypt-entry
;;   :hook (org-reveal-start . org-decrypt-entry)
;;   :preface
;;   ;; org-crypt falls back to CRYPTKEY property then `epa-file-encrypt-to', which
;;   ;; is a better default than the empty string `org-crypt-key' defaults to.
;;   (defvar org-crypt-key nil)
;;   (after! org
;;     (add-to-list 'org-tags-exclude-from-inheritance "crypt")
;;     (add-hook! 'org-mode-hook
;;       (add-hook 'before-save-hook 'org-encrypt-entries nil t))))
