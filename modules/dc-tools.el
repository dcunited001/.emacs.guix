;; -*- lexical-binding: t; -*-

;;* Tools


;;** Misc

;;*** TLDR
(setup (:pkg tldr))

;;** Emacs

;;*** Info Colors
(setup (:pkg info-colors :straight t))

;;** Guix
;; TODO how to handle geiser/guile when sourced from straight?
(setup (:pkg guix))

;; TODO configuring a call to consult-guix-packages
;;
;; look at trace of guix-packages-by-name for calls to consult--
;; - this uses cl-letf* to rebind the closure
;; - (completion-extra-properties ... ) sets up data for marginalia
;; - consult-completion-in-region handles previewing region-based completion
;;
;; look at consult-yasnippet source for examples
;; - consult-yasnippet--annotate
;; - consult-yasnippet--read-template, calls to consult--read
;;
;; guix uses bui to assemble tables (same as aurel)
;; - guix-ui-package.el: calls to (guix-ui-define-interface package info/list...)
;;   - these defines getters for synopsis/description
;; - guix-read.el#read-package-name calls guix-read-package-name-function
;;   - guix-read.el specifies readers for repl interaction
;; - guix-package.el contains the package "struct"
;;   - contains direct calls to (guix-eval-in-repl...)
;; - guix-ui-package.el#guix-read-package-entry-by-name passes entries to...
;; - guix-ui-package.el#...package-name-from-entries
;;   - runs --map (bui-entry-value it 'name) before (completing-read ...)
;;
;; either:
;; - (add-advice guix-package-name-from-entries ...)
;; - or wrap calls within a consult- closure transformation which
;;   can make calls to bui-x given the right context
;; - or use the (interactive) code from guix-packages-by-name
;;   to write a consult-guix--read-package function

;;** Unix

;;*** Firestarter
;; Interact with -*- vars: t; -*-
(setup (:pkg firestarter :straight t)
  (require 'firestarter)
  (firestarter-mode)
  (setq firestarter-default-type t))

;;*** Elf Mode
;; Interact with ELF binaries
(setup (:pkg elf-mode))

;;*** Crontab Mode
(setup (:pkg crontab-mode :straight t))

;;*** Journalctl Mode
(setup (:pkg journalctl-mode :straight t))

;;*** XDG Paths
(setup (:pkg xdg-paths :straight t))

;;** VCS

;;*** Repo
;; For Google Repo, but doesn't really do anything
;;(setup (:pkg repo :straight t))

;;*** Git Timemachine
;; control-f8, like facebook's conference
(setup (:pkg git-timemachine))

;; TODO: DOOM: defadvice! +vc-support-git-timemachine-a (fn)
;; TODO: DOOM: defadvice! +vc-update-header-line-a (revision)
;; TODO: DOOM: keybindings
  ;; (map! :map git-timemachine-mode-map
  ;;       :n "C-p" #'git-timemachine-show-previous-revision
  ;;       :n "C-n" #'git-timemachine-show-next-revision
  ;;       :n "gb"  #'git-timemachine-blame
  ;;       :n "gtc" #'git-timemachine-show-commit)

;;*** Magit tbdiff
;; interface to git-tbdiff, gives better control over git ranges
(setup (:pkg magit-tbdiff :straight t))

;; TODO: interactive: magit-tbdiff-ranges
;; TODO: interactive: magit-tbdiff-revs
;; TODO: interactive: magit-tbdiff-with-base
;; TODO: interactive: magit-tbdiff-save

;;** SSH

;;*** Tramp
;; (unless (bound-and-true-p tramp-remote-path)
;;   (setq tramp-remote-path
;;         '("~/.guix-profile/bin"
;;           "~/.guix-profile/sbin"
;;           "/run/current-system/profile/bin"
;;           )))
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path
               '("~/.guix-profile/bin"
                 "~/.guix-profile/sbin"
                 "/run/current-system/profile/bin")))

;;*** SSH Config Mode
(setup (:pkg ssh-config-mode))

;;*** x509 Mode
;; Inspect details on Certificates, CA's and CSR's
(setup (:pkg x509-mode :straight t)
  (setq auto-mode-alist (append
                         '(("\\.pem$" . x509-mode)
                           ("\\.cer$" . x509-mode)
                           ("\\.der$" . x509-mode)
                           ("\\.crt$" . x509-mode)
                           ("\\.crl$" . x509-mode))
                         auto-mode-alist)))

(provide 'dc-tools)
