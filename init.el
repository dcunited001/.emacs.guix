;; -*- lexical-binding: t; -*-
(require 'subr-x)
(require 'a)
(require 'xdg)

;;* Init

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; The default is 800 kilobytes.  Measured in bytes.
;; hitting this 30 times in startup with 50 MB
;; 27 with 100 MB
(setq gc-cons-threshold (* 100 (expt 2 20)))

;;** System Identification

;; DOOM: ./lisp/core/doom.el
;;; Global constants
(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))
(defconst EMACS28+    (> emacs-major-version 27))
(defconst EMACS29+    (> emacs-major-version 28))
(defconst MODULES     (featurep 'dynamic-modules))
(defconst NATIVECOMP  (featurep 'native-compile))
(defconst DBUS_FOUND (not (null (getenv "DBUS_SESSION"))))

;;** Early Vars

;;*** User

(setq-default user-full-name "David Conner"
              user-mail-address (or (getenv "EMAIL") "noreply@te.xel.io"))

;;*** Emacs Config

;;**** Paths

(setq dc/emacs-chemacs (expand-file-name "~/.emacs.d/")
      dc/emacs-d (expand-file-name "~/.emacs.g/")
      dc/emacs-cache (expand-file-name "~/.cache/emacs/")
      dc/emacs-dw (expand-file-name "dw" dc/emacs-d)
      ;; dc/emacs-doom-modules (expand-file-name "doom/modules" dc/emacs-d)
      dc/emacs-modules (expand-file-name "modules" dc/emacs-d))

;; Add configuration modules to load path
(add-to-list 'load-path dc/emacs-dw)
(add-to-list 'load-path dc/emacs-modules)

;; TODO: rectify user-emacs-* variables: the no-littering package is set from
;; these. they need to be set before, but it's variables aren't affected by
;; them. emacs sets user-emacs-directory on startup. no-lit may get a different
;; value or something else is changing them (chemacs2?)

;; (setq user-emacs-directory "~/.local/share/emacs/"
;;       user-emacs-data-directory "~/.local/share/emacs/"
;;       user-emacs-lisp-directory "~/.local/share/emacs/lisp"
;;       user-emacs-cache-directory "~/.cache/emacs/"
;;       user-emacs-config-directory "~/.config/emacs/"
;;       user-emacs-ensime-directory "~/.emacs.g/var/ensime/")

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
;; (setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
;;       url-history-file (expand-file-name "url/history" user-emacs-directory))

;;**** Native Comp

;; (setq native-comp-eln-load-path "not ~/.emacs.g/eln-cache")

;;*** Repo Paths

;; TODO: generally replace the dc/*-vars with defvar or ... maybe defcustom, but
;; that requires defgroup (must be dc-*-vars afaik or you brings chaos to the
;; emacs world)

;; comments are such bad, docs are wow and var lookups yay
(defvar dc/ecto-path (or (getenv "_ECTO") (expand-file-name "~/ecto"))
  "Directory where git-repo projects are checked out.")
(defvar dc/repo-path (or (getenv "_REPO") (expand-file-name "~/repo"))
  "Directory containing XML for git-repo projects are checked out.")
(defvar dc/lang-path (or (getenv "_LANG") (expand-file-name "~/lang"))
  "Directory containing quick projects under ./lang. It typically
contains config under ./.lang to encourage native and portable
12factor language configs, when not container.")

;;*** Guix/Geiser Paths

(defvar dc/guix-checkout-path (getenv "GUIX_SOURCE")
  "Directory containing a guix checkout. The .dir-locals.el in Guix
should be used for setting guix-load-path unless working on
checkouts of channels which depend on modified packages in the
Guix channel.")

(defvar source-directory (getenv "EMACS_SOURCE")
  "Directory containing the ./src directory of an Emacs checkout.")

(unless source-directory (warn "Emacs: source-directory is not set"))
(unless dc/guix-checkout-path (warn "Emacs: source-directory is not set"))

;; TODO: update service definitions and settle on environment variables

(defun dc/guix-profile-get-default-path ()
  (expand-file-name "emacs-g/emacs-g" (getenv "GUIX_EXTRA")))

(defun dc/guix-guile-paths (&optional profile-path)
  "Return `load-path' and `load-compiled-path' for a guix
 `profile-path'"

  ;; guix-profile(?), guix-home-profile(?), guix-user-profile(?),
  ;; guix-pulled-profile(?)
  ;; guix-system-profile (lacks ? method)
  (let ((profile-path (or profile-path guix-pulled-profile)))
    `((load-path . ,(expand-file-name "share/guile/3.0/site"
                                      profile-path))
      (compiled-load-path . ,(expand-file-name "lib/guile/3.0/site-ccache"
                                               profile-path)))))

;; NOTE guile-load-compiled-path not needed when .scm and .go are in the same
;; directory. this happens in a guix checkout, but not for the channels.
(defun dc/guix-reset-paths ()
  (setq-default guix-load-path (list (expand-file-name "share/guile/site/3.0"
                                                       guix-pulled-profile))
                guix-load-compiled-path (list (expand-file-name "lib/guile/3.0/site-ccache"
                                                                guix-pulled-profile))))

;; this points to the profile for `guix shell`
(setq-default dc/guix-profile-path (or (getenv "GUIX_ENVIRONMENT")
                                       (dc/guix-profile-get-default-path))
              dc/emacs-sound-theme-path (file-name-as-directory
                                         (expand-file-name
                                          "share/sounds/freedesktop/stereo"
                                          dc/guix-profile-path)))

;;*** Org Paths

(setq-default org-directory (file-name-as-directory (or (getenv "ORG_DIRECTORY") "/data/org"))
              org-roam-file-extensions '("org")
              org-roam-directory (or (and (boundp 'org-roam-directory) org-roam-directory) "roam")
              org-roam-directory (thread-first org-roam-directory
                                               (expand-file-name org-directory)
                                               (file-truename)
                                               (file-name-as-directory))


              ;; gets set by no-littering anyways
              ;; org-roam-db-location (file-name-concat no-littering-var-directory "org" "org-roam.db")

              dc/org-roam-n-dailies 5
              dc/org-roam-templates-path (expand-file-name "etc/captures/roam"
                                                           dc/emacs-d)
              dc/org-roam-dailies-template (expand-file-name "daily-default.org"
                                                             dc/org-roam-templates-path))

;;*** Org Babel Load Languages

;; this is appended to in dc-dev-*.el, then loaded in dc-shim.el
;;
;; NOTE: at some point, jupyter presented an issue regarding jupyter kernelspecs
;; being required when the symbols are loaded by (org-babel-do-load-languages
;; ...)
;;
;; I guess everything else can be determined here
(setq dc/org-babel-load-languages
      '((emacs-lisp . t)
        (shell . t)
        (python . t)
        (jq . t)
	      (restclient . t)))

;;**** Org Ref & Bibtex

;; TODO refactor slim down (auto def symbols, create paths if dc/aca-doc-root exists)
;;
;; - may need to ensure that the doi's exist. a macro would help, but i just
;;   need to determine how the file/db structure would accommodate changes

;; i'm not sure what structure i'll stick with. this is usually what PhD's need
;;   zero help with, of course. the most successful PhD's have no idea what
;;   they're doing here and typically leave notes wherever. doesn't
;;   matter. </joking>

(setq-default dc/aca-doc-root (xdg-user-dir "DOCUMENTS")

              ;; see 'org-bibtex-types for the 14 official types
              ;; dc/aca-bibtex-types (list :article :book :techreport :manual)
              dc/aca-subpaths (list "articles" "books" "texts")

              ;; both citar and org-ref want these to end in a slash
              ;; citar magically agrees on on the citekey org-ref uses to create PDF's
              dc/aca-notes-path (expand-file-name "noter/" org-roam-directory)

              dc/aca-texts-directory (expand-file-name "texts/" dc/aca-doc-root)
              dc/aca-texts-bibtex (expand-file-name "noter/texts.bib" org-roam-directory)
              dc/aca-articles-directory (expand-file-name "articles/" dc/aca-doc-root)
              dc/aca-articles-bibtex (expand-file-name "noter/articles.bib" org-roam-directory)
              dc/aca-books-directory (expand-file-name "books/" dc/aca-doc-root)
              dc/aca-books-bibtex (expand-file-name "noter/books.bib" org-roam-directory)

              dc/aca-library-paths (list dc/aca-texts-directory
                                         dc/aca-articles-directory
                                         dc/aca-books-directory)
              dc/aca-bibtex-files (list dc/aca-texts-bibtex
                                        dc/aca-articles-bibtex
                                        dc/aca-books-bibtex))

(dolist (el dc/aca-bibtex-files)
  (unless (file-exists-p el)
    ;; (f-touch el)
    (warn "Bibtex: file does not exist %s. See 'dc-bibtex" el)))

;;** Modules

;; Load pertinent modules
(require 'dw-package)
(require 'iso-transl)
(require 'dw-settings)

;;** Straight

;;*** Pseudo Packages

;; alot of warnings like this when loading ghub
;; - Required package ‘compat-29.1.4.1’ is unavailable
;; - these reference the wrong version numbers (magit-2.21, ghub-2.0, ghub+)
;; - see note in org roam

(require 'compat)

;; get straight to avoid fetching these (i'm hoping it will build against the
;; correct entryies in load-paths, but I haven't had problems yet.

(let ((deps-from-guix
       '(pdf-tools org which-key hydra magit compat ; eglot
                   embark consult corfu cape vertigo marginalia
                   flycheck
                   orderless kind-icon)))
  (mapc (apply-partially #'add-to-list 'straight-built-in-pseudo-packages)
        deps-from-guix))

;;*** Load Eglot Early

;; The newer version of eglot 0.17 has a lot of improvements over what's bundled
;; with 29.1 and 29.2. This seems to load eglot through straight

(setup (:pkg eglot :straight t :type git
             :host github :repo "emacs-straight/eglot"
             :files ("*" (:exclude ".git")))
  (require 'eglot))

;;** Core

;;*** Appendables

;; init early to keep logic close to (require 'package) ... which I may change,
;; since I lose control of the ordering of prominent modes.

;; In that case, i'll just run this and then fixup.
;;
;; `grep -re "(add-to-list 'minions-prominent-modes '.*)" >> ./modules/dc-modelines.el

;; (eq t (xor (minions-demoted (or minions-promoted enabled))))
(setq minions-prominent-modes
      '(superword-mode
        subword-mode
        2C-mode
        multiple-cursors-mode)
      minions-demoted-modes nil)

;;**** Prominent

(add-to-list 'minions-prominent-modes 'combobulate-mode)
(add-to-list 'minions-prominent-modes 'apheleia-mode)

;; editor
(add-to-list 'minions-prominent-modes 'undo-tree-mode)
(add-to-list 'minions-prominent-modes 'smartparens-mode)

;; customize overwrite-mode-binary and overwrite-mode-textual
(add-to-list 'minions-prominent-modes 'overwrite-mode)
(add-to-list 'minions-prominent-modes 'view-mode)

;; org-like
(add-to-list 'minions-prominent-modes 'cdlatex-mode)
(add-to-list 'minions-prominent-modes 'outline-mode)

;;**** Demoted

(add-to-list 'minions-demoted-modes 'rainbow-mode)
(add-to-list 'minions-demoted-modes 'rainbow-delimiters-mode)
(add-to-list 'minions-demoted-modes 'super-savemode)
(add-to-list 'minions-demoted-modes 'ws-butler-mode)

;;*** Core Init

(require 'dw-core)

(require 'dc-support)
(require 'dc-network)

;; no backup files
(setq make-backup-files nil)
(setq dc/eld-path (thread-last no-littering-etc-directory
                               (expand-file-name "dc")
                               (file-name-as-directory)))

(load-file (expand-file-name (concat dc/emacs-chemacs "per-system-settings.el")))

;;** UI

;; (require 'dc-terminal)
(require 'dc-desktop)
(require 'dc-alert)
(require 'dc-project)
(require 'dc-interface)
(require 'dc-popup)
(require 'dc-auth)

;;*** Info

(require 'dc-info)

;;** Org

(require 'dw-org)
(require 'dc-org)
(require 'dc-bibtex)

;;** Dev

(require 'dc-fly)
;; (require 'dw-shell)
(require 'dc-dev)
(require 'dc-dev-web)
(require 'dw-swagger)
(require 'dc-dev-cpp)
(require 'dc-dev-java)
(require 'dc-dev-clojure)
(require 'dc-dev-scala)
(require 'dc-dev-python)

;;** System

(require 'dc-vcs)
(require 'dc-tools)
(require 'dc-dev-yaml)

(require 'dc-latex)

;; (require 'dc-workflow)

;;** Apps

(require 'dc-social)
;; (require 'dc-applications)
;; (require 'dw-media)
;; (require 'dw-system)
;; (require 'dc-games)

;;** Keys & Mouse

(require 'dc-keys)
(require 'dc-mouse)

;;** Final

(require 'dc-modeline)

;;*** Shim

(require 'dc-shim)

;;**** No ido

(when (featurep 'ido)
  (ido-mode nil))

;;**** Start the Daemon
(server-start)

(setq gc-cons-threshold (* 50 (expt 2 20)))

(alert "Emacs server ready." :title "Emacs GC:")
