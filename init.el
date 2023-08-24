;; -*- lexical-binding: t; -*-
(require 'subr-x)
(require 'a)

;;* Init

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

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

;;** Early Vars

;;*** User

(setq user-full-name "David Conner"
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
(defvar dc/ecto-path (getenv "_ECTO")
  "Directory where git-repo projects are checked out.")
(defvar dc/repo-path (getenv "_REPO")
  "Directory containing XML for git-repo projects are checked out.")

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
  (expand-file-name "~/.guix-extra-profiles/emacs-g/emacs-g/"))

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
  (setq guix-load-path (list (expand-file-name "share/guile/site/3.0"
                                               guix-pulled-profile))
        guix-load-compiled-path (list (expand-file-name "lib/guile/3.0/site-ccache"
                                                        guix-pulled-profile))))

;; this points to the profile for `guix shell`
(setq dc/guix-profile-path (or (getenv "GUIX_ENVIRONMENT")
                               (dc/guix-profile-get-default-path))
      dc/emacs-sound-theme-path (file-name-as-directory
                                 (expand-file-name
                                  "share/sounds/freedesktop/stereo"
                                  dc/guix-profile-path)))

;;*** Org Paths

(setq org-directory (file-name-as-directory (or (getenv "ORG_DIRECTORY") "/data/org"))
      org-roam-file-extensions '("org")
      org-roam-directory (or (and (boundp 'org-roam-directory) org-roam-directory) "roam")
      org-roam-directory (thread-first org-roam-directory
                                       (expand-file-name org-directory)
                                       (file-truename)
                                       (file-name-as-directory))


      ;; gets set by no-littering anyways
      ;; org-roam-db-location (file-name-concat no-littering-var-directory "org" "org-roam.db")

      dc/org-roam-templates-path (expand-file-name "etc/captures/roam"
                                                   dc/emacs-d)
      dc/org-roam-dailies-template (expand-file-name "daily-default.org"
                                                     dc/org-roam-templates-path))

;;** Modules

;; Load pertinent modules
(require 'dw-package)
(require 'iso-transl)
(require 'dw-settings)


;;*** Core

(require 'dw-core)

(setq dc/eld-path (thread-last no-littering-etc-directory
                               (expand-file-name "dc")
                               (file-name-as-directory)))

(require 'dc-support)

(load-file (expand-file-name (concat dc/emacs-chemacs "per-system-settings.el")))

;;*** UI

;; (require 'dc-terminal)
(require 'dc-desktop)
(require 'dc-alert)
(require 'dc-interface)
(require 'dc-popup)
(require 'dc-auth)
(require 'dc-project)

;;**** Info

(require 'dc-info)

;;*** Org

(require 'dw-org)
(require 'dc-org)

;;*** Dev

;; (require 'dw-shell)
(require 'dc-dev)
(require 'dc-dev-web)
(require 'dw-swagger)
;; (require 'dw-dev-web)
(require 'dc-dev-clojure)
(require 'dc-dev-scala)
(require 'dc-dev-python)

;;*** System

(require 'dc-tools)

(require 'dc-latex)

;; (require 'dc-workflow)

;;*** Apps

(require 'dc-social)
;; (require 'dw-media)
;; (require 'dw-system)

;;*** Keys & Mouse

(require 'dc-keys)
(require 'dc-mouse)

;;*** Final

;;**** Shims

(require 'dc-shim)

;;**** No ido

(when (featurep 'ido)
  (ido-mode nil))

;;**** Start the Daemon
(server-start)

(setq gc-cons-threshold (* 20 1000 1000))
