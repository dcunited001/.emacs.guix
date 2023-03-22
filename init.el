;; -*- lexical-binding: t; -*-
(require 'subr-x)
(require 'a)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun dc/guix-profile-get-default-path ()
  (expand-file-name "~/.guix-extra-profiles/emacs-g/emacs-g/"))

(setq user-full-name "David Conner"
      user-mail-address "noreply@te.xel.io")

(setq dc/emacs-chemacs (expand-file-name "~/.emacs.d/")
      dc/emacs-d (expand-file-name "~/.emacs.g/")
      dc/emacs-cache (expand-file-name "~/.cache/emacs/")
      dc/emacs-dw (expand-file-name "dw" dc/emacs-d)
      dc/emacs-modules (expand-file-name "modules" dc/emacs-d )
      dc/emacs-doom-modules (expand-file-name "doom/modules" dc/emacs-d)
      dc/guix-profile-path (or (getenv "GUIX_ENVIRONMENT")
                               (dc/guix-profile-get-default-path))
      dc/emacs-sound-theme-path (file-name-as-directory (expand-file-name "share/sounds/freedesktop/stereo" dc/guix-profile-path)))

;; guix source: used to set guix-load-path and guix-load-compiled-path
(if-let ((guix-source-path (or (getenv "GUIX_SOURCE")
                               (and (getenv "_ECTO")
                                    (expand-file-name "guix/guix" (getenv "_ECTO"))))))
    (setq dc/guix-source-path (file-name-as-directory guix-source-path)))

;; emacs source (not the build source though)
(if-let ((emacs-source-path (or (getenv "EMACS_SOURCE")
                                (and (getenv "_ECTO")
                                     (expand-file-name "emacs/emacs/src" (getenv "_ECTO"))))))
    (setq source-directory (file-name-as-directory emacs-source-path)))

;; TODO: rectify user-emacs-* variables:
;; ... yeh, priceless are things like (kbd "C-u C-x e") to eval & insert
;; ... it works with any "M-x eval-*" function to eval emacs-lisp
;; ... using emacs for long, long time (since 2012 on/off).
;; ... I seen it on the refcard. I just realized the convention.

;; (setq user-emacs-directory "~/.local/share/emacs/"
;;       user-emacs-data-directory "~/.local/share/emacs/"
;;       user-emacs-lisp-directory "~/.local/share/emacs/lisp"
;;       user-emacs-cache-directory "~/.cache/emacs/"
;;       user-emacs-config-directory "~/.config/emacs/"
;;       user-emacs-ensime-directory "~/.emacs.g/var/ensime/")

;; (setq native-comp-eln-load-path "not ~/.emacs.g/eln-cache")

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

;; Add configuration modules to load path
(add-to-list 'load-path dc/emacs-dw)
(add-to-list 'load-path dc/emacs-modules)
(add-to-list 'load-path (expand-file-name "popup" dc/emacs-doom-modules))

;; Load pertinent modules
(require 'dw-package)
(require 'iso-transl)
(require 'dw-settings)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
;; (setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
;;       url-history-file (expand-file-name "url/history" user-emacs-directory))

(require 'dw-core)

(setq dc/eld-path (thread-last no-littering-etc-directory
                               (expand-file-name "dc")
                               (file-name-as-directory)))

(require 'dc-support)

(load-file (expand-file-name (concat dc/emacs-chemacs "per-system-settings.el")))

;; (require 'dc-terminal)
(require 'dc-desktop)
(require 'dc-alert)
(require 'dc-interface)
(require 'dc-popup)
;; (require 'dc-auth)

(require 'dc-info)

(require 'dw-org)
(require 'dc-org)

;; (require 'dw-shell)
(require 'dc-dev)
(require 'dw-swagger)
;; (require 'dw-dev-web)
(require 'dc-dev-clojure)

(require 'dc-tools)

;; (require 'dc-workflow)

(require 'dc-social)
;; (require 'dw-media)
;; (require 'dw-system)

(require 'dc-keys)
(require 'dc-mouse)

(when (featurep 'ido)
  (ido-mode nil))

;;*** Start the Daemon
(server-start)

(setq gc-cons-threshold (* 20 1000 1000))
