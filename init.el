;; -*- lexical-binding: t; -*-

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'after-init-hook (lambda ()
                             ;; Make gc pauses faster by decreasing the threshold.
                             ;; (setq gc-cons-threshold (* 2 1000 1000))
                             (setq gc-cons-threshold (* 20 1000 1000))))

(setq user-full-name "David Conner"
      user-mail-address "noreply@te.xel.io")

(setq dc/emacs-chemacs (expand-file-name "~/.emacs.d/")
      dc/emacs-d (expand-file-name "~/.emacs.g/")
      dc/emacs-cache (expand-file-name "~/.cache/emacs/")
      dc/emacs-dw (concat (file-name-as-directory dc/emacs-d) "dw")
      dc/emacs-modules (concat (file-name-as-directory dc/emacs-d) "modules"))

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
      org-roam-directory (file-name-as-directory (concat org-directory "roam"))
      org-roam-db-location (expand-file-name "~/.local/share/org-roam/org-roam.db")
      org-roam-file-extensions '("org"))

;; Add configuration modules to load path
(add-to-list 'load-path dc/emacs-dw)
(add-to-list 'load-path dc/emacs-modules)

;; Load pertinent modules
(require 'dw-package)
(require 'iso-transl)
(require 'dw-settings)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
;; (setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
;;       url-history-file (expand-file-name "url/history" user-emacs-directory))

(require 'dw-core)



(require 'dc-support)

(load-file (expand-file-name (concat dc/emacs-chemacs "per-system-settings.el")))
(require 'dc-interface)
;; (require 'dc-auth)

(require 'dw-org)

;; (require 'dw-shell)
(require 'dc-dev)
;; (require 'dw-dev-web)

(require 'dc-tools)

;; (require 'dc-workflow)

(require 'dc-social)
;; (require 'dw-media)
;; (require 'dw-system)

(require 'dc-keys)
(require 'dc-mouse)

(when (featurep 'ido)
  (ido-mode nil))
