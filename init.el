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

(setq dc/emacs-chemacs (expand-file-name "~/.emacs.d/")
      dc/emacs-d (expand-file-name "~/.emacs.guix/")
      dc/emacs-cache (expand-file-name "~/.cache/emacs/")
      dc/emacs-dw (concat (file-name-as-directory dc/emacs-d) "dw")
      dc/emacs-modules (concat (file-name-as-directory dc/emacs-d) "modules"))

(setq org-directory (file-name-as-directory (or (getenv "ORG_DIRECTORY") "/data/org"))
      org-roam-directory (concat org-directory "roam")
      org-roam-db-location (expand-file-name "~/.local/share/org-roam/org-roam.db")
      org-roam-file-extensions '("org"))

;; Add configuration modules to load path
(add-to-list 'load-path dc/emacs-dw)
(add-to-list 'load-path dc/emacs-modules)

;; Load pertinent modules
(require 'dw-package)
;; (require 'dw-settings) ;; TODO: per-system-settings

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
;; (setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
;;       url-history-file (expand-file-name "url/history" user-emacs-directory))

(require 'dw-core)

(require 'dc-interface)
;; (require 'dc-auth)

;; (require 'dw-shell)
;; (require 'dw-dev)
;; (require 'dw-dev-web)

;; (require 'dc-workflow)

;; (require 'dw-social)
;; (require 'dw-media)
;; (require 'dw-system)

