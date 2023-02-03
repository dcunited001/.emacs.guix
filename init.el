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

(setq dc/emacs-chemacs "~/.emacs.d"
      dc/emacs-d "~/.emacs.new"
      dc/emacs-cache "~/.cache/emacs"
      dc/emacs-dw (concat (file-name-as-directory dc/emacs-d) "dw")
      dc/emacs-modules (concat (file-name-as-directory dc/emacs-d) "modules"))

(setq org-directory (getenv "ORG_DIRECTORY")
      org-roam-directory (concat (file-name-as-directory org-directory) "roam")
      org-roam-db-location (concat (file-name-as-directory (concat (getenv "HOME") "/.local/share/org-roam")) "org-roam.db")
      org-roam-file-extensions '("org"))

;; Add configuration modules to load path
(add-to-list 'load-path dc/emacs-dw)
(add-to-list 'load-path dc/emacs-modules)

;; Load pertinent modules
(require 'dw-package)
;; (require 'dw-settings) ;; TODO: per-system-settings
(require 'dw-core)
(require 'dw-interface)
(require 'dc-auth)
;; (require 'dw-shell)
;; (require 'dw-dev)
;; (require 'dw-dev-web)
(require 'dc-workflow)
;; (require 'dw-social)
;; (require 'dw-media)
;; (require 'dw-system)


;; activated outside of init.el
;; (require 'dw-mail) ;; n/a

;; (when (string= system-name "acidburn")
;;   (require 'dw-streaming))

;; (when dw/exwm-enabled (require 'dw-desktop))
;; (when dw/mail-enabled (require 'dw-mail))
