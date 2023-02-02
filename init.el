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

(setq dc/emacs-chemacs "~/.emacs.d")
(setq dc/emacs-d "~/.emacs.new")
(setq dc/emacs-cache "~/.cache/emacs")
(setq dc/emacs-dw
      (concat (file-name-as-directory dc/emacs-d) "dw"))
(setq dc/emacs-modules
      (concat (file-name-as-directory dc/emacs-d) "modules"))

;; Add configuration modules to load path
(add-to-list 'load-path dc/emacs-dw)
(add-to-list 'load-path dc/emacs-modules)

;; Load pertinent modules
(require 'dw-package)
;; (require 'dw-settings) ;; TODO: per-system-settings
(require 'dw-core)
(require 'dw-interface)
;; (require 'dw-auth)
;; (require 'dw-shell)
;; (require 'dw-dev)
;; (require 'dw-dev-web)
;; (require 'dw-workflow)
;; (require 'dw-social)
;; (require 'dw-media)
;; (require 'dw-system)

;; (when (string= system-name "acidburn")
;;   (require 'dw-streaming))

;; (when dw/exwm-enabled (require 'dw-desktop))
;; (when dw/mail-enabled (require 'dw-mail))
