;; -*- lexical-binding: t; -*-

;;; -- Use Emacs for Pinentry -----

(setq dc/use-pinentry nil)

(when dc/use-pinentry
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

;;; -- Password Management -----

;; (setup (:pkg password-store)
;;   (setq password-store-password-length 12)
;;   (dw/leader-key-def
;;     "ap" '(:ignore t :which-key "pass")
;;     "app" 'password-store-copy
;;     "api" 'password-store-insert
;;     "apg" 'password-store-generate))

(setup (:pkg auth-source-pass)
  (auth-source-pass-enable))

;;; -- OAuth2 -----

(setup (:pkg oauth2 :straight t))

(provide 'dc-auth)
