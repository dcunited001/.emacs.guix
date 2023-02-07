;; -*- lexical-binding: t; -*-

;;* Auth

;;** pinentry

(setq dc/use-pinentry nil)

(when dc/use-pinentry
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

;;** auth-source-pass

(setup (:pkg auth-source-pass)
  (auth-source-pass-enable))

;;** oauth2

(setup (:pkg oauth2 :straight t))

(provide 'dc-auth)
