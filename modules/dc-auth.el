;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2023 David Conner
;; Copyright © 2021 David Wilson
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;* Auth

;;** epa

;; epa-armor changes extensions to .asc, then encrypt/decrypt isn't transparent

;; (setq epa-armor t)
;; (setq epa-file-auto-mode-alist-entry
;;  '("\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" nil epa-file))
;; (setq epa-file-name-regexp
;;  "\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'")

;;** pinentry

;; epg-X-program must be set with customize (see epg-find-configuration in
;; epg-config.el). or alias gpg2 => gpg on arch.

;; run `gpgconf --listdirs bindir` to get the actual gpg. this must at least be
;; the same version as the gpg-agent

(require 'epg)
;; (setq epg-pinentry-mode 'cancel)
(setq epg-pinentry-mode 'loopback)
;; (setq epg-pinentry-mode 'ask)

(setq-default epg-user-id user-mail-address)
;; (setq epg-debug t)

;;** auth-source-pass

;; this doesn't provide any interactive functions

(use-package auth-source-pass :straight t
  :demand t
  ;;:init (setq-default auth-source-pass-filename ...)
  :config
  (auth-source-pass-enable))

;;** oauth2
;; (setup (:pkg oauth2 :straight t))

(provide 'dc-auth)

;; auth-source-gopass: "triplem/auth-source-gopass"
