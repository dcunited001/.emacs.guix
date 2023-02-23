;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2023 David Conner
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

;;* Tools


;;** Docs

;;*** TLDR
(setup (:pkg tldr))

;;*** Info

;; (setup (:pkg info-colors :straight t))
(with-eval-after-load 'info
  (require 'info+))


;;** Guix
;; TODO how to handle geiser/guile when sourced from straight?
(setup (:pkg guix))

;; TODO configuring a call to consult-guix-packages
;;
;; look at trace of guix-packages-by-name for calls to consult--
;; - this uses cl-letf* to rebind the closure
;; - (completion-extra-properties ... ) sets up data for marginalia
;; - consult-completion-in-region handles previewing region-based completion
;;
;; look at consult-yasnippet source for examples
;; - consult-yasnippet--annotate
;; - consult-yasnippet--read-template, calls to consult--read
;;
;; guix uses bui to assemble tables (same as aurel)
;; - guix-ui-package.el: calls to (guix-ui-define-interface package info/list...)
;;   - these defines getters for synopsis/description
;; - guix-read.el#read-package-name calls guix-read-package-name-function
;;   - guix-read.el specifies readers for repl interaction
;; - guix-package.el contains the package "struct"
;;   - contains direct calls to (guix-eval-in-repl...)
;; - guix-ui-package.el#guix-read-package-entry-by-name passes entries to...
;; - guix-ui-package.el#...package-name-from-entries
;;   - runs --map (bui-entry-value it 'name) before (completing-read ...)
;;
;; either:
;; - (add-advice guix-package-name-from-entries ...)
;; - or wrap calls within a consult- closure transformation which
;;   can make calls to bui-x given the right context
;; - or use the (interactive) code from guix-packages-by-name
;;   to write a consult-guix--read-package function

;;** Unix

;;*** Firestarter
;; Interact with -*- vars: t; -*-
(setup (:pkg firestarter :straight t)
  (require 'firestarter)
  (firestarter-mode)
  (setq firestarter-default-type t))

;;*** Elf Mode
;; Interact with ELF binaries
(setup (:pkg elf-mode))

;;*** Crontab Mode
(setup (:pkg crontab-mode :straight t))

;;*** Journalctl Mode
(setup (:pkg journalctl-mode :straight t))

;;*** XDG Paths
(setup (:pkg xdg-paths :straight t))

;;** VCS

;;*** Repo
;; For Google Repo
(setup (:pkg repo))
;; TODO: repo interactives/customs: repo-status, repo-init...

;;*** Repology
(setup (:pkg repology))
;; TODO: repology interactives/customs:
;; https://github.com/emacs-straight/repology/blob/master/repology.el

;;*** Git Timemachine
;; control-f8, like facebook's conference
(setup (:pkg git-timemachine))

;; TODO: DOOM: defadvice! +vc-support-git-timemachine-a (fn)
;; TODO: DOOM: defadvice! +vc-update-header-line-a (revision)
;; TODO: DOOM: keybindings
  ;; (map! :map git-timemachine-mode-map
  ;;       :n "C-p" #'git-timemachine-show-previous-revision
  ;;       :n "C-n" #'git-timemachine-show-next-revision
  ;;       :n "gb"  #'git-timemachine-blame
  ;;       :n "gtc" #'git-timemachine-show-commit)

;;*** Magit tbdiff
;; interface to git-tbdiff, gives better control over git ranges
(setup (:pkg magit-tbdiff :straight t))

;; TODO: interactive: magit-tbdiff-ranges
;; TODO: interactive: magit-tbdiff-revs
;; TODO: interactive: magit-tbdiff-with-base
;; TODO: interactive: magit-tbdiff-save

;;** SSH

;;*** Tramp
;; (unless (bound-and-true-p tramp-remote-path)
;;   (setq tramp-remote-path
;;         '("~/.guix-profile/bin"
;;           "~/.guix-profile/sbin"
;;           "/run/current-system/profile/bin"
;;           )))
(setup (:pkg tramp)
  (:option tramp-default-method "ssh"))

(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path
               '("~/.guix-profile/bin"
                 "~/.guix-profile/sbin"
                 "/run/current-system/profile/bin")))

;;*** SSH Config Mode
(setup (:pkg ssh-config-mode))

;;*** x509 Mode
;; Inspect details on Certificates, CA's and CSR's
(setup (:pkg x509-mode :straight t)
  (cl-dolist (modespec '(("\\.pem$" . x509-mode)
                         ("\\.cer$" . x509-mode)
                         ("\\.der$" . x509-mode)
                         ("\\.crt$" . x509-mode)
                         ("\\.crl$" . x509-mode)))
    (add-to-list 'auto-mode-alist modespec)))

;;** Database

;;** API

;;*** Restclient
;; TODO: configure restclient
;; (setup (:pkg restclient))
;; (setup (:pkg ob-restclient))

;;*** OpenAPI
;; TODO configure openapi-yaml-mode

;;*** GraphQL
;; TODO configure graphql & ob-graphql
;; (setup (:pkg graphql))
;; (setup (:pkg graphql-mode))
;;
;; (with-eval-after-load ... (in org)...
;; (setup (:pkg ob-graphql))
;;
;; TODO: package: dynamic-graphs?
;; TODO: package: ob-dot

;;** Visualization

;;*** GNU Plot

;;*** Graphviz/Dot

;;*** PlantUML

;;** Misc

;;*** Translation
(setup (:pkg google-translate)
  (:option google-translate-backend-method 'curl)
  ;; (:bind "C-T" #'my-google-translate-at-point)
  (defun google-translate--search-tkk ()
    (list 430675 2721866130))
  (defun my-google-translate-at-point ()
    "reverse translate if prefix"
    (interactive)
    (if current-prefix-arg
        (google-translate-at-point)
      (google-translate-at-point-reverse))))

;; TODO: ....ob-translate wants to clone org with straight...
;; (setup (:pkg ob-translate :straight t)
;;   (:load-after 'google-translate))

(provide 'dc-tools)
