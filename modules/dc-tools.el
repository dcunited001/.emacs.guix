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

;;* Systems

;;** Emacs

;; benchmarking init
(setup (:pkg esup)
  (:option esup-depth 0))

;;*** Testing

(setup (:pkg buttercup))

;;** Arch
(setup (:pkg aurel :straight t :type git :flavor melpa
             :host github :repo "alezost/aurel"))
;;** Guix

(setup (:pkg guix)
  (:option guix-read-package-name-function
           #'guix-read-package-name-at-point)
  (:with-mode guix-derivation-mode
    (:file-match "\\/gnu\\/store\\/.*\\.drv\\'")
    (:hook lispy-mode))
  (:with-mode guix-build-log-mode
    (:file-match "\\/var\\/log\\/guix\\/drvs\\/.*\\.drv\\'"))
  (require 'guix-profiles))

(with-eval-after-load 'guix
  (cl-dolist (m '(guix-devel-mode
                  guix-build-log-mode
                  guix-derivation-mode))
    (add-to-list 'minions-prominent-modes m)))



;; (setq auto-mode-alist (cddr auto-mode-alist))

;;*** Guix Geiser Configuration

;;*** Guix graph configuration

;; https://emacs-guix.gitlab.io/website/manual/latest/emacs-guix.html#Graph-Configuration

;; to open a guix graph in an external application
;; (setq guix-find-file-function 'org-open-file)
;; (add-to-list 'org-file-apps '("\\.png\\'" . "sxiv %s"))

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

;;** Nix

(setup (:pkg nix-mode)
  (:file-match "\\.nix\\'"))

(setup (:pkg ob-nix :type git :flavor melpa
             :host codeberg :repo "theesm/ob-nix"))

;;** Containerd

(setup (:pkg docker))

;; (add-to-list
;; 'docker-image-run-custom-args
;; `("^postgres" ("-e POSTGRES_PASSWORD=postgres" . ,docker-image-run-default-args)))

;; TODO: add ~/.bin/script or get dockefile-build-buffer to implement this
;; https://dev.to/smortimerk/docker-build-replace-57gh

;;** Unix

;;*** Firestarter
;; Interact with -*- vars: t; -*-
(setup (:pkg firestarter :straight t)
  (:option firestarter-default-type t)
  (require 'firestarter)
  (firestarter-mode))

;;*** Elf Mode
;; Interact with ELF binaries
(setup (:pkg elf-mode))

;;*** Crontab Mode
(setup (:pkg crontab-mode :straight t))

;;*** Syslog Mode

;; this branch removes (toggle-read-only) so it loads strace_notes.el on (syslog-mode)
(setup (:pkg syslog-mode :type git :flavor melpa :host github
             :repo "dcunited001/syslog-mode"
             :branch "dcunited001-read-only")
  (:option syslog-setup-on-load t))

(add-to-list 'auto-mode-alist
	           '("\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\|\\.s?trace\\)\\'" . syslog-mode))

;; customize syslog-notes-files or set in .dir-locals.el to memoize notes files
;; the notes files are  *.elc files

;; (syslog-add-hooks), modifies auto-mode-alist, but isn't being called for me
;; "\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\|\\.s?trace\\)\\'"

;; options:

;; `syslog-notes-files’ : An alist used by `syslog-load-notes’ for choosing a notes file to load.
;; `syslog-mode-hook’ : *Hook to setup `syslog-mode’. (default = nil)
;; `syslog-views’ : A list of views. (default = nil)
;; `syslog-datetime-regexp’ A regular expression matching the date-time at the beginning of each line in the log file.
;; `syslog-log-file-directory’ : The directory in which log files are stored. (default = ”var/log”)
;; `syslog-large-file-size’ : When `syslog-show-file-note’ tries to load a file larger than this it prompts the user.
;; `syslog-hi-face-defaults’ : Alist of face sets to use for automatic highlighting.
;; `syslog-manpage-wait’ : Amount of time to wait for manpage to finish rendering, when processing manpages.
;; `syslog-note-things’ : An alist of (REGEX . SYMB) pairs for choosing `syslog-note-thing’.
;; `syslog-notes-default’ : List of `syslog-notes’ items that are always available.

;;** Linux

;;*** Device Tree

(setup (:pkg dts-mode))

;;*** Journalctl Mode
(setup (:pkg journalctl-mode :straight t))

;;** Red Hat
;; an emacs package that integrates a mock workflow would help quite a bit

;; nbarrientos/archive-rpm: simple and it helps.
;; I don't have enough exp to be sure of workflow support, but it helped a lot
(setup (:pkg archive-rpm :straight t
             :type git :flavor melpa
             :host github :repo "nbarrientos/archive-rpm"))

;; Other straight packages
;; emacsattic/rpm: last updated 14 years ago, probably good but probably not current
;; (rpm :type git :host github :repo "wemacsattic/rpm")

;; stigbjorlykke/rpm-spec-mode: tough to validate, but updated fairly recently
;; (rpm-spec-mode :type git :flavor melpa :host github :repo "stigbjorlykke/rpm-spec-mode")

;;** Builds
(setup (:pkg makefile-executor :straight t))
;; TODO project-aware dispatch of (makefile-executor-...)
;; - equivalent to doom's (+make/run)

;;* Network

;;** Terraform

;; mostly a major-mode only
(setup (:pkg terraform)
  (:option terraform-format-on-save t))
;; (:hook #'eglot-ensure)

;;** SSH

;;*** Tramp
;; TODO: look into Tramp "nc" method for busybox. will it exec babel scripts on DDWRT?
;; - can it connect to ddwrt with /bin/ash?
;; TODO: tramp: configure tramp-remote-path and basic tramp-connection properties

;; (unless (bound-and-true-p tramp-remote-path)
;;   (setq-default tramp-remote-path
;;         '("~/.guix-profile/bin"
;;           "~/.guix-profile/sbin"
;;           "/run/current-system/profile/bin"
;;           )))
(setup (:pkg tramp)
  (:option tramp-default-method "ssh"))

;; the guix emacs build prepends guix search paths to tramp-remote-path
(with-eval-after-load 'tramp
  (require 'tramp-container))

;;*** SSH Config Mode
(setup (:pkg ssh-config-mode))

;;*** x509 Mode
;; Inspect details on Certificates, CA's and CSR's
(setup (:pkg x509-mode)
  (cl-dolist (modespec '(("\\.pem$" . x509-mode)
                         ("\\.cer$" . x509-mode)
                         ("\\.der$" . x509-mode)
                         ("\\.crt$" . x509-mode)
                         ("\\.crl$" . x509-mode)))
    (add-to-list 'auto-mode-alist modespec)))

;; (with-eval-after-load 'x509-mode
;;   (add-hook 'x509-mode-hook #'(lambda () (call-interactively 'x509-dwim))))

;;* Services

;;** Configuration Management

;;*** SALTSTACK
;; (use-package! salt-mode)

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
;; ob-graphql requires graphql-mode, which does not depend on graphql
(setup (:pkg graphql-mode))
(setup (:pkg ob-graphql :straight t :type git :flavor melpa
             :host github :repo "jdormit/ob-graphql"))

;; (with-eval-after-load ... (in org)...
;; TODO: package: dynamic-graphs?
;; TODO: package: ob-dot

;;** Visualization

;;*** GNU Plot

;;*** Graphviz/Dot
(setup (:pkg graphviz-dot-mode)
  ;; TODO setup graphviz
  )

;; (setup (:pkg dynamic-graphs :straight t))

;;*** Mermaid

;; run from docker/podman, nice
;; https://github.com/mermaid-js/mermaid-cli#alternative-installations
(setup (:pkg mermaid-mode :straight t :type git :flavor melpa
             :host github :repo "abrochard/mermaid-mode")
  (require 'mermaid-mode)
  ;; also mermaid-mmdc-location, mermaid-flags
  (:option mermaid-output-format ".svg"))

;; both pkgs define org-babel-execute:mermaid.  ensure ob-mermaid loads after.
;; depending on how straight builds load-path, different functions could run.
;; https://github.com/abrochard/mermaid-mode/blob/master/mermaid-mode.el#L102-L121
(with-eval-after-load 'mermaid-mode
  ;; ob-mermaid basically only provides org-babel-execute:mermaid and formatting
  (setup (:pkg ob-mermaid :straight t :type git :flavor melpa
               :host github :repo "arnm/ob-mermaid")))

;; only necessary if (executable-find ...) returns nil
;; (:option ob-mermaid-cli-path "mmdc")

;;*** PlantUML

;;* Misc

;;*** Science
(setup (:pkg smiles-mode :straight t))

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
