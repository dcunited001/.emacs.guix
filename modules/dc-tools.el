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

;;*** Dash


;;*** TLDR
(use-package tldr :straight t :defer t)

;;* Systems

;;** Emacs

;; benchmarking init
(use-package esup :straight t
  :disabled
  :custom
  (esup-depth 0))

;;*** Testing

(use-package buttercup :straight t :disabled)

;;** Arch
(use-package aurel :straight t :disabled)

;;** Guix

;; (use-package guix :straight t
;;   (:option guix-read-package-name-function
;;            #'guix-read-package-name-at-point)
;;   (:with-mode guix-derivation-mode
;;     (:file-match "\\/gnu\\/store\\/.*\\.drv\\'")
;;     (:hook lispy-mode))
;;   (:with-mode guix-build-log-mode
;;     (:file-match "\\/var\\/log\\/guix\\/drvs\\/.*\\.drv\\'"))
;;   (require 'guix-profiles))

;; (with-eval-after-load 'guix
;;   (cl-dolist (m '(guix-devel-mode
;;                   guix-build-log-mode
;;                   guix-derivation-mode))
;;     (add-to-list 'minions-prominent-modes m)))

;;*** Recutils

;; Guix package search results are output in the recutils format

;; rec-mode and ob-rec are both in emacs-rec-mode
(use-package rec-mode :straight t
  :defer t
  :config
  (require 'ob-rec)
  (add-to-list 'dc/org-babel-load-languages '(rec . t))
  (dc/org-babel-do-load-languages))

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

;; TODO for nix: interpreter-mode-alist?
(use-package nix-mode :straight t :defer t :mode "\\.nix\\'")
(use-package ob-nix :straight t :defer t :after nix-mode
  :config
  (add-to-list 'dc/org-babel-load-languages '(nix . t))
  (dc/org-babel-do-load-languages))

;;** Containerd

(use-package docker :straight t :defer t)

;; (add-to-list
;; 'docker-image-run-custom-args
;; `("^postgres" ("-e POSTGRES_PASSWORD=postgres" . ,docker-image-run-default-args)))

;; TODO: add ~/.bin/script or get dockefile-build-buffer to implement this
;; https://dev.to/smortimerk/docker-build-replace-57gh

;;** Unix

;;*** Firestarter
;; -*- firestarter: "shell command"; -*- run command on save
(use-package firestarter :straight t :disabled
  :custom
  (firestarter-default-type t))

;  :config
;  (firestarter-mode)

;;*** Elf Mode
;; Interact with ELF binaries
(use-package elf-mode :straight t
  :commands elf-mode
  :magic ("ELF" . 'elf-mode))

;;*** Crontab Mode
(use-package crontab-mode :straight t :demand t)

;;*** Syslog Mode

;; this branch removes (toggle-read-only) so it loads strace_notes.el on (syslog-mode)
(use-package syslog-mode :straight (:type git :flavor melpa :host github
				:repo "dcunited001/syslog-mode"
				:branch "dcunited001-read-only")
  :custom (syslog-setup-on-load t)
  :mode ("\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\|\\.s?trace\\)\\'" . syslog-mode))

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

(use-package dts-mode :straight t :defer t)

;;*** Journalctl Mode
(use-package journalctl-mode :straight t :defer t
  :commands journalctl)

;;** Red Hat
;; an emacs package that integrates a mock workflow would help quite a bit

;; nbarrientos/archive-rpm: simple and it helps.
;; I don't have enough exp to be sure of workflow support, but it helped a lot
(use-package archive-rpm :straight t)

;; Other straight packages
;; emacsattic/rpm: last updated 14 years ago, probably good but probably not current
;; (rpm :type git :host github :repo "wemacsattic/rpm")

;; stigbjorlykke/rpm-spec-mode: tough to validate, but updated fairly recently
;; (rpm-spec-mode :type git :flavor melpa :host github :repo "stigbjorlykke/rpm-spec-mode")

;;** Builds

;; configure in dc-dev instead?

;; (runs a formatted %string)
;; (setup (:pkg makefile-executor :straight t))

;; TODO project-aware dispatch of (makefile-executor-...)
;; - equivalent to doom's (+make/run)

;;* Network

;;** password-store.el

;; access the password store

(use-package password-store :straight t :demand t
  :custom (password-store-time-before-clipboard-restore 15))

;;** Pass.el

;; Pass.el mostly is used to edit/manage the (a) password store.  pass-view-mode
;; derives from nil and functions a bit like dired.

(use-package pass :straight t :after password-store :demand t
  ;; these are defaults
  :custom
  (pass-show-keybindings t)
  (pass-username-field "username")
  (pass-username-fallback-on-filename nil)
  (pass-suppress-confirmations nil))

;;** Terraform

;; mostly a major-mode only
(use-package terraform-mode :straight t
  :custom (terraform-format-on-save t))
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
(use-package tramp :straight t :demand t
  ;; the guix emacs build prepends guix search paths to tramp-remote-path
  :config (require 'tramp-container)
  ;; TODO (add-hook 'emacs-startup-hook 'tramp-container)
  :custom (tramp-default-method "ssh"))

;;*** SSH Config Mode
(use-package ssh-config-mode :straight t)

;;*** x509 Mode
;; Inspect details on Certificates, CA's and CSR's
(use-package x509-mode :straight t
 ;; TODO x509-mode-hook: jumping straight to x509 doesn't work well
 ;; :hook (x509-mode-hook . (lambda () (call-interactively 'x509-dwim)))
 :mode ((rx "." (| "pem" "cer" "der" "crt" "crl") eos) . x509-mode))

;;* Services

;;** Management

;;*** SystemD

;; loads for buffers that match systemd-autoload-regexp
(use-package systemd :straight (:type built-in) :defer t
  :config
  (if (and IS-LINUX (null dw/is-guix-system))
      (add-hook 'systemd-mode-hook #'flycheck-mode)))

;; TODO: GUIX: check for executable at: (flycheck-define-checker systemd-analyze ...)

;;*** Prodigy.el

;;*** Detach.el

;; TODO: customize detached.el (plan to use .dir-locals.el
(use-package detached :straight t :defer t)

(defun dc/detached-kbd-setup ()
  ;; trying out detached. just lazily load for now
  (interactive)
  (detached-init)
  (general-define-key
   :keymaps 'global
   "C-c d" '(:prefix-command detached-action-map :wk "DETACHED")
   [remap detached-open-session #'detached-consult-session]))

;;** Configuration Management

;;*** SALTSTACK
;; (use-package! salt-mode)

;;* Data

;;** Structure

;; this is found in the protocolbuffers/protobuf repository
(use-package protobuf-mode :straight t :defer t)

;;** Database

;;** API

;;*** Restclient
;; TODO: configure restclient
(use-package restclient :defer t
  :straight (:type git :flavor melpa
		   :host github :repo "pashky/restclient.el"
		   :files ("restclient.el" "restclient-pkg.el" "restclient.jq")))

;; (use-package ob-restclient :after restclient :straight t :defer t
;;   :config
;;   (require 'ob-restclient)
;;   (if-let* ((lang (and  'restclient))
;;             (add-to-list 'dc/org-babel-load-languages '(restclient . t))
;;             (dc/org-babel-do-load-languages))))

;; TODO here, add restclient to ob-do-load-lang

;;*** OpenAPI
;; TODO configure openapi-yaml-mode

;; Using straight:
(use-package swagg :straight t :defer t)

;;*** GraphQL
;; TODO configure graphql & ob-graphql
;; ob-graphql requires graphql-mode, which does not depend on graphql
(use-package graphql-mode :straight t :defer t)
(use-package ob-graphql :straight t
  :defer t :after graphql-mode
  :config
  (add-to-list 'dc/org-babel-load-languages '(graphql . t))
  (dc/org-babel-do-load-languages))
;;  (:type git :flavor melpa :host github :repo "jdormit/ob-graphql")

;; (with-eval-after-load ... (in org)...
;; TODO: package: dynamic-graphs?
;; TODO: package: ob-dot

;;** Visualization

;;*** GNU Plot

;;*** Graphviz/Dot

;; TODO setup graphviz
(use-package graphviz-dot-mode :straight t)

;; (setup (:pkg dynamic-graphs :straight t))

;;*** D2

;; diagrams generated from Go binary.

;; TODO: finish babel configuration for d2-mode

;; d2-mode.el sets up basic params for babel integration (there is also ob-d2.el, but
;;  it somewhat overlaps

(defvar dc/d2-snippets-dir "straight/repos/d2-mode/snippets"
  "Path to snippets included with d2-mode.")

(use-package d2-mode :straight t :defer t
  :mode "\\.d2\\'"
  :custom
  (d2-location "/usr/bin/d2")
  ;; d2-tmp-dir ;; tmp files
  ;; d2-flags ""
  (d2-output-format ".svg"))

;; these just need to be brought into snippets dir
;;
;; (with-eval-after-load 'd2-mode
;;   (let* ((d2-snippets (expand-file-name dc/d2-snippets-dir dc/emacs-d)))
;;     (when (file-exists-p d2-snippets)
;;       (add-to-list 'yas-snippet-dirs (expand-file-name dc/d2-snippets-dir dc/emacs-d) t))))

;;*** Mermaid

;; run from docker/podman, nice
;; https://github.com/mermaid-js/mermaid-cli#alternative-installations
(use-package mermaid-mode :straight t :defer t
  ;; also mermaid-mmdc-location, mermaid-flags
  :custom (mermaid-output-format ".svg"))

;; both pkgs define org-babel-execute:mermaid.  ensure ob-mermaid loads after.
;; depending on how straight builds load-path, different functions could run.
;; https://github.com/abrochard/mermaid-mode/blob/master/mermaid-mode.el#L102-L121

;; ob-mermaid basically only provides org-babel-execute:mermaid and formatting
(use-package ob-mermaid :straight t :defer t :after mermaid-mode
  :config
  (add-to-list 'dc/org-babel-load-languages '(mermaid . t))
  (dc/org-babel-do-load-languages))

;; only necessary if (executable-find ...) returns nil
;; :custom (ob-mermaid-cli-path "mmdc")

;;*** PlantUML

;;* Misc

;;** Science

;;*** Chemistry

(use-package smiles-mode :straight t :defer t)

;; last two cmds in ob-smiles.el cause issues. compilation issue?
;;
;; - org-link-set-parameters needs 'ol to be loaded (?)
;; - org-element-map shouldn't be running until blocks eval'd... but idk

;; instead, load explicitly (must run from an org file)

  (use-package ob-smiles :straight t :defer t :after smiles-mode
    :config
    (add-to-list 'dc/org-babel-load-languages '(smiles . t))
    (dc/org-babel-do-load-languages))

;;** Writing

;;*** Kanji

(use-package kanji-mode :straight t :defer t)

;;*** Translation

(use-package google-translate :straight t :defer t
  :custom
  (google-translate-backend-method 'curl)

  ;; (:bind "C-T" #'my-google-translate-at-point)
  (defun google-translate--search-tkk ()
    (list 430675 2721866130))
  (defun my-google-translate-at-point ()
    "reverse translate if prefix"
    (interactive)
    (if current-prefix-arg
        (google-translate-at-point)
      (google-translate-at-point-reverse))))

;; this seems to be doing something similar (google-translate doesn't seem to
;; finish loading, (with-eval-after-load ...) never calls (setup ...)
;; then init blows up in dc-shim.el

(defun dc/ob-translate-setup ()
  (interactive)
  (use-package ob-translate :straight t :defer t :after google-translate
    :config
    (add-to-list 'dc/org-babel-load-languages '(translate . t))
    (dc/org-babel-do-load-languages)))

(provide 'dc-tools)
