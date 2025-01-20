;; -*- lexical-binding: t; -*-

;; NOTE: straight is just building everything right now. it's probably easier that way

;;* Init

;; gv -> macroexp: required for `setf'
;; env

;;** Native Comp

;; This would break every other emacs profile loaded.

;; make the directory in 'dc-straight
(defvar dc/emacs-eln-cache (file-name-as-directory (expand-file-name "eln-cache" user-emacs-directory)) "") 
(let* ((-other-dir-eln (file-name-directory (directory-file-name (car native-comp-eln-load-path)))))
  (unless (eq -other-dir-eln dc/emacs-eln-cache)
	  (setf (car native-comp-eln-load-path) dc/emacs-eln-cache)))
;; (setq test-nc (copy-sequence native-comp-eln-load-path))

(setq-default native-comp-async-report-warnings-errors nil)

;;*** ELN Cache Fix

;; TODO: check for byte-compile
(defun dc/native-comp-state (&optional feat)
  (mapcar
   (lambda (sym)
     (let* ((libsym sym)
	          (lib (symbol-name libsym))
	          libname libeln)
       ;; FIXME (libname file-error "Can't find library" "lcms2")
       (when (setq libname (condition-case err
			                         (find-library-name lib)
			                       (error err)))
         ;; FIXME (libeln wrong-type-argument stringp
	       ;; (file-error "Can't find library" "multi-tty"))
	       (setq libeln (condition-case err
			                    (comp-el-to-eln-filename libname)
			                  (error err))))
       `(,libsym :lib ,lib :libname ,libname :libeln ,libeln)))
   (or feat features)))

;; using this method could end up being more complicated, depending on
;; how the eln binary's expect to interoperate. Load order could cause
;; issues:
;;
;; - if earlier higher-order built-in packages loaded first, then
;; - later lower-order packages are rebuilt by straight the first
;; - packages may not have their `elc' or `eln' updated

(defvar dc/native-comp-state-on-load (dc/native-comp-state)
  "List of emacs `features' with native-comp details.")

(defvar dc/straight-built-in-pseudo-packages '()
  "list of packages to append to `straight-built-in-pseudo-packages'")

(defvar dc/straight-to-build '() ;; maybe project, elgot
  "list of packages to append to `straight-to-build'")

(defvar dc/straight-not-build
  (mapcar #'car dc/native-comp-state-on-load)
  "list of packages to append to `straight-not-build'")

;;** GC Setup

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; The default is 800 kilobytes.  Measured in bytes.
;; hitting this 30 times in startup with 50 MB
;; 27 with 100 MB

;; (setq gc-cons-threshold (* 100 (expt 2 20)))
(setq gc-cons-threshold most-positive-fixnum)

;;** System Identification

;; DOOM: ./lisp/core/doom.el
;;; Global constants
(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))
(defconst EMACS28+    (> emacs-major-version 27))
(defconst EMACS29+    (> emacs-major-version 28))
(defconst MODULES     (featurep 'dynamic-modules))
(defconst NATIVECOMP  (featurep 'native-compile))
(defconst DBUS_FOUND (not (null (getenv "DBUS_SESSION"))))

;;** Essential Vars

;;*** User

(setq-default user-full-name "David Conner"
              user-mail-address (or (getenv "EMAIL") "noreply@te.xel.io"))

;;*** Emacs Config

;;**** Paths
;;; dc/emacs-chemacs (expand-file-name "~/.emacs.d/")
(defvar dc/emacs-d (expand-file-name "~/.emacs.g/") "TODO: docs.")
(defvar dc/emacs-cache (expand-file-name "~/.cache/emacs/") "TODO: docs.")
(defvar dc/emacs-dw (expand-file-name "dw" dc/emacs-d) "TODO: docs.")
;; dc/emacs-doom-modules (expand-file-name "doom/modules" dc/emacs-d)
(defvar dc/emacs-modules (expand-file-name "modules" dc/emacs-d) "TODO: docs.")

;; Add configuration modules to load path
(add-to-list 'load-path dc/emacs-dw)
(add-to-list 'load-path dc/emacs-modules)

;;** Straight

;; TODO: try to move this up higher to avoid potential problems

(require 'dc-straight)

;; =============================================
;;
;;* Phase 1
;; ---------------------------------------------

;;** Common Deps From Straight
;;** Common Deps

;; load early to ensure a consistent dependency graph for later loads
;; this would make tweaking compilation simpler later on

(use-package compat :straight t :demand t)

;;*** Built-in Pkgs
(use-package map :straight (:type built-in) :demand t)
(use-package derived :straight (:type built-in) :demand t)

(use-package ansi-term :straight (:type built-in) :defer t
  :custom
  (ansi-color-for-compilation-mode
   t "Needs ansi-color-process-output ⊂
compilation-filter-hook. Default")
  (ansi-color-for-comint-mode
   t "Needs ansi-color-process-output ⊂
comint-output-filter-functions. Default")
  (ansi-color-bold-is-bright
   t "\"combining ANSI bold and a color produces the bright\""))

;;**** Built-in Support
(use-package xdg :straight (:type built-in) :demand t)

(use-package desktop :straight (:type built-in) :demand t)
(use-package comint :straight (:type built-in) :demand t)
(use-package autoinsert :straight (:type built-in) :demand t
  :config (auto-insert-mode +1))

;; project -> xref ... -> project there's a circular dependency, but project
;; doesn't require xref as a compile-time dependency...
(use-package project :straight t :demand t)
(use-package xref :straight (:type built-in) :demand t)
;; (use-package xref :straight (:type built-in) :demand t)
;;
;; ... but straight starts with the PackageRequires header, which will build
;; .elc & .eln using a different version of xref anyways.



;; flymake -> project (eglot will break if project loads first)
;; ..... unfortunately, since the emacs eln code is already compiled,
;; i believe this is causing an issue.
(use-package flymake :straight (:type built-in) :defer t)

;;**** Built-in Major Modes

(use-package nxml-mode :straight (:type built-in) :defer t)
(use-package python-mode :straight (:type built-in) :defer t)

;; (use-package fdsa :straight (:type built-in) :defer t)

;;*** External Pkgs

(use-package a :straight t :demand t)
(use-package dash :straight t :demand t)
(use-package f :straight t :demand t)

;; ---------------------------------------------

;;*** Delight

(use-package diminish :straight t :demand t)
(use-package delight :straight t :demand t)

;;*** No Littering

(use-package no-littering :straight t
  :demand t
  :config
  (defalias 'dc/emacs-etc #'no-littering-expand-etc-file-name)
  (defalias 'dc/emacs-var #'no-littering-expand-var-file-name))

;;** Customizations

;; TODO setup basics for defgroup

;; :group GROUP

;; :version VERSION :package-version (PKG . VERSION) ; the second has precedence
;;
;; :tag LABEL ; use label instead of item's name
;;
;; :load ; load a specific file before display customization buffer
;;

;; LINK-DATA may contain a tag, but this needs to be the first arg
;;
;; (info-link :tag "foo" "(emacs)Top")

;; :link LINK-DATA ; this is great (reduces comments!)
;;
;; (custom-manual INFO-NODE) ; INFO-NODE specifies Info node: e.g. "(emacs)Top".
;; (info-link INFO-NODE) ; ‘custom-manual’, but show link with the Info node name.
;; (url-link URL)
;; (emacs-commentary-link LIBRARY) ; Link to commentary section of LIBRARY.
;; (emacs-library-link LIBRARY) ; Link to an Emacs Lisp LIBRARY file.
;; (file-link FILE)
;; (function-link FUNCTION)
;; (variable-link VARIABLE)
;; (custom-group-link GROUP)  ; Link to another GROUP.

;;*** Features
(setq desktop-dirname (file-name-concat no-littering-var-directory "desktop/")
      bookmark-default-file (file-name-concat no-littering-var-directory "bookmarks.el")
      tabspaces-session-file (file-name-concat no-littering-var-directory "tabsession.el"))

(require 'dc-support)
(require 'dc-util)

(defvar dc/eld-path (thread-last no-littering-etc-directory
                                 (expand-file-name "dc")
                                 (file-name-as-directory)) "TODO: doc.")

;;*** dw/system-settings

;; load emacs settings for system (from dotfiles)
;; expects to (require 'map)

(require 'dw-settings)

;;*** Custom.el (from daviwil's config)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
;; (load custom-file t)

;; ---------------------------------------------
;; garbage collection: gcmh.el
(use-package gcmh :straight t
  :demand t
  :delight
  :init
  (setq gcmh-idle-delay 'auto
	      gcmh-idle-delay-factor 10
	      gcmh-high-cons-threshold (* 16 (expt 2 20)))
  ;; with depth -75 (early) ... doom loads on first frame
  ;; (add-hook 'server-after-make-frame-hook #'gcmh-mode -75)
  (add-hook 'emacs-startup-hook #'gcmh-mode 25))


;; TODO: rectify user-emacs-* variables: the no-littering package is set from
;; these. they need to be set before, but it's variables aren't affected by
;; them. emacs sets user-emacs-directory on startup. no-lit may get a different
;; value or something else is changing them (chemacs2?)

;; (setq user-emacs-directory "~/.local/share/emacs/"
;;       user-emacs-data-directory "~/.local/share/emacs/"
;;       user-emacs-lisp-directory "~/.local/share/emacs/lisp"
;;       user-emacs-cache-directory "~/.cache/emacs/"
;;       user-emacs-config-directory "~/.config/emacs/"
;;       user-emacs-ensime-directory "~/.emacs.g/var/ensime/")

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
;; (setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
;;       url-history-file (expand-file-name "url/history" user-emacs-directory))

;;*** Early Keys

;;*** Core Key Bindings

(use-package which-key :straight t
  :after general
  :init
  (setq which-key-idle-delay 2.0
        which-key-idle-secondary-delay 0.05
        which-key-lighter "│WK"
	      which-key-side-window-location 'bottom ;; 'top
        which-key-frame-max-height 20          ; default
        which-key-min-display-lines 8
        which-key-popup-type 'side-window ; default
        ;; also: 'custom 'frame (not a child frame for me)
        ;; 'minibuffer (probably inconsistent with consult)
        which-key-special-keys
        '(
          ;; regexp cause problems here "*" "^"
          "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" ;; "-" "="
          ;; "10" "11" "12" ;; bc function keys
          ;; and the vowels (to visually space)
          "a$" "e" "i" "o" "u" "y"
          "A$" "E" "I" "O" "U" "Y")
        which-key-highlighted-command-list
        `(
          (,(rx string-start (or "consult-" "embark-")) 'ef-themes-heading-8)
          (,(rx string-start (or "org-clock-")) 'ef-themes-heading-8)))
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

(use-package general :straight t :demand t)

;;*** Early Packages

;; TODO: maybe configure eglot defaults
;; (setq-default eglot-workspace-configuration '(:lsp-server-key (:config ...)))

;;**** Project

;; maybe move project.el here

;;**** Conf Mode
;; load this early, so it messes with auto-mode-alist less
(use-package conf-mode :straight (:type built-in))

;;**** Eglot

;; Configure c-mode to run ccls with podman.........
;;
;; https://git.sr.ht/~bkhl/dotfiles/tree/main/item/.emacs.d/configuration.org#L1009-1022
;;
;; and also go-ts-mode
;;
;; https://git.sr.ht/~bkhl/dotfiles/tree/main/item/.emacs.d/configuration.org#L1059-1073
;;
;; does this work?

(setq dc/eglot-events-buffer-configs
      `((default . (:size 2000000 :format full))
        (debug . (:size ,(expt 2 (+ 2 4 20)) :format json))))

(use-package eglot :straight t
  ;; (:with-hook eglot-managed-mode-hook
  ;;   (:hook #'dc/eglot-setup-buffer))

  ;; TODO: c-mode-hook is hooked in c-mode-hook?
  ;; (:with-hook c-mode-hook
  ;;   (:hook eglot-ensure))

  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect 1)
  (eglot-connect-timeout 15)
  (eglot-send-changes-idle-time 0.5)
  ;; other common options: xref, imenu, eldoc
  ;; also see (eglot--setq-saving...)

  ;; NOTE: This fucks up flymake buffers everywhere:
  ;; (eglot-stay-out-of '(flymake)) ;; it's a per-project setting

  (eglot-extend-to-xref t)              ;TODO: assess eglot-extend-to-xref
  ;; (eglot-menu-string "Æ")

  (eglot-events-buffer-config
   (alist-get 'debug dc/eglot-events-buffer-configs))

  ;; see note about popups/point in
  ;; .emacs.doom/modules/tools/lsp/+eglot.el
  ;; (eglot-auto-display-help-buffer nil)
  (eglot-confirm-server-initiated-edits 'confirm)

  :config
  (add-to-list
   ;; TODO: Is this needed now?
   'eglot-server-programs
   '((js2-mode typescript-mode) .
     ("typescript-language-server" "--stdio")))

  (add-to-list
   'eglot-server-programs
   `((html-ts-mode mhtml-mode web-mode) .
     ,(a-get* eglot-server-programs 'html-mode)))

  (add-to-list
   'eglot-server-programs
   '(python-mode . ("pylsp")))

  (add-to-list
   'eglot-server-programs
   '((ansible-mode) .
     ("ansible-language-server" "--stdio")))

  ;; '(astro-ts-mode
  ;;   . ("astro-ls" "--stdio"
  ;;      :initializationOptions
  ;;      (:typescript (:tsdk "./node_modules/typescript/lib"))))
  (add-to-list 'eglot-server-programs
               '(astro-ts-mode
                 "astro-ls" "--stdio"
                 :initializationOptions
                 (:typescript (:tsdk "./node_modules/typescript/lib"))))

  ;; about to configure this for yaml, but it needs yaml schema-specific glob
  ;; patterns anyways

  ;; increase process read size from 4k to 512K
  ;; see: https://emacs-lsp.github.io/lsp-mode/page/performance/#tuning
  ;; and: https://github.com/doomemacs/doomemacs/blob/master/lisp/doom-start.el#L77
  (setq read-process-output-max (expt 2 19)))

;;*** Repo Paths

;; TODO: generally replace the dc/*-vars with defvar or ... maybe defcustom, but
;; that requires defgroup (must be dc-*-vars afaik or you brings chaos to the
;; emacs world)

;; comments are such bad, docs are wow and var lookups yay
(defvar dc/ecto-path (or (getenv "_ECTO") (expand-file-name "~/ecto"))
  "Directory where git-repo projects are checked out.")
(defvar dc/repo-path (or (getenv "_REPO") (expand-file-name "~/repo"))
  "Directory containing XML for git-repo projects are checked out.")
(defvar dc/lang-path (or (getenv "_LANG") (expand-file-name "~/lang"))
  "Directory containing quick projects under ./lang. It typically
contains config under ./.lang to encourage native and portable
12factor language configs, when not container.")

;;*** Guix/Geiser Paths

(defvar dc/guix-checkout-path (getenv "GUIX_SOURCE")
  "Directory containing a guix checkout. The .dir-locals.el in Guix
should be used for setting guix-load-path unless working on
checkouts of channels which depend on modified packages in the
Guix channel.")

(defvar source-directory (getenv "EMACS_SOURCE")
  "Directory containing the ./src directory of an Emacs checkout.")

(unless source-directory (warn "Emacs: source-directory is not set"))
(unless dc/guix-checkout-path (warn "Emacs: source-directory is not set"))

;; TODO: update service definitions and settle on environment variables

;; =============================================
;; TODO: how to bring in emacs-geiser/guile and emacs-guix?

(defun dc/guix-profile-get-default-path ()
  (expand-file-name "emacs-g/emacs-g" (getenv "GUIX_EXTRA")))

(defconst dc/guix-startup-profile-path
  (getenv "GUIX_ENVIRONMENT")
  "The original `GUIX_ENVIRONMENT' value on startup." )

(defconst emacs-sound-theme-path
  (expand-file-name "share/sounds/freedesktop/stereo" dc/guix-startup-profile-path))

;; TODO: maybe change this
(defvar dc/guix-profile-path
  (or (getenv "GUIX_ENVIRONMENT") (dc/guix-profile-get-default-path)))

;; (defun dc/guix-guile-paths (&optional profile-path)
;;   "Return `load-path' and `load-compiled-path' for a guix
;;  `profile-path'"

;;   ;; each of these has a method ending with "?" except guix-system-profile
;;   ;;
;;   ;; guix-profile\??, guix-home-profile\??, guix-user-profile\??,
;;   ;; guix-pulled-profile\??, guix-system-profile
;;   ;;
;;   ;;  (lacks ? method)
;;   (let ((profile-path (or profile-path guix-pulled-profile)))
;;     `((load-path . ,(expand-file-name "share/guile/3.0/site"
;;                                       profile-path))
;;       (compiled-load-path . ,(expand-file-name "lib/guile/3.0/site-ccache"
;;                                                profile-path)))))

;; ;; NOTE guile-load-compiled-path not needed when .scm and .go are in the same
;; ;; directory. this happens in a guix checkout, but not for the channels.
;; (defun dc/guix-reset-paths ()
;;   (setq-default guix-load-path (list (expand-file-name "share/guile/site/3.0"
;;                                                        guix-pulled-profile))
;;                 guix-load-compiled-path (list (expand-file-name "lib/guile/3.0/site-ccache"
;;                                                                 guix-pulled-profile))))

;;*** Org

;; ;; this points to the profile for `guix shell`
(setq-default dc/guix-profile-path (or (getenv "GUIX_ENVIRONMENT")
                                       (dc/guix-profile-get-default-path))
              dc/emacs-sound-theme-path (file-name-as-directory
                                         (expand-file-name
                                          "share/sounds/freedesktop/stereo"
                                          dc/guix-profile-path)))

;;*** Org Paths

(setq-default org-directory (file-name-as-directory (or (getenv "ORG_DIRECTORY") "/data/org"))
              org-roam-file-extensions '("org")
              org-roam-directory (or (and (boundp 'org-roam-directory) org-roam-directory) "roam")
              org-roam-directory (thread-first org-roam-directory
                                               (expand-file-name org-directory)
                                               (file-truename)
                                               (file-name-as-directory))


              ;; gets set by no-littering anyways
              ;; org-roam-db-location (file-name-concat no-littering-var-directory "org" "org-roam.db")

              dc/org-roam-n-dailies 5
              dc/org-roam-templates-path (expand-file-name "etc/captures/roam" dc/emacs-d)
              dc/org-roam-dailies-template (expand-file-name "daily-default.org" dc/org-roam-templates-path))

;;**** Load Org from Emacs
(use-package org :straight (:type built-in) :demand t
  :custom

  (org-modules
   '(org-id ol-info ol-man ol-bibtex ol-doi org-protocol org-notify ol-gnus org-crypt ox-extra)
   "For descriptions, M-x customize-variable org-modules")

  (org-edit-src-content-indentation 0 "no effect when org-src-preserve-indentation")
  (org-src-fontify-natively t)

  ;; ====== org-babel ======= (move after reorganizing dc-org)
  (org-confirm-babel-evaluate t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)

  ;; default, works pretty well, may obviate the defadvice! below
  ;; (org-src-window-setup 'reorganize-frame)
  (org-src-window-setup 'split-window-below)

  ;; org-confirm-babel-evaluate nil
  (org-link-elisp-confirm-function 'y-or-n-p)
  (org-link-shell-confirm-function 'y-or-n-p)
  ;; =========================
  )

(use-package org-contrib :straight t :after (org) :demand t
  :init
  ;; (add-to-list 'org-modules 'org-screen)
  ;; (add-to-list 'org-modules 'ol-bookmark)
  (add-to-list 'org-modules 'org-eldoc)
  (add-to-list 'org-modules 'org-tempo))

;;*** Org Babel Load Languages

;; this is appended to in dc-dev-*.el, then loaded in dc-shim.el
;;
;; NOTE: at some point, jupyter presented an issue regarding jupyter kernelspecs
;; being required when the symbols are loaded by (org-babel-do-load-languages
;; ...)
;;

;; *** Org Babel Default Langs

;; I guess everything else can be determined here
(setq dc/org-babel-load-languages
      '((emacs-lisp . t)
        (shell . t)
        ;;(rec . t) ;; GNU recutils
        ;;(jq . t)
	      ;;(restclient . t)
        (python . t)))

(defun dc/org-babel-do-load-languages ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   dc/org-babel-load-languages))

;; (defun dc/org-babel-add-load-language (langsym &optional obsym)
;;   (if-let* ((langsym (bound-and-true-p langsym))
;; 	    (langname (symbol-name langsym))
;; 	    (obname (format "ob-%s" langname))
;; 	    ;; TODO check usage of intern
;; 	    (obsym (or obsym
;; 		       (intern-soft langname)
;; 		       (intern langname))))

;;       ;; (let ((obadded? (memq org-babel-load-languages obsym))
;;       ;; 	    (obloaded? (memq org-babel-load-languages obsym)))

;;       ;; NOTE: would prefer a single source of truth (either dc/.. or
;;       ;; org-babel-load-languages).

;;       ;; (unless (memq org-babel-load-languages obsym) (add-to-list
;;       ;; '))

;;   ))

;;*** dc/aca paths

;; Org Ref & Bibtex

;; TODO refactor slim down (auto def symbols, create paths if dc/aca-doc-root exists)
;;
;; - may need to ensure that the doi's exist. a macro would help, but i just
;;   need to determine how the file/db structure would accommodate changes

;; i'm not sure what structure i'll stick with. this is usually what PhD's need
;;   zero help with, of course. the most successful PhD's have no idea what
;;   they're doing here and typically leave notes wherever. doesn't
;;   matter. </joking>

(defvar dc/aca-doc-root (xdg-user-dir "DOCUMENTS") "TODO: doc dc/aca-doc-root")

;; for the 14 official types
;; + see 'org-bibtex-types
;; + or https://bibtex.eu/types/
;;
;; + dc/aca-bibtex-types (list :article :book :techreport :manual)

(setq-default
 dc/aca-subpaths (list "article" "book" "text" "incollection")

 ;; both citar and org-ref want these to end in a slash
 ;; citar magically agrees on on the citekey org-ref uses to create PDF's
 dc/aca-notes-path (expand-file-name "noter/" org-roam-directory)

 dc/aca-texts-directory (expand-file-name "texts/" dc/aca-doc-root)
 dc/aca-texts-bibtex (expand-file-name "noter/texts.bib" org-roam-directory)
 dc/aca-articles-directory (expand-file-name "articles/" dc/aca-doc-root)
 dc/aca-articles-bibtex (expand-file-name "noter/articles.bib" org-roam-directory)
 dc/aca-books-directory (expand-file-name "books/" dc/aca-doc-root)
 dc/aca-books-bibtex (expand-file-name "noter/books.bib" org-roam-directory)
 dc/aca-collections-directory (expand-file-name "collections/" dc/aca-doc-root)
 dc/aca-collections-bibtex (expand-file-name "noter/collections.bib" org-roam-directory)

 dc/aca-library-paths (list dc/aca-texts-directory
                            dc/aca-articles-directory
                            dc/aca-books-directory
                            dc/aca-collections-directory)
 dc/aca-bibtex-files (list dc/aca-texts-bibtex
                           dc/aca-articles-bibtex
                           dc/aca-books-bibtex
                           dc/aca-collections-bibtex))


;; (dolist (el dc/aca-bibtex-files)
;;   (unless (file-exists-p el)
;;     ;; (f-touch el)
;;     (warn "Bibtex: file does not exist %s. See 'dc-bibtex" el)))

;; =============================================

;; ;;*** Require Early Packages

;; (require 'eglot)
;; (require 'project)
;; (require 'xref)
;; (require 'eldoc)
;; (require 'jsonrpc)


;; =============================================
;;
;;* Phase 2
;; ---------------------------------------------

;; ;;** Core


;; ---------------------------------------------
;;*** Minions -- TODO: overlaps in functionality with diminish/delight




;; init early to keep logic close to (require 'package) ... which I may change,
;; since I lose control of the ordering of prominent modes.

;; In that case, i'll just run this and then fixup.
;;
;; `grep -re "(add-to-list 'minions-prominent-modes '.*)" >> ./modules/dc-modelines.el

;; ;; (eq t (xor (minions-demoted (or minions-promoted enabled))))
;; (setq minions-prominent-modes
;;       '(superword-mode
;;         subword-mode
;;         2C-mode
;;         multiple-cursors-mode)
;;       minions-demoted-modes nil)

;; ;;**** Prominent

;; (add-to-list 'minions-prominent-modes 'combobulate-mode)
;; (add-to-list 'minions-prominent-modes 'apheleia-mode)

;; ;; editor
;; (add-to-list 'minions-prominent-modes 'undo-tree-mode)
;; (add-to-list 'minions-prominent-modes 'smartparens-mode)

;; ;; customize overwrite-mode-binary and overwrite-mode-textual
;; (add-to-list 'minions-prominent-modes 'overwrite-mode)
;; (add-to-list 'minions-prominent-modes 'view-mode)

;; ;; org-like
;; (add-to-list 'minions-prominent-modes 'cdlatex-mode)
;; (add-to-list 'minions-prominent-modes 'outline-mode)

;; ;;**** Demoted

;; (add-to-list 'minions-demoted-modes 'rainbow-mode)
;; (add-to-list 'minions-demoted-modes 'rainbow-delimiters-mode)
;; (add-to-list 'minions-demoted-modes 'super-savemode)
;; (add-to-list 'minions-demoted-modes 'ws-butler-mode)

;; ---------------------------------------------

;; ;;*** Core Init

(require 'dw-core)

(defun dc/yasnippet-template-default ()
  (let* ((tmpl-path (dc/emacs-etc "yasnippet/yasdefault")))
	  (with-temp-buffer
	    (insert-file-contents-literally (dc/emacs-etc "yasnippet/yasdefault"))
	    (buffer-string))))

(use-package yasnippet :straight t
  :init
  ;; TODO add file-exists-p (when not in vim) ... and disable yas-global-mode?
  (setq yas-new-snippet-default (dc/yasnippet-template-default))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets :straight t
  :after yasnippet
  ;;  :demand t
  :hook
  ((prog-mode
    org-mode
    LaTeX-mode
    latex-mode
    prog-mode) . yas-minor-mode)
  :config
  (add-to-list 'yas-snippet-dirs dc/guix-checkout-path t))

(require 'dc-network)

;; ;; no backup files
;; (setq make-backup-files nil)

;; ;; NOTE 20240708: this should already load with 'dw-settings
;; ;; (load-file (expand-file-name (concat dc/emacs-chemacs "per-system-settings.el")))

;;** UI

;; (require 'dc-terminal)
(require 'dc-desktop)
(require 'dc-alert)
(require 'dc-project)
(require 'dc-interface)
(require 'dc-popup)
(require 'dc-auth)

;;*** Info

(require 'dc-info)

;;** Org

(require 'dw-org)
;;(require 'dc-org)
;;(require 'dc-bibtex)

;;** Dev

(require 'dc-fly)
;;;; (require 'dw-shell)
(require 'dc-dev)
;;(require 'dc-dev-web)
;;(require 'dw-swagger)
;;(require 'dc-dev-cpp)
;;(require 'dc-dev-java)
;;(require 'dc-dev-clojure)
;;(require 'dc-dev-scala)
;;(require 'dc-dev-python)

;;** System

(require 'dc-vcs)
(require 'dc-tools)
;;(require 'dc-dev-yaml)

;;(require 'dc-latex)

;;;; (require 'dc-workflow)

;;** Apps

;;(require 'dc-social)
;;;; (require 'dc-applications)
;;;; (require 'dw-media)
;;;; (require 'dw-system)
;;;; (require 'dc-games)

;;** Keys & Mouse

;;(require 'dc-keys)
;;(require 'dc-mouse)

;;** Final

;;(require 'dc-modeline)

;;*** Shim

(require 'dc-shim)

;; =============================================

;;**** No ido

(when (featurep 'ido)
  (ido-mode nil))

;;**** Start the Daemon
(server-start)

;; doom hooks gcmh-mode when the first buffer is loaded.
;; (setq gc-cons-threshold (* 50 (expt 2 20)))
(setq gc-cons-threshold (* 16 (expt 2 20)))

(alert "Emacs server ready." :title "Emacs GC:")
