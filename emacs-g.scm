;;* Guix Emacs Packages
(use-modules (ice-9 vlist)
             (ice-9 match)
             ;; (ice-9 pretty-print)
             (gnu packages guile))

;; export DEBUG=1 when running update-emacs-g
;;
;; the script should build an alternative profile and alternate launch scripts
;; need to be used.
;;
;; hopefully this satisfies the requirements for gdb
;;
;; https://www.reddit.com/r/emacs/comments/rxg6z8/comment/hri3mrs/?context=3
(define emacs-pkg
  ;; NOTE: (define (emacs-29.2 (package (inherit emacs..)))) to bump version

  (if (getenv "DEBUG_EMACS")
      "emacs-next-pgtk-debug"
      ;; "emacs-next-pgtk"
      "emacs-pgtk"))

;;* System
;; A ridiculous way to manage package lists? Yes ...
(define guix-emacs-vhash
  (vhash-consq
   'system
   (list->vlist `("nss-certs"
                  ,emacs-pkg
                  "emacs-setup"
                  "emacs-rec-mode"
                  "git"
                  "git:send-email"
                  "sound-theme-freedesktop"
                  "tidy-html"

                  ;; detached.el
                  "dtach"

                  ;; aspell
                  "aspell"
                  "aspell-dict-en"
                  ;; "aspell-dict-la"
                  "aspell-dict-grc"
                  "aspell-dict-es"
                  "aspell-dict-fr"
                  "aspell-dict-it"
                  ;; "aspell-dict-ia"
                  "aspell-dict-de"))
   vlist-null))

;; TODO: define method to assemble a recursive alist into a package list

;; (define (assemble-pkg-vlist pkg-vhash)
;;   (vhash-fold
;;    (lambda (k v res)
;;      (match v
;;         ;; ((vhash?) (...))
;;         (($ vlist _) (vlist-cons res v))
;;         ((_ foo) (vlist-cons res v))))
;;    '()
;;    pkg-vhash))

(define (assemble-pkg-vlist pkg-vhash)
  (vhash-fold
   (lambda (k v res) (vlist-append res v))
   vlist-null
   guix-emacs-vhash))

;;** Fonts

;; some fontconfig may be necessary.  if it's configured on the base system,
;; then installing there will probobly work hash

;;* Emacs

;;** Support
;; "emacs-f"
;; "emacs-ht"
;; "emacs-s"
;; "emacs-ts"
;; "emacs-jq"
;; "emacs-map"

(set! guix-emacs-vhash
  (vhash-consq
   'support
   (list->vlist '("emacs-a"
                  "emacs-dash"
                  "emacs-fsm"))
   guix-emacs-vhash))

;;** Config
(set! guix-emacs-vhash
  (vhash-consq
   'config
   (list->vlist '("emacs-gcmh"))
   guix-emacs-vhash))

;;*** Input
(set! guix-emacs-vhash
  (vhash-consq
   'input
   (list->vlist '())
   guix-emacs-vhash))

;;*** Auth
(set! guix-emacs-vhash
  (vhash-consq
   'auth
   (list->vlist '(
                  "emacs-pinentry"
                  "pinentry-emacs"
                  "emacs-pass"
                  "emacs-password-store"
                  "emacs-auth-source-pass"))
   guix-emacs-vhash))

;;** Completion
(set! guix-emacs-vhash
  (vhash-consq
   'completion
   (list->vlist '("emacs-vertico"
                  "emacs-corfu"
                  "emacs-orderless"

                  "emacs-consult"
                  "emacs-consult-yasnippet"
                  "emacs-consult-org-roam"
                  "emacs-consult-xdg-recent-files"
                  "emacs-consult-dir"
                  ;; "emacs-consult-lsp"
                  ;; "emacs-consult-eglot" ;; 0.2.0 does not include fix to #14
                  "emacs-consult-bibtex"
                  "emacs-cape"

                  "emacs-wgrep"
                  "emacs-marginalia"
                  "emacs-embark"))
   guix-emacs-vhash))

;; "emacs-corfu-terminal"
;; "emacs-corfu-doc-terminal"

;;** UI
(set! guix-emacs-vhash
  (vhash-consq
   'ui
   (list->vlist '("emacs-hide-mode-line"
                  ;; "emacs-doom-modeline" ; 3.3.2 does not include eglot--spinner fix
                  "emacs-minions"       ;minor mode mgmt: toggle/info
                  "emacs-pulsar"
                  "emacs-hydra"

                  "emacs-avy"
                  "emacs-ace-window"
                  "emacs-buffer-move"

                  "emacs-default-text-scale"

                  "emacs-dired-hacks"
                  ;; "emacs-dired-single" ; n/a

                  "emacs-popper"
                  ))
   guix-emacs-vhash))

;;*** Prettify
(set! guix-emacs-vhash
  (vhash-consq
   'prettify
   (list->vlist '("emacs-info-plus"
                  "emacs-alert"
                  "emacs-emojify"
                  ;; "emacs-all-the-icons"
                  ;; "emacs-all-the-icons-dired"
                  ;; "emacs-all-the-icons-completion"
                  ;; TODO: readd when guix emacs-kind-icon is current
                  ;; "emacs-kind-icon"
                  "emacs-rainbow-mode"))
   guix-emacs-vhash))

;;** Keys
(set! guix-emacs-vhash
  (vhash-consq
   'keys
   (list->vlist '("emacs-general"
                  "emacs-which-key"))
   guix-emacs-vhash))

;;** Themes
(set! guix-emacs-vhash
  (vhash-consq
   'themes
   (list->vlist '("emacs-ef-themes"
                  ;; NOTE maybe dependencies b/w doom-themes/modeline
                  ;; "emacs-spacegray-theme"
                  ;; "emacs-doom-themes"
                  ))
   guix-emacs-vhash))

;;** Editor
(set! guix-emacs-vhash
  (vhash-consq
   'editor

   (list->vlist '("emacs-editorconfig"
                  "emacs-origami-el"
                  "emacs-drag-stuff"
                  "emacs-tmr"

                  ;; "emacs-lispy" ; straight has a much more recent version
                  "emacs-parinfer-mode"
                  "emacs-smartparens"
                  "emacs-rainbow-delimiters"
                  "emacs-highlight-symbol"
                  "emacs-highlight-indent-guides"

                  ;; direnv for buffer-local environments
                  ;; "emacs-buffer-env"
                  "emacs-envrc"

                  "emacs-no-littering"
                  "emacs-posframe"
                  "emacs-keycast"
                  "emacs-super-save"    ;auto-save on activity
                  "emacs-ws-butler"     ;trim
                  ;; "emacs-reformatter"

                  ;; apheleia package doesn't include bin/apheleia-npx
                  ;; "emacs-apheleia"      ;autoformat without jumping
                  "emacs-undo-tree"

                  "emacs-yasnippet"
                  ;; "emacs-doom-snippets"
                  "emacs-yasnippet-snippets"
                  ;; "emacs-emmet-mode"

                  "emacs-visual-fill-column"))
   guix-emacs-vhash))

;;*** Editor: Straight
;; "emacs-prism"
;; "emacs-origami-el"

;;** Term
;; (append! guix-emacs-packages
;;          '())
(set! guix-emacs-vhash
  (vhash-consq
   'term
   (list->vlist '("emacs-vterm"
                  ;; "emacs-eat"
                  "emacs-bash-completion"
                  "emacs-esh-autosuggest"
                  "emacs-eshell-syntax-highlighting"
                  "emacs-eshell-toggle"
                  "emacs-eshell-z"
                  "emacs-pcmpl-args"    ;shell completion
                  "emacs-spacegray-theme"))
   guix-emacs-vhash))

;;** Checkers
;; (append! guix-emacs-packages
;;          '())

;;** VCS

(set! guix-emacs-vhash
  (vhash-consq
   'vcs
   (list->vlist '("emacs-git-modes"
                  "emacs-gitpatch"
                  "emacs-git-link"
                  ;; "emacs-git-email"
                  "emacs-git-timemachine"

                  ;; until magit-4 is published, use straight packages (also re-enable graphql above)
                  ;; "emacs-magit"
                  ;; "emacs-magit-todos"
                  ;; "emacs-forge"

;;;; "emacs-ghub" ;; propagated by graphql
                  "emacs-srht"

                  "emacs-repo"
                  "emacs-repology"
                  ))
   guix-emacs-vhash))

;;*** Git
;;*** Magit
;;*** Forge

;;** Tools
;; mostly loaded in dc-tools
(set! guix-emacs-vhash
  (vhash-consq
   'tools
   (list->vlist '("emacs-burly"

                  ;; bugtracking
                  "emacs-debbugs"

                  ;; linux
                  "emacs-elf-mode"
                  "emacs-syslog-mode"
                  "emacs-dts-mode"
                  "emacs-x509-mode"
                  "emacs-ssh-config-mode"
                  "emacs-systemd-mode"

                  ;; misc
                  "emacs-tldr"

                  ;; processes/services
                  "emacs-prodigy"
                  "emacs-detached"

                  ;; tests
                  "emacs-buttercup"
                  "emacs-esup"))
   guix-emacs-vhash))

;;*** Firewall

;;*** Tools: Straight
;; "emacs-firestarter"
;; "emacs-x509-mode"
;; "emacs-info-colors"
;; "emacs-crontab-mode"
;; "emacs-magit-tbdiff"
;; "emacs-xdg-paths"
;; "emacs-aurel"                       ;; browse AUR

;;*** Devops
(set! guix-emacs-vhash
  (vhash-consq
   'devops
   (list->vlist '("emacs-terraform-mode"
                  "emacs-docker"
                  "emacs-dockerfile-mode"))
   guix-emacs-vhash))

;;*** Cloud
(set! guix-emacs-vhash
  (vhash-consq
   'cloud
   (list->vlist '())
   guix-emacs-vhash))

;;** OS
(set! guix-emacs-vhash
  (vhash-consq
   'os
   (list->vlist '(;; "emacs-xclip"
                  ))
   guix-emacs-vhash))

;;** Tree-sitter
(set! guix-emacs-vhash
  (vhash-consq
   'tree-sitter
   (list->vlist '("tree-sitter"))
   guix-emacs-vhash))
;; "tree-sitter-cli"

;;** Lang
;; (append! guix-emacs-packages
;;          '())

(set! guix-emacs-vhash
  (vhash-consq
   'lang
   (list->vlist '("emacs-nix-mode"

                  ;; GUIX
                  "emacs-guix"

                  ;; GEISER
                  "emacs-geiser"
                  "emacs-geiser-guile"
                  ;; "emacs-geiser-racket"

                  ;; ARES
                  ;; "emacs-eros"          ; req for arei.el
                  ;; "emacs-sesman"        ; req for arei.el
                  ;;; "emacs-arei"       ; install with straight

                  ;; install guile-ares-rs in project/directory.  if
                  ;; arei-mode-auto non-nil, it enables itself on all scheme
                  ;; buffers when required. after emacs init, it can be added to
                  ;; projects, but be wary of potential conflicts if Geiser/Arei
                  ;; access the same Emacs features.

                  ;; For simplicity, avoid Lispy's evaluation functionality,
                  ;; since that expects Geiser. At least some of Lispy's
                  ;; features should work), since

                  ;; "emacs-elisp-refs"
                  "emacs-elisp-demos"

                  ;; "emacs-clojure-mode"
                  ;;; "emacs-clojure-snippets"
                  ;; "emacs-cider"
                  ;;; "emacs-clj-deps-new"
                  ;; "emacs-clj-refactor"
                  ;; "emacs-parseedn"
                  ;; "emacs-parseclj"

                  ;; "emacs-scala-mode"
                  ;; "emacs-sbt-mode"

                  "emacs-sly"
                  "emacs-sly-asdf"
                  "emacs-sly-macrostep"

                  "emacs-js2-mode"
                  "emacs-js2-refactor-el"
                  "emacs-typescript-mode"
                  ;; "emacs-rjsx-mode"
                  ;; "tide"

                  "emacs-web-mode"
                  "emacs-skewer-mode"

                  "emacs-csv-mode"
                  "emacs-yaml-mode"
                  "emacs-jq-mode"       ; propagated by emacs-restclient
                  "emacs-json-mode"
                  "emacs-json-snatcher"
                  "emacs-json-reformat"

                  "emacs-protobuf-mode"

                  "emacs-emacsql"

                  ;; "emacs-plantuml-mode"
                  "emacs-graphviz-dot-mode"
                  "emacs-gnuplot"

                  ;; "emacs-graphql"
                  "emacs-graphql-mode"

                  ;; "emacs-eglot"
                  ;; "emacs-lsp-mode"
                  ;; "emacs-lsp-ui"
                  ;; "emacs-dap-mode"

                  "emacs-arduino-mode"

                  "emacs-bazel"

                  ;; KDE
                  "emacs-qml-ts-mode"

                  ;; TODO decide on whether to include CCLS
                  "ccls"
                  "emacs-ccls"
                  "emacs-go-mode"
                  "emacs-rust-mode"

                  "emacs-markdown-mode"
                  ))
   guix-emacs-vhash))

;;*** Lang: Straight
;; "emacs-sass-mode"
;; "emacs-arduino-cli-mode"

;;*** Python

;; Packages bringing python to the party
(set! guix-emacs-vhash
  (vhash-consq
   'python
   (list->vlist '("python-yamllint"
                  "python-yapf"))
   guix-emacs-vhash))

;;*** Jupyter
(set! guix-emacs-vhash
  (vhash-consq
   'jupyter
   (list->vlist '("emacs-zmq"
                  "emacs-jupyter"))
   guix-emacs-vhash))
;;*** Clojure
;; "emacs-clomacs" ?

;;*** Cider

;;*** Scheme

;;*** XML

(set! guix-emacs-vhash
  (vhash-consq
   'xml
   (list->vlist '("emacs-esxml"))
   guix-emacs-vhash))

;;*** Julia

;;*** KDE

;;*** QT

;;*** C++

;;*** Arduino

;;*** Spice

;;*** Graphviz

;;*** PlantUML

;;*** OpenAPI

;;*** Restclient

;;*** GraphQL

;;*** MathJAX

;;*** SMILES (end here)

;;*** Javascript

;;** LSP
;; (append! guix-emacs-packages
;;          '())

;;** Latex
(define guix-emacs-vhash
  (vhash-consq
   'latex
   (list->vlist '("emacs-auctex"
                  "emacs-pdf-tools"
                  "emacs-latex-preview-pane"
                  "emacs-cdlatex"
                  ;; github via straight (upstream git.tecosaur.net; Forgejo)
                  ;; codeblock highlighting with emacs faces
                  ;; some overhead, but low (compared to pygmints/minted)
                  ;; "emacs-engrave-faces"
                  "emacs-parsebib"
                  "emacs-biblio"
                  ;; "emacs-citar"
                  ;; "emacs-citar-org-roam"
                  "emacs-math-symbol-lists"))
   guix-emacs-vhash))

;;** Org
(set! guix-emacs-vhash
  (vhash-consq
   'org
   (list->vlist '("emacs-org"
                  "emacs-org-modern"
                  "emacs-org-contrib"
                  "emacs-toc-org"
                  "emacs-org-sidebar"

                  "emacs-org-make-toc"
                  "emacs-org-caldav"
                  "emacs-org-present"
                  "emacs-org-appear"

                  ;; org-agenda
                  "emacs-org-ql"
                  "emacs-org-superstar"

                  ;; org-roam
                  "emacs-org-roam"
                  "emacs-org-roam-ui"

                  ;; org-capture
                  ;; TODO: capture templates from web

                  ;; calendar
                  "emacs-org-caldav"

                  ;; ox
                  "emacs-ox-pandoc"     ; note: requires pandoc
                  "emacs-ox-reveal"
                  "emacs-org-re-reveal"

                  ;; TODO: setup pandoc (usually req pandoc/haskell deps)
                  "emacs-pandoc-mode"   ; for conversions outside of org-mode
                  ;; "emacs-org-pandoc-import" ; dep on pandoc

                  ;; babel
                  ;; "emacs-ob-async"
                  ;; "emacs-restclient"
                  ;; "emacs-ob-restclient"
                  ))
   guix-emacs-vhash))

;;*** Org: Straight

;;*** Org Babel
;;*** Org Capture
;;*** Org Export
;;*** Org Agenda
;;*** Org Roam

;;** Natlang
(set! guix-emacs-vhash
  (vhash-consq
   'natlang
   (list->vlist '("emacs-spell-fu"
                  ;; "emacs-wordgen" ;very cool, but too dynamic
                  "emacs-kanji"

                  ;; pulls in wordnet as propagated input
                  "emacs-synosaurus"))
   guix-emacs-vhash))

;;** App
(set! guix-emacs-vhash
  (vhash-consq
   'app
   (list->vlist '("emacs-elpher"
                  "emacs-openwith"))
   guix-emacs-vhash))

;; guix package, but unsure
;; "emacs-app-launcher"

;;*** Social
(set! guix-emacs-vhash
  (vhash-consq
   'social
   (list->vlist '("emacs-elfeed"
                  "emacs-elfeed-org"
                  ;; "emacs-elfeed-score"
                  "emacs-elfeed-protocol"))
   guix-emacs-vhash))

;; Social: Straight
;; "emacs-0x0"

;;*** Email

;; n/a
;; "isync"
;; "mu"
;; "emacs-mu4e-alert"
;; "notmuch"
;; "emacs-notmuch"
;; "emacs-notmuch-maildir"

;; "ledger"
;; ;; "hledger"
;; "emacs-ledger-mode"

(specifications->manifest

 (append
  (vlist->list
   (assemble-pkg-vlist guix-emacs-vhash))
  '(

    ;; n/a, unless improved tab behavior
    ;; "emacs-perspective"

    "emacs-project"
    "emacs-projectile"
    "ripgrep"                     ; For counsel-projectile-rg (doom?) and others

    ;; "emacs-helpful"

    "emacs-flycheck"
    ;; "emacs-flycheck-cpplint"
    "emacs-flycheck-guile"
    "emacs-flycheck-package"
    ;; "emacs-flycheck-plantuml" ;straight
    ;; "emacs-flycheck-haskell"

    ;; n/a, evaluate against restclient
    ;; "emacs-request"

    ;; "emacs-restclient"
    ;; "emacs-ob-restclient" ; straight

    "emacs-xterm-color"
    "emacs-exec-path-from-shell"

    "emacs-tracking"

    ;; n/a (telegram)
    ;; "emacs-telega"

    ;; n/a: IRC
    ;; "emacs-erc"
    ;; "emacs-erc-image"
    ;; "emacs-erc-hl-nicks"

    ;; gopher/gemini?
    "emacs-elpher"

    ;; manage daemons/services
    ;; - does it allow per-project definition of daemons?
    ;; "emacs-daemons"

    )))

;;** excluded
;; without external management of metadata (see org-roam's usage of sqlite),
;; performance concerns are too high
;; "emacs-git-gutter"
;; "emacs-git-gutter-fringe"

;;*** evil
;; "emacs-evil"
;; "emacs-evil-collection"
;; "evil-tutor" (not on guix)
;; "emacs-evil-nerd-commenter"
;; "emacs-evil-org"

;;*** exwm
;; "emacs-exwm"
;; "emacs-desktop-environment"

;;*** media
;; "emacs-emms"
;; "emacs-pulseaudio-control"
;; "emacs-obs-websocket-el"

;;*** unsure
;; "emacs-inheritenv" :: doom dependency
