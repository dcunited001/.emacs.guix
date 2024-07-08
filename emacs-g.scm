;;* Guix Emacs Packages
(use-modules (ice-9 vlist)
             (ice-9 match)
             ;; (ice-9 pretty-print)
             (guix transformations)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages version-control)
             (gnu packages screen)      ; dtach
             (gnu packages tree-sitter)
             (gnu packages aspell)
             (gnu packages libcanberra)  ; sound-theme-freedesktop
             (gnu packages rust-apps)    ; ripgrep
             (gnu packages haskell-apps) ; shellcheck (req. pandoc to build)
             (gnu packages web)          ; tidy
             (gnu packages python-xyz)
             (ellipsis packages emacs-xyz))

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
      emacs-next-pgtk-debug
      ;; "emacs-next-pgtk"
      emacs-pgtk))

;; TODO: check ./emacs-g.bindir for possible missing bins

;; nss-certs?
(define emacs-pkgs-system
  `(,emacs-pkg
    aspell
    aspell-dict-en
    dtach                               ; detached.el
    git
    git:send-email
    guile-ares-rs
    ripgrep
    shellcheck
    sound-theme-freedesktop
    tidy-html
    tree-sitter

    ;; python?
    python-yamllint
    python-yapf))

;; ;; aspell-dict-la
;; aspell-dict-grc
;; aspell-dict-es
;; aspell-dict-fr
;; aspell-dict-it
;; ;; aspell-dict-ia
;; aspell-dict-de

;; "pinentry-emacs"

;; ---------------------------------------------
;; straight no-clone:
;;
;; emacs-gcmh

;; ---------------------------------------------
;; emacs needed:
;;
;; emacs-use-package
;; emacs-straight-el
;; emacs-rec-mode
;; emacs-a
;; emacs-dash
;;;; emacs-fsm


;; ;;* Guix Packages

;; ;;** To Transform

;; ;; emacs-repo => emacs-repo-no-magit

;; ;; (define transform
;; ;;   (options->transformation
;; ;;    '((with-graft . "emacs-repo=emacs-repo-no-magit"))))

;; ;;** Support
;; ;; "emacs-f"
;; ;; "emacs-ht"
;; ;; "emacs-s"
;; ;; "emacs-ts"
;; ;; "emacs-jq"
;; ;; "emacs-map"

;; ;; auth
;; "emacs-pinentry"
;; "emacs-pass"
;; "emacs-password-store"
;; "emacs-auth-source-pass"

;; ;; completion
;; "emacs-vertico"
;; "emacs-corfu"
;; "emacs-orderless"
;; "emacs-consult"
;; "emacs-consult-yasnippet"
;; "emacs-consult-org-roam"
;; "emacs-consult-xdg-recent-files"
;; "emacs-consult-dir"
;; ;; "emacs-consult-lsp"
;; ;; "emacs-consult-eglot" ;; 0.2.0 does not include fix to #14
;; "emacs-consult-bibtex"
;; "emacs-cape"
;; "emacs-wgrep"
;; "emacs-marginalia"
;; "emacs-embark"

;; ;; "emacs-corfu-terminal"
;; ;; "emacs-corfu-doc-terminal"

;; ;; UI
;; "emacs-hide-mode-line"
;; ;; "emacs-doom-modeline" ; 3.3.2 does not include eglot--spinner fix
;; "emacs-minions"       ;minor mode mgmt: toggle/info
;; "emacs-pulsar"
;; "emacs-hydra"
;; "emacs-avy"
;; "emacs-ace-window"
;; "emacs-buffer-move"
;; "emacs-default-text-scale"
;; "emacs-dired-hacks"
;; "emacs-popper"

;; ;; prettify
;; "emacs-info-plus"
;; "emacs-alert"
;; "emacs-emojify"
;; ;; "emacs-all-the-icons"
;; ;; "emacs-all-the-icons-dired"
;; ;; "emacs-all-the-icons-completion"
;; ;; TODO: readd when guix emacs-kind-icon is current
;; ;; "emacs-kind-icon"
;; "emacs-rainbow-mode"

;; ;;** Keys
;; "emacs-general"
;; "emacs-which-key"

;; ;;** Themes
;; "emacs-ef-themes"
;; ;; NOTE maybe dependencies b/w doom-themes/modeline
;; ;; "emacs-spacegray-theme"
;; ;; "emacs-doom-themes"

;; ;;** Editor
;; "emacs-editorconfig"
;; "emacs-origami-el"
;; "emacs-drag-stuff"
;; "emacs-tmr"

;; ;; "emacs-lispy" ; straight has a much more recent version
;; "emacs-parinfer-mode"
;; "emacs-smartparens"
;; "emacs-rainbow-delimiters"
;; "emacs-highlight-symbol"
;; "emacs-highlight-indent-guides"

;; ;; direnv for buffer-local environments
;; ;; "emacs-buffer-env"
;; "emacs-envrc"

;; "emacs-no-littering"
;; "emacs-posframe"
;; "emacs-keycast"
;; "emacs-super-save"    ;auto-save on activity
;; "emacs-ws-butler"     ;trim
;; ;; "emacs-reformatter"

;; ;; apheleia package doesn't include bin/apheleia-npx
;; ;; "emacs-apheleia"      ;autoformat without jumping
;; "emacs-undo-tree"

;; "emacs-yasnippet"
;; ;; "emacs-doom-snippets"
;; "emacs-yasnippet-snippets"
;; ;; "emacs-emmet-mode"

;; "emacs-visual-fill-column"

;; ;;*** Editor: Straight
;; ;; "emacs-prism"
;; ;; "emacs-origami-el"

;; ;;** Term
;; "emacs-vterm"
;; ;; "emacs-eat"
;; "emacs-bash-completion"
;; "emacs-esh-autosuggest"
;; "emacs-eshell-syntax-highlighting"
;; "emacs-eshell-toggle"
;; "emacs-eshell-z"
;; "emacs-pcmpl-args"                      ;shell completion
;; "emacs-spacegray-theme"

;; ;;** Checkers

;; ;;** VCS
;; "emacs-git-modes"
;; "emacs-gitpatch"
;; "emacs-git-link"
;; ;; "emacs-git-email"
;; "emacs-git-timemachine"

;; ;; until magit-4 is published, use straight packages (also re-enable graphql above)
;; "emacs-magit"
;; ;; "emacs-magit-todos"
;; ;; "emacs-forge"

;; ;;;; "emacs-ghub" ;; propagated by graphql
;; "emacs-srht"

;; ;; transform: "emacs-repo"
;; "emacs-repology"

;; ;;*** Git
;; ;;*** Magit
;; ;;*** Forge

;; ;;** Tools
;; ;; mostly loaded in dc-tools
;; "emacs-burly"

;; ;; bugtracking
;; "emacs-debbugs"

;; ;; linux
;; "emacs-elf-mode"
;; "emacs-syslog-mode"
;; "emacs-dts-mode"
;; "emacs-x509-mode"
;; "emacs-ssh-config-mode"
;; "emacs-systemd-mode"

;; ;; misc
;; "emacs-tldr"

;; ;; processes/services
;; "emacs-prodigy"
;; "emacs-detached"

;; ;; tests
;; "emacs-buttercup"
;; "emacs-esup"

;; ;;*** Firewall

;; ;;*** Tools: Straight
;; ;; "emacs-firestarter"
;; ;; "emacs-x509-mode"
;; ;; "emacs-info-colors"
;; ;; "emacs-crontab-mode"
;; ;; "emacs-magit-tbdiff"
;; ;; "emacs-xdg-paths"
;; ;; "emacs-aurel"                       ;; browse AUR

;; ;;*** Devops
;; '("emacs-terraform-mode"
;;   "emacs-docker"
;;   "emacs-dockerfile-mode")

;; ;;*** Cloud


;; ;;** OS


;; ;;** Tree-sitter
;; ;; "tree-sitter-cli"

;; ;;** Lang

;; ;; GUIX
;; "emacs-guix"

;; ;; nix-mode depends on magit-section;
;; ;; "emacs-nix-mode"

;; ;; GEISER
;; "emacs-geiser"
;; "emacs-geiser-guile"
;; ;; "emacs-geiser-racket"

;; ;; ARES
;; ;; "emacs-eros"          ; req for arei.el
;; ;; "emacs-sesman"        ; req for arei.el
;; ;; ;; "emacs-arei"       ; install with straight

;; ;; "emacs-elisp-refs"
;; "emacs-elisp-demos"

;; ;; "emacs-clojure-mode"
;; ;; ;; "emacs-clojure-snippets"
;; ;; "emacs-cider"
;; ;; ;; "emacs-clj-deps-new"
;; ;; "emacs-clj-refactor"
;; ;; "emacs-parseedn"
;; ;; "emacs-parseclj"

;; ;; "emacs-scala-mode"
;; ;; "emacs-sbt-mode"

;; "emacs-sly"
;; "emacs-sly-asdf"
;; "emacs-sly-macrostep"

;; "emacs-js2-mode"
;; "emacs-js2-refactor-el"
;; "emacs-typescript-mode"
;; ;; "emacs-rjsx-mode"
;; ;; "tide"

;; "emacs-web-mode"
;; "emacs-skewer-mode"

;; "emacs-csv-mode"
;; "emacs-yaml-mode"
;; "emacs-jq-mode"                         ; propagated by emacs-restclient
;; "emacs-json-mode"
;; "emacs-json-snatcher"
;; "emacs-json-reformat"

;; "emacs-protobuf-mode"

;; "emacs-emacsql"

;; ;; "emacs-plantuml-mode"
;; "emacs-graphviz-dot-mode"
;; "emacs-gnuplot"

;; ;; "emacs-graphql"
;; "emacs-graphql-mode"

;; ;; "emacs-eglot"
;; ;; "emacs-lsp-mode"
;; ;; "emacs-lsp-ui"
;; ;; "emacs-dap-mode"

;; "emacs-arduino-mode"

;; "emacs-bazel"

;; ;; KDE
;; "emacs-qml-ts-mode"

;; ;; TODO decide on whether to include CCLS (don't)
;; ;; ;; "ccls"
;; ;; ;; "emacs-ccls"
;; "emacs-go-mode"
;; "emacs-rust-mode"

;; "emacs-markdown-mode"


;; ;;*** Lang: Straight
;; ;; "emacs-sass-mode"
;; ;; "emacs-arduino-cli-mode"

;; ;;*** Python

;; ;; Packages bringing python to the party
;; "python-yamllint"
;; "python-yapf"

;; ;;*** Jupyter
;; "emacs-zmq"
;; "emacs-jupyter"

;; ;;*** Clojure
;; ;; "emacs-clomacs" ?

;; ;;*** Cider

;; ;;*** Scheme

;; ;;*** XML
;; "emacs-esxml"


;; ;;*** Julia

;; ;;*** KDE

;; ;;*** QT

;; ;;*** C++

;; ;;*** Arduino

;; ;;*** Spice

;; ;;*** Graphviz

;; ;;*** PlantUML

;; ;;*** OpenAPI

;; ;;*** Restclient

;; ;;*** GraphQL

;; ;;*** MathJAX

;; ;;*** SMILES (end here)

;; ;;*** Javascript

;; ;;** LSP
;; ;; (append! guix-emacs-packages
;; ;;          '())

;; ;;** Latex

;; "emacs-auctex"
;; "emacs-pdf-tools"
;; "emacs-latex-preview-pane"
;; "emacs-cdlatex"
;; ;; github via straight (upstream git.tecosaur.net; Forgejo)
;; ;; codeblock highlighting with emacs faces
;; ;; some overhead, but low (compared to pygmints/minted)
;; ;; "emacs-engrave-faces"
;; "emacs-parsebib"
;; "emacs-biblio"
;; ;; "emacs-citar"
;; ;; "emacs-citar-org-roam"
;; "emacs-math-symbol-lists"

;; ;;** Org
;; "emacs-org"
;; "emacs-org-modern"
;; "emacs-org-contrib"
;; "emacs-toc-org"
;; "emacs-org-sidebar"

;; "emacs-org-make-toc"
;; "emacs-org-caldav"
;; "emacs-org-present"
;; "emacs-org-appear"

;; ;; org-agenda
;; "emacs-org-ql"
;; "emacs-org-superstar"

;; ;; org-roam
;; "emacs-org-roam"
;; "emacs-org-roam-ui"

;; ;; org-capture
;; ;; TODO: capture templates from web

;; ;; calendar
;; "emacs-org-caldav"

;; ;; ox
;; "emacs-ox-pandoc"                     ; note: requires pandoc
;; "emacs-ox-reveal"
;; "emacs-org-re-reveal"

;; ;; TODO: setup pandoc (usually req pandoc/haskell deps)
;; "emacs-pandoc-mode"               ; for conversions outside of org-mode
;; ;; "emacs-org-pandoc-import" ; dep on pandoc

;; ;; babel
;; ;; "emacs-ob-async"
;; ;; "emacs-restclient"
;; ;; "emacs-ob-restclient"


;; ;;*** Org: Straight

;; ;;*** Org Babel
;; ;;*** Org Capture
;; ;;*** Org Export
;; ;;*** Org Agenda
;; ;;*** Org Roam

;; ;;** Natlang
;; "emacs-spell-fu"
;; ;; "emacs-wordgen" ;very cool, but too dynamic
;; "emacs-kanji"

;; ;; pulls in wordnet as propagated input
;; "emacs-synosaurus"

;; ;;** App
;; "emacs-elpher"
;; "emacs-openwith"

;; ;; guix package, but unsure
;; ;; "emacs-app-launcher"

;; ;;*** Social

;; "emacs-elfeed"
;; "emacs-elfeed-org"
;; ;; "emacs-elfeed-score"
;; "emacs-elfeed-protocol"

;; ;; Social: Straight
;; ;; "emacs-0x0"

;; ;;*** Email

;; ;; n/a
;; ;; "isync"
;; ;; "mu"
;; ;; "emacs-mu4e-alert"
;; ;; "notmuch"
;; ;; "emacs-notmuch"
;; ;; "emacs-notmuch-maildir"

;; ;; "ledger"
;; ;; ;; "hledger"
;; ;; "emacs-ledger-mode"

;; ;;* Manifest

;; ;; n/a, unless improved tab behavior
;; ;; "emacs-perspective"

;; "emacs-xref"
;; "emacs-project"
;; "emacs-jsonrpc"

;; "emacs-projectile"
;; "ripgrep"                       ; For counsel-projectile-rg (doom?) and others

;; ;; "emacs-helpful"

;; "emacs-flycheck"
;; ;; "emacs-flycheck-cpplint"
;; "emacs-flycheck-guile"
;; "emacs-flycheck-package"
;; ;; "emacs-flycheck-plantuml" ;straight
;; ;; "emacs-flycheck-haskell"

;; ;; n/a, evaluate against restclient
;; ;; "emacs-request"

;; ;; "emacs-restclient"
;; ;; "emacs-ob-restclient" ; straight

;; "emacs-xterm-color"
;; "emacs-exec-path-from-shell"

;; "emacs-tracking"

;; ;; n/a (telegram)
;; ;; "emacs-telega"

;; ;; n/a: IRC
;; ;; "emacs-erc"
;; ;; "emacs-erc-image"
;; ;; "emacs-erc-hl-nicks"

;; ;; gopher/gemini?
;; "emacs-elpher"

;; ;; manage daemons/services
;; ;; - does it allow per-project definition of daemons?
;; ;; "emacs-daemons"

;; ;;** excluded
;; ;; without external management of metadata (see org-roam's usage of sqlite),
;; ;; performance concerns are too high
;; ;; "emacs-git-gutter"
;; ;; "emacs-git-gutter-fringe"

;; ;;*** evil
;; ;; "emacs-evil"
;; ;; "emacs-evil-collection"
;; ;; "evil-tutor" (not on guix)
;; ;; "emacs-evil-nerd-commenter"
;; ;; "emacs-evil-org"

;; ;;*** exwm
;; ;; "emacs-exwm"
;; ;; "emacs-desktop-environment"

;; ;;*** media
;; ;; "emacs-emms"
;; ;; "emacs-pulseaudio-control"
;; ;; "emacs-obs-websocket-el"

;; ;;*** unsure
;; ;; "emacs-inheritenv" :: doom dependency
