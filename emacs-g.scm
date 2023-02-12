;;* Guix Emacs Packages
(use-modules (ice-9 vlist)
             (ice-9 match))

;;* System
;; A ridiculous way to manage package lists? Yes ... Reduction in cyclomatic
;; complexity: basically none. Getting to know scheme folding on efficient data
;; structures? ... yeh
(define guix-emacs-vhash
  (vhash-consq
   'system
   (list->vlist '("nss-certs"
                  "emacs-next-pgtk"
                  "emacs-setup"
                  "git"
                  "git:send-email"))
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
;; some fontconfig may be necessary.
;; if it's configured on the base system, then installing there will probobly work
(define guix-emacs-vhash
  (vhash-consq
   'font
   (list->vlist '("font-juliamono"
                  "font-jetbrains-mono"
                  ;; "font-sarasa-gothic" ;iosevka-based
                  "font-iosevka"
                  "font-iosevka-aile"
                  "font-iosevka-slab"
                  "font-iosevka-term"
                  "font-iosevka-term-slab"
                  "font-iosevka-etoile"
                  "font-iosevka-curly"
                  "font-iosevka-curly-slab"))
   guix-emacs-vhash))

;;* Emacs

;;** Config
(define guix-emacs-vhash
  (vhash-consq
   'config
   (list->vlist '("emacs-better-defaults"


                  ))
   guix-emacs-vhash))

;;*** Input
(define guix-emacs-vhash
  (vhash-consq
   'input
   (list->vlist '())
   guix-emacs-vhash))

;;*** Auth
(define guix-emacs-vhash
  (vhash-consq
   'auth
   (list->vlist '(
                  "emacs-pinentry"
                  "pinentry-emacs"
                  "emacs-password-store"
                  "emacs-auth-source-pass"))
   guix-emacs-vhash))

;;** Completion
(define guix-emacs-vhash
  (vhash-consq
   'completion
   (list->vlist '("emacs-vertico"
                  "emacs-corfu"
                  "emacs-corfu-doc"
                  "emacs-orderless"
                  "emacs-consult"
                  "emacs-wgrep"
                  "emacs-marginalia"
                  "emacs-embark"))
   guix-emacs-vhash))

;; "emacs-corfu-terminal"
;; "emacs-corfu-doc-terminal"

;;** UI
(define guix-emacs-vhash
  (vhash-consq
   'ui
   (list->vlist '("emacs-doom-modeline" ;modelline
                  "emacs-minions"       ;minor mode mgmt: toggle/info
                  "emacs-hydra"

                  "emacs-avy"
                  "emacs-ace-window"

                  "emacs-default-text-scale"

                  "emacs-dired-hacks"
                  ;; "emacs-dired-single" ; n/a

                  ))
   guix-emacs-vhash))

;;*** Prettify
(define guix-emacs-vhash
  (vhash-consq
   'prettify
   (list->vlist '("emacs-alert"
                  "emacs-emojify"
                  "emacs-all-the-icons"
                  "emacs-all-the-icons-dired"
                  "emacs-rainbow-mode"
                  "emacs-kind-icon"))
   guix-emacs-vhash))

;;** Keys
(define guix-emacs-vhash
  (vhash-consq
   'keys
   (list->vlist '("emacs-general"))
   guix-emacs-vhash))

;;** Themes
(define guix-emacs-vhash
  (vhash-consq
   'themes
   (list->vlist '("emacs-doom-themes"
                  "emacs-spacegray-theme"
                  "emacs-ef-themes"
                  ))
   guix-emacs-vhash))


;;** Editor
(define guix-emacs-vhash
  (vhash-consq
   'editor
   (list->vlist '("emacs-origami-el"
                  "emacs-drag-stuff"
                  "emacs-a"
                  "emacs-tmr"

                  "emacs-lispy"
                  ;; emacs-prism
                  "emacs-parinfer-mode"
                  "emacs-smartparens"
                  "emacs-rainbow-delimiters"

                  ;; direnv for buffer-local environments
                  "emacs-buffer-env"
                  "emacs-no-littering"
                  "emacs-posframe"
                  "emacs-keycast"
                  "emacs-super-save"    ;auto-save on activity
                  "emacs-ws-butler"     ;trim
                  "emacs-apheleia"      ;autoformat without jumping
                  "emacs-undo-tree"

                  "emacs-yasnippet"
                  "emacs-doom-snippets"
                  ;; "emacs-yasnippet-snippets"

                  "emacs-emmet-mode"

                  "emacs-visual-fill-column"
                  ))
   guix-emacs-vhash))

;;** Term
;; (append! guix-emacs-packages
;;          '())
 (define guix-emacs-vhash
  (vhash-consq
   'term
   (list->vlist '("emacs-vterm"
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

(define guix-emacs-vhash
  (vhash-consq
   'vcs
   (list->vlist '("emacs-git-modes"
                  "emacs-gitpatch" ;; TODO: configure
                  "emacs-git-link" ;; TODO: configure
                  ;; "emacs-git-email" ;;TODO: consider


                  ;; "emacs-git-auto-commit-mode" ;; TODO: consider

                  "emacs-magit"
                  "emacs-magit-todos"

                  "emacs-forge"
                  ))
   guix-emacs-vhash))

;;*** Git
;;*** Magit
;;*** Forge

;;** Tools
;; mostly loaded in dc-tools
(define guix-emacs-vhash
  (vhash-consq
   'tools
   (list->vlist '(
                 "emacs-ef-themes"
                 "emacs-burly"
                 "emacs-elf-mode"
                 "emacs-ssh-config-mode"
                 "emacs-tldr"
                 "emacs-git-timemachine"))
   guix-emacs-vhash))

;;*** Firewall

;;*** Tools: Straight
;; "emacs-firestarter"
;; "emacs-x509-mode"
;; "emacs-info-colors"
;; "emacs-crontab-mode"
;; "emacs-magit-tbdiff"
;; "emacs-xdg-paths"

;;*** Devops
(define guix-emacs-vhash
  (vhash-consq
   'devops
   (list->vlist '("emacs-docker"
                  "emacs-docker-tramp"
                  "emacs-dockerfile-mode"))
   guix-emacs-vhash))

;;*** Cloud
(define guix-emacs-vhash
  (vhash-consq
   'cloud
   (list->vlist '())
   guix-emacs-vhash))

;;** OS
;; (append! guix-emacs-packages
;;          '())


;;** Lang
;; (append! guix-emacs-packages
;;          '())


(define guix-emacs-vhash
  (vhash-consq
   'lang
   (list->vlist '("emacs-guix"
                  "emacs-geiser"

                  "emacs-sly"
                  "emacs-sly-asdf"

                  "emacs-js2-mode"
                  "emacs-typescript-mode"

                  "emacs-web-mode"
                  "emacs-skewer-mode"

                  "emacs-yaml-mode"

                  "emacs-eglot"
                  "emacs-lsp-mode"
                  "emacs-lsp-ui"
                  "emacs-dap-mode"

                  "ccls"
                  "emacs-ccls"
                  "emacs-go-mode"
                  "emacs-rust-mode"

                  "emacs-markdown-mode"
                  ))
   guix-emacs-vhash))


;;*** Clojure

;;*** Cider

;;*** Scheme

;;*** XML

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


;;*** Latex

;;*** MathJAX

;;*** SMILES (end here)

;;*** Javascript

;;** LSP
;; (append! guix-emacs-packages
;;          '())


;;** Org
(define guix-emacs-vhash
  (vhash-consq
   'org
   (list->vlist '("emacs-org"

                 "emacs-org-pomodoro" ;; TODO: remove
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
                 ;; "emacs-org-caldav"
                 ))
   guix-emacs-vhash))

;;*** Org Babel
;;*** Org Capture
;;*** Org Export
;;*** Org Agenda
;;*** Org Roam

;;** App
(define guix-emacs-vhash
  (vhash-consq
   'app
   (list->vlist '("emacs-elpher"
                  "emacs-openwith"))
   guix-emacs-vhash))

;; guix package, but unsure
;; "emacs-app-launcher"

;;*** Social
(define guix-emacs-vhash
  (vhash-consq
   'social
   (list->vlist '("emacs-elfeed"
                  "emacs-elfeed-org"
                  "emacs-elfeed-protocol"))
   guix-emacs-vhash))

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

;;** Config



;;**
;; (append! guix-emacs-packages
;;          '())

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

    "emacs-helpful"

    "emacs-flycheck"

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

    "emacs-elfeed"

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
