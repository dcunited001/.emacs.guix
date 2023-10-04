;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2023 David Conner
;; Copyright © 2021 David Wilson
;; Copyright © 2014-2022 Henrik Lissner.
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

;;* Keys Pre
;; see ./emacs/$version/lisp/bindings.el for defaults

;;** Setup

(setq mode-line-defining-kbd-macro " K♫ ")
(add-to-list 'minions-prominent-modes 'defining-kbd-macro)

;;*** unbind function keys
;; or use the following (which may only work for general definitions)
;; (general-auto-unbind-keys)

(defvar dc/keymaps-unbound-at-init
  ;; the app doesn't need to tell i3 what to do
  ;; OS/WM functions should use super-key
  '("<f11>"

    ;; unbind 2C-Mode
    ;; i would probably actually use this
    "<f2> 2"
    "<f2> b"
    "<f2> s"
    "<f2> <f2>"

    ;; i honestly don't know how to use these in workflow
    "<f3>"
    "<f4>"

    ;; tab prev/next
    "C-<tab>"
    "C-S-<tab>"

    ;; org-agenda
    "C-x 1"                   ;delete-other-windows (if i want it ,i 'll M-x it)

    ;; unbind menubar shortcuts
    ;; TODO what to do about terminal compat
    "<f10>"                             ;menu-bar-open
    "S-<f10>"                           ;context-menu-open
    "C-<f10>"                           ;buffer-menu-open
    "M-<f10>"                           ;toggle-frame-maximized
    "ESC <f10>"                         ;toggle-frame-maximized

    ;; unbinding from global will yield quick help for which-key
    "<f1> <f1>"                         ;help-for-help (dup keybind)

    ;; bind to quick functions (loading/themes/etc) a la doom
    "<f1> <f2>"                         ;no standard binding

    ;; globally, bound to the same function
    "M-S-<left>"                        ;translate to M/C-<left/right>
    "M-S-<right>"

    "C-x f"                             ;set-fill-column
    ))

;;**** trying to pack a lambda into a symbol
;; (fset 'dc/unbind-key (macroexpand-all '(unbind-key k)))
;; (seq-do (symbol-function 'dc/unbind-key) dc/keys-unbound-at-init)

(defun dc/unbind-keys (key-names &optional keymap)
  (seq-do (lambda (key)
            (if keymap
                (unbind-key key keymap)
              (unbind-key key)))
          key-names))

(dc/unbind-keys dc/keymaps-unbound-at-init)

;; TODO xkb: setup "AltGr-<f_x>" -> "<f_x+12>"
;; and if you buy right now, we'll double your function keys
;; chromebooks and macbooks not applicable

;;* Which Keys

;;** Special Keys

;; NOTE: which-keys-special-keys will assemble a regexp like the following
;;       which, in the which-keys--propertize-key, matches on the "key"

;; "\\(!\\|@\\|&\\|_\\|1\\|2\\|3\\|4\\|5\\|6\\|7\\|8\\| ...

;; "M" "C-M" "C-.+" "H"  ;only highlights commands starting with modkey

;; regexps like [:punct:], \\[:punct:\\], etc don't seem to work (req. pcre afaik)

;; NOTE: which-key-local-map-descriptions-face highlights what's listed
;;       on the (current-local-map) keymap

(setup (:pkg which-key)
  (:option which-key-side-window-location 'bottom ;; 'top
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
  (:with-hook ef-themes-post-load-hook
    (:hook (lambda ()
             (dc/update-face 'which-key-separator-face
                             'epa-string)
             (dc/update-face 'which-key-local-map-description-face
                             'ef-themes-heading-5)))))

;; TODO: many of the overwritten replacements aren't labelled correctly
;; e.g. <f12> for C-c, but I may remove many of these. they are okay, but
;; don't work in console/etc
;; (setq which-key-allow-multiple-replacements t)

;; TODO: define a global minor mode where a *regexp* for the active major mode's
;; prefix is prepended to the which-key lookup

;; (defun dc/learn-good-mode- (rx mode-name)
;;   "highlight the major modes functions in which-key"
;;   ;; use major-mode to get symbol, then extract the prefix, use (rx ...)
;;   ;; prepend to which-key-highlighted-command-list
;; )

;; TODO: custom sorting is possible by creating a new comparator with
;;       potentially a new (performant) which-key--{string,description}<

;; which-key-key-order

;; TODO: customize which-key-group-description-face to include background

;;* Keys

;;** Help

;; NOTE: need to specify both global & help so f1 will substitute as C-h
;;
;; when general new prefixes maps on C-h help, which-key descriptions are not
;; set on <f1> prefixs
;;
;; general.el suggests managing which-key alists directly in some cases
(defun dc/init-keybinds-quick ()
  ;; (dolist (pfx '("C-h" "<f1>")))

  (general-define-key
   :keymaps '(global help)
   :wk-full-keys nil
   :prefix "<f1>"
   "<f2>" '(:ignore t :which-key "QUICK")

   "<f2> a" '(:ignore t :which-key "ALERT")
   "<f2> ao" #'alert--log-open-log
   "<f2> a M-c" #'alert--log-clear-log
   "<f2> ad" #'alert--log-enable-debugging
   "<f2> aD" #'alert--log-disable-debugging
   "<f2> al" #'alert--log-disable-logging
   "<f2> aL" #'alert--log-enable-logging
   "<f2> am" #'alert--log-enable-messaging
   "<f2> aM" #'alert--log-disable-messaging


   "<f2> b" '(:ignore t :which-key "BUFFER ENV")
   "<f2> bd" #'buffer-env-describe
   "<f2> bu" #'buffer-env-update
   "<f2> br" #'buffer-env-reset

   "<f2> d" #'docker

   ;; already accessible from docker
   ;; "<f2> M-d" #'docker-compose

   "<f2> D" '(:ignore t :which-key "DESKTOP")
   "<f2> Ds" #'desktop-save-in-desktop-dir
   "<f2> DS" #'desktop-save
   "<f2> Dr" #'desktop-read

   "<f2> e" #'envrc-command-map
   "<f2> E" '(:ignore t :which-key "ENVRC")
   "<f2> Er" #'envrc-reload-all
   "<f2> Eg" #'envrc-global-mode

   "<f2> h" #'shortdoc

   ;; lookups
   "<f2> l" '(:ignore t :which-key "LOOKUP")
   "<f2> ln" '(:ignore t :which-key "NIST")
   "<f2> lnf" #'nist-webbook-formula
   "<f2> lnn" #'nist-webbook-name

   ;; local variables
   "<f2> M-l" '(:ignore t :which-key "LOCAL VARS")
   "<f2> M-l ad" #'add-dir-local-variable
   "<f2> M-l aF" #'add-file-local-variable
   "<f2> M-l af" #'add-file-local-variable-prop-line
   "<f2> M-l dd" #'delete-dir-local-variable
   "<f2> M-l dF" #'delete-file-local-variable
   "<f2> M-l df" #'delete-file-local-variable-prop-line
   "<f2> M-l k" #'kill-local-variable
   "<f2> M-l m" #'make-local-variable
   "<f2> M-l M" #'make-variable-buffer-local
   "<f2> M-l h" #'apropos-local-variable
   "<f2> M-l H" #'array-display-local-variables

   "<f2> O" #'aw-show-dispatch-help

   ;; "<f2> p"  '(:ignore t :wk "POPUP")
   "<f2> P" #'pomm

   ;; "<f2> #" '(:ignore t :which-key "TRAMP")
   "<f2> T" '(:ignore t :which-key "TRAMP")
   "<f2> Tc" #'tramp-cleanup-connection
   "<f2> TC" #'tramp-cleanup-all-buffers
   "<f2> T M-c" #'tramp-cleanup-all-connections
   "<f2> TM" #'tramp-compat-set-file-modes

   "<f2> r" #'repology

   "<f2> t" '(:ignore t :which-key "THEME")
   "<f2> tr" #'ef-themes-load-random
   "<f2> ts" #'ef-themes-select
   "<f2> tt" #'ef-themes-toggle

   ;; TODO check tramp-completion-use-auth-sources
   "<f2> Tg" #'tramp-crypt-add-directory

   ;; vterm
   "<f2> M-v" #'vterm

   ;; needs tramp-default-rename-alist
   ;; "<f2> Tr" #'tramp-rename-these-files
   ;; "<f2> TR" #'tramp-rename-files
   ;; "<f2> Td" #'tramp-setup-debug-buffer
   ;; #'tramp-change-syntax ;; default/simplified/separate

   "<f2> $" '(:ignore t :which-key "STRAIGHT")

   ;; straight -* utils
   "<f2> $4" #'straight-get-recipe
   "<f2> $$" #'straight-pull-recipe-repositories
   "<f2> $v" #'straight-visit-package
   "<f2> $V" #'straight-visit-website
   "<f2> $d" #'straight-dependencies
   "<f2> $D" #'straight-dependents
   ;; "<f2> $" #'straight-use-package

   ;; straight -package
   "<f2> $F" #'straight-pull-package
   "<f2> $ M-F" #'straight-fetch-package
   "<f2> $P" #'straight-push-package
   "<f2> $C" #'straight-check-package
   "<f2> $R" #'straight-rebuild-package
   "<f2> $M" #'straight-merge-package
   "<f2> $N" #'straight-normalize-package

   ;; straight -and-deps
   "<f2> $&" '(:ignore t :which-key "AND DEPS")
   "<f2> $& M-f" #'straight-fetch-package-and-deps
   "<f2> $&F" #'straight-pull-package-and-deps
   "<f2> $&m" #'straight-merge-package-and-deps

   ;; straight -all
   "<f2> $f" #'straight-pull-all
   "<f2> $ M-f" #'straight-fetch-all
   "<f2> $p" #'straight-push-all
   "<f2> $c" #'straight-check-all
   "<f2> $r" #'straight-rebuild-all
   "<f2> $M" #'straight-merge-all       ;oh boy!
   "<f2> $N" #'straight-normalize-all

   ;; thesaurus
   "<f2> M-t t" #'synosaurus-choose-and-insert
   "<f2> M-t M-t" #'synosaurus-choose-and-insert

   ;; vcs ops: merge, normalize
   ;; pull/fetch/merge-package-and-deps
   ;; freeze/thaw-versions
   ;; use-package-mode
   ;; watcher-start/stop
   "<f2> 0" '(:ignore t :which-key "0x0")
   "<f2> 0-" #'0x0-dwim
   "<f2> 0t" #'0x0-upload-text
   "<f2> 0f" #'0x0-upload-file
   "<f2> 0k" #'0x0-upload-kill-ring
   "<f2> 0p" #'0x0-popup
   "<f2> 0u" #'0x0-shorten-uri)

  (when (featurep 'consult-gh)
    (general-define-key
     :keymaps '(global help)
     :wk-full-keys nil
     :prefix "<f1>"
     ;; use #'embark-select with SPC for embark integration
     "<f2> g" '(:ignore t :which-key "GH")
     "<f2> gc" #'consult-gh-repo-clone
     "<f2> gf" #'consult-gh-find-file
     "<f2> g M-f" #'consult-gh-repo-fork
     "<f2> gi" #'consult-gh-search-issues
     "<f2> go" #'consult-gh-orgs
     "<f2> gr" #'consult-gh-search-repos
     "<f2> gR" #'consult-gh-default-repos)))

(defun dc/init-keybinds-help ()
  (dolist (pfx '("C-h" "<f1>"))
    (general-define-key
     :keymaps '(global help)
     :prefix pfx

     ;; can insert values with embark
     "M-v" #'getenv
     "M-k" #'describe-keymap
     "B" #'embark-bindings
     "M-b" #'embark-bindings-in-keymap
     "M-m" #'consult-minor-mode-menu
     "M-f" #'list-faces-display)))

(general-define-key
 :keymaps '(Info-mode-map)
 "a" #'info-apropos)

;;** Globals

(general-define-key
 :keymaps '(global)

 "C-M-|" #'dc/indent-buffer)

;;*** global-leader-key (C-x, f2)
;; this helps balance keyboard usage, giving and gives your pinky a break

;; this prefix should find itself associated with
;; editor features, global state and outward-looking functions
(global-leader-def
  :keymaps '(global)
  :wk-full-keys nil

  ;; C-u commands very useful!
  "o" #'ace-window
  "C-d" #'consult-dir

  ;; "C-x M-f" #'set-fill-column

  "M-f" #'find-file-at-point
  "f" '(:ignore t :wk "FIND/FILE")
  "ff" #'consult-recent-file
  "fl" #'find-library
  "fL" #'find-library-name
  "fs" #'find-sibling-file
  "fS" #'find-sibling-file-search
  ;; "f" #'
  ;; "f" #'
  ;; "f" #'
  ;; "f" #'

  "fF" '(:ignore t :wk "FILL")
  "fFc" #'set-fill-column
  "fFp" #'set-fill-prefix

  "g" #'guix
  ;; "M-g" '(:ignore t :which-key "GUIX")
  ;; "M-g x" #'guix-extended-command
  ;; "M-g M-h" #'guix-hash
  ;; "M-g M-b" #'guix-switch-to-buffer
  ;; "M-g M-r" #'guix-switch-to-repl

  "G" '(:ignore t :which-key "DEBBUGS")
  "Gb" #'debbugs-gnu-bugs
  "Gg" #'debbugs-gnu-guix-search
  "Gs" #'debbugs-gnu-search
  "Gp" #'debbugs-gnu-package

  "l" #'pulsar-pulse-line
  "L" #'pulsar-highlight-dwim

  "T" #'tldr
  "<left>" #'winner-undo
  "<right>" #'winner-redo
  "X M-e" #'esup

  "C-e" (lambda () (interactive) (message "Instead use C-M-x to eval top form"))
  "M-e" #'eval-last-sexp)

;;**** kmacro and

(dc/unbind-keys '("C-x ("                            ;kmacro-start-macro
                  "C-x )"                            ;kmacro-end-macro
                  "C-x e"))                          ;kmacro-end-and-call-macro

(global-leader-def
  :keymaps '(global)
  :wk-full-keys nil

  "(" #'ignore
  ")" #'ignore

  ;; TODO maybe rebind eval here
  "e" '(:ignore t :wk "EVAL"))

;; but e is a very common character, so it's
;; valuable as a future mnemonic
;;
;; eval-last-sexp is covered by lispy

;;*** leader-key (C-c, f12)

;; this prefix should find itself associated with
;; project mgmt, minor mode features and inward-looking functions

;;*** UI

;;**** tab-bar

(general-define-key
 :keymaps 'global
 :wk-full-keys nil

 "C-<next>" #'tab-bar-switch-to-next-tab
 "C-<prior>" #'tab-bar-switch-to-prev-tab
 "C-<tab>" #'tab-bar-switch-to-tab
 "C-S-<tab>" #'tab-prev

 [remap tab-next] #'dc/tab-next
 [remap tab-previous] #'dc/tab-previous)

;;**** buffer-move

;; shouldn't be using arrows
(dc/unbind-keys
 ;; left/right-word
 '("C-S-<left>"
   "C-S-<right>"
   ;; globally, bound to the same function
   "C-S-<up>"
   "C-S-<down>"))

(general-define-key
 :keymaps 'global
 :wk-full-keys nil

 "<C-S-up>" #'buf-move-up
 "<C-S-down>" #'buf-move-down
 "<C-S-left>" #'buf-move-left
 "<C-S-right>" #'buf-move-right)

;;**** popper

;; M-` conflicts with geiser-capf
;; H-` and s-` conflict with KDE's switch-frame
(general-define-key
 :keymaps 'popper-mode-map
 "C-~" #'popper-cycle                 ; M-`
 "C-`" #'popper-toggle-latest           ; C-`
 "C-M-`" #'popper-toggle-type)          ; C-M-`

;;**** Ace Jump
;; TODO change popper from M-`

(leader-def
  :wk-full-keys nil

  "g" '(:ignore t :which-key "GIT")
  "gL"  'git-link

  "j"   '(:ignore t :which-key "JUMP")
  "jj"  '(avy-goto-char :which-key "jump to char")
  "jw"  '(avy-goto-word-0 :which-key "jump to word")
  "jl"  '(avy-goto-line :which-key "jump to line"))

;; redirect F2 -> C-c (doesn't show everything on which-keys)
;; (general-define-key
;;  :keymaps 'global
;;  :wk-full-keys nil
;;  "<f2>" '(:prefix-command global-leader-prefix-command))

;;** Interface

;;*** minibuffer-local-map

(general-define-key
 :keymaps 'minibuffer-local-map

 "C-r" #'consult-history
 "C-." #'embark-act
 "C-;" #'embark-dwim
 "C-l" #'dc/match-components-literally
 "C-y" #'yank
 "C-c C-;" #'embark-export
 "C-c C-l" #'embark-collect
 ;; "C-c C-e" #'+vertico/embark-export-write

 "M-s" #'consult-history ;; orig. next-matching-history-element
 "M-r" #'consult-history

 "M-h" #'vertico-directory-up

 ;; bound in multiform map
 ;;   M-V -> `vertico-multiform-vertical'
 ;;   M-G -> `vertico-multiform-grid'
 ;;   M-F -> `vertico-multiform-flat'
 ;;   M-R -> `vertico-multiform-reverse'
 ;;   M-U -> `vertico-multiform-unobtrusive'

 ;; can't quite figure this one out
 "C-x C-j" #'consult-dir-jump-file

 "M-A" #'marginalia-cycle)

;;*** Hilights (hi-lock)

;; these hi-lock commands are all emacs native and in search-map by default
;; M-s h .					highlight-symbol-at-point
;; M-s h f					hi-lock-find-patterns
;; M-s h l					highlight-lines-matching-regexp
;; M-s h p					highlight-phrase
;; M-s h r					highlight-regexp
;; M-s h u					unhighlight-regexp
;; M-s h w					hi-lock-write-interactive-patterns

(general-define-key
 :keymaps 'search-map

 "h M-h" #'highlight-symbol
 "h M-u" #'highlight-symbol-remove-all
 "h M-U" #'highlight-symbol-remove-all)

;;*** Consult

;;**** remaps (consult)
(general-define-key
 :keymaps 'global

 ;; [remap apropos-command] #'describe-symbol
 [remap bookmark-jump]                 #'consult-bookmark
 ;;   [remap evil-show-marks]               #'consult-mark
 ;;   [remap evil-show-jumps]               #'+vertico/jump-list
 ;;   [remap evil-show-registers]           #'consult-register
 [remap goto-line]                     #'consult-goto-line
 [remap imenu]                         #'consult-imenu
 [remap locate]                        #'consult-locate
 [remap load-theme]                    #'consult-theme
 [remap man]                           #'consult-man
 [remap Info-search]                   #'consult-info
 [remap recentf-open-files]            #'consult-recent-file
 [remap switch-to-buffer]              #'consult-buffer
 [remap switch-to-buffer-other-window] #'consult-buffer-other-window
 [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
 ;; [remap persp-switch-to-buffer] #'+vertico/switch-workspace-buffer

 [remap yas-insert-snippet] #'consult-yasnippet
 [remap yas-visit-snippet-file] #'consult-yasnippet-visit-snippet-file

 ;; doesn't work in minibuffer
 ;; [remap yank]                            #'consult-yank-replace
 "C-y"                                 #'consult-yank-replace
 [remap yank-pop]                      #'consult-yank-pop)

;;**** globals (consult)
(general-define-key
 :keymaps 'global

 "C-x d" #'consult-dir
 "C-x C-d" #'dired
 "C-s" #'consult-line
 "C-M-j" #'consult-buffer
 "C-M-l" #'consult-imenu
 ;; "C-M-." #'embark-act

 "C-c M-x" #'consult-mode-command
 "C-c h" #'consult-history

 ;; C-x bindings (ctl-x-map
 "C-x M-:" #'consult-complex-command ;; orig. repeat-complex-command

 "C-x b" #'bufler
 "C-c t b" #'bufler-sidebar
 "C-x M-b" #'consult-buffer ;; orig. switch-to-buffer
 "C-x B" #'ibuffer

 "C-x 4 b" #'consult-buffer-other-window ;; orig. switch-to-buffer-other-window
 "C-x 5 b" #'consult-buffer-other-frame  ;; orig. switch-to-buffer-other-frame
 "C-x r b" #'consult-bookmark            ;; orig. bookmark-jump
 "C-x p b" #'consult-project-buffer      ;; orig. project-switch-to-buffer

 ;; Custom M-# bindings for fast register access
 "M-#" #'consult-register-load
 "M-'" #'consult-register-store ;; orig. abbrev-prefix-mark (unrelated
 "C-M-#" #'consult-register

 ;; Other custom bindings
 "C-x M-y" #'consult-yank-from-kill-ring

 ;; nav
 ;; #'consult-org-heading
 ;; #'consult-recent-file

 ;; misc
 ;; #'consult-bibtex-embark-map
 ;; #'consult-isearch-forward

 ;; completion
 ;; #'consult-preview-at-point
 ;; #'consult-lsp-symbols
 ;; #'consult-lsp-diagnostics
 ;; #'consult-lsp-file-symbols
 ;; #'consult-lsp-diagnostics
 )

;;**** goto-map (consult)
;; M-g bindings

(dc/unbind-keys '("M-g c"))
(general-define-key
 :keymaps 'goto-map
 :wk-full-keys nil
 "C" #'goto-char
 "e" #'consult-compile-error
 "f" '(:ignore t :which-key "FLY")
 "fc" #'consult-flycheck
 "fm" #'consult-flymake

 "g" #'consult-goto-line   ;; orig. goto-line
 "M-g" #'consult-goto-line ;; orig. goto-line
 "o" #'consult-outline     ;; Alternative: consult-org-heading
 "m" #'consult-mark
 "k" #'consult-global-mark
 "i" #'consult-imenu-multi
 "I" #'consult-imenu-multi              ;; duplicate

 "a" #'consult-org-agenda

 "r" '(:ignore t :which-key "ROAM")
 "rr" #'consult-org-roam-search
 "rb" #'consult-org-roam-backlinks
 "rf" #'consult-org-roam-file-find
 "rl" #'consult-org-roam-forward-links
 )

;;**** search-map (consult)
;; M-s bindings

(general-define-key
 :keymaps 'search-map
 "d" #'consult-find
 "D" #'consult-locate
 ;; "M-d" #'consult-dir-jump-file
 "g" #'consult-grep
 "G" #'consult-git-log-grep
 "M-g" #'consult-git-grep
 "i" #'consult-info
 "k" #'consult-keep-lines
 "m" #'consult-man
 "r" #'consult-ripgrep
 "s" #'consult-line-multi               ; "L"
 "M-s" #'consult-yasnippet
 "M-S" #'consult-yasnippet-visit-snippet-file
 "u" #'consult-focus-lines

 ;; Isearch integration
 "e" #'consult-isearch-history)

(general-define-key
 :keymaps 'isearch-mode-map

 "M-e" #'consult-isearch-history   ;; orig. isearch-edit-string
 "M-s e" #'consult-isearch-history ;; orig. isearch-edit-string
 "M-s l" #'consult-line            ;; needed by consult-line to detect isearch
 "M-s L" #'consult-line-multi      ;; needed by consult-line to detect isearch
 )

;; (leader-def
;;   :keymaps 'global
;;   "a" #'embark-act)

;;*** vertico-map

;; "C-S-r"        #'vertico-repeat

(general-define-key
 :keymaps 'vertico-map
 "C-n" #'vertico-next
 "C-p" #'vertico-previous

 "C-M-n" #'vertico-next-group
 "C-M-p" #'vertico-previous-group

 "C-'" #'vertico-quick-jump
 "C-\"" #'vertico-quick-embark
 "C-c '" #'vertico-quick-insert
 "C-c \"" #'vertico-quick-exit

 "C-x C-d" #'consult-dir
 "C-x C-j" #'consult-dir-jump-file)

;;*** corfu-map

;; https://github.com/minad/corfu#key-bindings
;; corfu includes key remaps for common emacs functions
;; but lispy remaps things like C-a and C-e

(general-define-key
 :keymaps 'corfu-map
 "C-n" #'corfu-next
 "C-p" #'corfu-previous
 "TAB" #'corfu-insert
 [tab] #'corfu-insert

 "'" #'corfu-quick-complete

 ;; for orderless queries, maintain completion with spaces
 "C-f" #'corfu-insert-separator
 "M-SPC" #'corfu-insert-separator

 "M-m" #'corfu-move-to-minibuffer  ;; to access embark actions
 "M-g" #'corfu-info-location
 "M-h" #'corfu-info-documentation

 ;; should be in corfu@main
 ;; "C-a" #'corfu-prompt-beginning
 ;; "C-e" #'corfu-prompt-end

 ;; "M-d" #'corfu-popupinfo-toggle
 ;; "M-p" #'corfu-popinfo-scroll-up
 ;; "M-n" #'corfu-popinfo-scroll-down

 "C-M-n" #'vertico-next-group
 "C-M-p" #'vertico-previous-group

 "C-x C-d" #'consult-dir
 "C-x C-j" #'consult-dir-jump-file)

;;**** corfu-popupinfo-map

;; see corfu-popupinfo-map
;; (defvar-keymap corfu-popupinfo-map
;;   :doc "Additional keymap activated in popupinfo mode."
;;   "M-t" #'corfu-popupinfo-toggle
;;   "<remap> <corfu-info-documentation>" #'corfu-popupinfo-documentation
;;   "<remap> <corfu-info-location>" #'corfu-popupinfo-location
;;   "<remap> <scroll-other-window>" #'corfu-popupinfo-scroll-up
;;   "<remap> <scroll-other-window-down>" #'corfu-popupinfo-scroll-down
;;   "<remap> <end-of-buffer-other-window>" #'corfu-popupinfo-end
;;   "<remap> <beginning-of-buffer-other-window>" #'corfu-popupinfo-beginning)

;;**** cape

(leader-def
  :keymaps '(global)
  :wk-full-keys nil
  "p" '(:ignore t :which-key "CAPF")
  "p p"  #'completion-at-point
  "p t"  #'complete-tag
  "p d"  #'cape-dabbrev
  "p h"  #'cape-history
  "p f"  #'cape-file
  "p k"  #'cape-keyword
  "p s"  #'cape-elisp-symbol
  "p e"  #'cape-elisp-block
  "p a"  #'cape-abbrev
  "p l"  #'cape-line
  "p w"  #'cape-dict
  "p \\"  #'cape-tex
  "p _"  #'cape-tex
  "p ^"  #'cape-tex
  "p &"  #'cape-sgml
  "p r"  #'cape-rfc1345)

;;** UI

;;*** Bookmarks, Registers

(global-leader-def
  :keymaps '(global)
  :wk-full-keys nil
  "rB" '(:ignore t :which-key "BURLY")
  "rBo" #'burly-open-bookmark
  "rBO" #'burly-open-url
  "rBw" #'burly-bookmark-windows
  "rBf" #'burly-bookmark-frames
  "rBB" #'burly-kill-buffer-url
  "rBF" #'burly-kill-frames-url
  "rBW" #'burly-kill-windows-url)

;;*** Window Management

;;*** Jump

(leader-def
  :wk-full-keys nil
  "j"   '(:ignore t :which-key "jump")
  "jj"  '(avy-goto-char :which-key "jump to char")
  "jw"  '(avy-goto-word-0 :which-key "jump to word")
  "jl"  '(avy-goto-line :which-key "jump to line"))

;;*** Shell

;; TODO: map C-c M-! to :which-key and allow selecting shell scripts
;; that paste output to buffer
;;
;; edit so output is more friendly to insertion
;; guix search emacs-srv | recsel -p name,description

;;*** Lispy

;; TODO: determine why the lispy-x hydra req. manual eval to run
;;  shows in message buffer sometimes.

;; most of these are already bound under the lispy-x functionality
;; https://github.com/abo-abo/lispy#features

(general-define-key
 :keymaps '(lispy-mode-map)
 "M-<up>" #'lispy-outline-prev
 "M-<down>" #'lispy-outline-next

 "å" #'lispy-to-cond                    ;w
 "á" #'lispy-to-lambda                  ;a
 "ß" #'lispy-to-ifs                     ;d
 "ð" #'lispy-to-defun                   ;s

 ;; altgr+wads (on us intl altgr + deadkeys, maybe custom keyboard)
 "Å" #'lispy-move-up                    ;W
 "Á" #'lispy-move-left                  ;A
 "§" #'lispy-move-down                  ;D
 "Ð" #'lispy-move-right                 ;S

 ;; "Å" #'lispy-move-outline-up         ;W
 ;; "§" #'lispy-move-outline-up         ;S
 ;; altgr+] and altgr+}
 "»" #'lispy-stringify
 "”" #'lispy-unstringify

 ;; altgr+jk
 "œ" #'lispy-insert-outline-left
 "ï" #'lispy-insert-outline-below)

;;** Doom

;;*** Prefixes (C-c)

(leader-def
  :keymaps 'global
  :wk-full-keys nil
  "c" '(:ignore t :wk "CODE")
  "e" '(:ignore t :wk "EVAL")
  "f" '(:ignore t :wk "FILE")
  "i" '(:ignore t :wk "INSERT")
  "l" '(:ignore t :wk "LOCAL")
  "m" '(:ignore t :wk "M/CURSOR")
  "n" '(:ignore t :wk "ORG/NOTES")
  "o" '(:ignore t :wk "OPEN")
  "p" '(:ignore t :wk "PROJECT")
  "q" '(:ignore t :wk "QUIT")
  "r" '(:ignore t :wk "REMOTE")
  "s" '(:ignore t :wk "SEARCH")
  "t" '(:ignore t :wk "TOGGLE")
  "v" '(:ignore t :wk "VCS")
  "w" '(:ignore t :wk "WORKSPACE")

  "&" '(:ignore t :wk "SNIPPET")
  "7" '(:ignore t :wk "SNIPPET")
  "!" '(:ignore t :wk "FLYCHECK")
  "1" '(:ignore t :wk "FLYCHECK")

  ;; "C-f" '(:ignore t :wk "FOLD") ;; imenu > folding
  )

;;*** & 7 SNIPPETS

(defun dc/init-keybinds-yasnippet ()
  (dolist (pfx '("&" "7"))
    (leader-def
      :keymaps '(yas-minor-mode-map)
      :wk-full-keys nil
      ;; doesn't generate :wk
      ;; :prefix (concat "C-c " pfx)

      pfx '(:ignore t :wk "SNIPPETS")
      (concat pfx "n") #'yas-new-snippet
      (concat pfx "i") #'yas-insert-snippet
      (concat pfx "/") #'yas-visit-snippet-file
      (concat pfx "r") #'yas-reload-all
      ;; "c" #'aya-create
      ;; "e" #'aya-expand
      )))

;; this looped setup runs fine
(dc/init-keybinds-yasnippet)

;;*** ! 1 Flycheck
;; flycheck-mode-map requires having invoked the mode
(with-eval-after-load 'flycheck
  (general-translate-key
    nil '(flycheck-mode-map)
    "C-c 1" "C-c !"
    "<f12> 1" "C-c !"
    "<f12> 1" "C-c !"))

;;*** c CODE

(leader-def
  :keymaps 'global
  :wk-full-keys nil
  "cc" #'compile
  "cC" #'recompile
  ;; "d" #'+lookup/definition
  ;; "D" #'+lookup/references
  ;; "e" #'+eval/buffer-or-region
  ;; "E" #'+eval/region-and-replace
  ;; "f" #'+format/region-or-buffer
  ;; "i" #'+lookup/implementations
  ;; "k" #'+lookup/documentation
  ;; "s" #'+eval/send-region-to-repl
  ;; "t" #'+lookup/type-definition
  "cw" #'delete-trailing-whitespace
  ;; "W" #'doom/deletr-trailing-newlines
  ;; "x" #'+default/diagnostics
  )

;;**** LSP
;; TODO: LSP UI bindings?

;;        (:when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
;;         :desc "LSP Code actions"                      "a"   #'lsp-execute-code-action
;;         :desc "LSP Organize imports"                  "o"   #'lsp-organize-imports
;;         :desc "LSP Rename"                            "r"   #'lsp-rename
;;         :desc "LSP"                                   "l"   #'+default/lsp-command-map
;;         (:when (modulep! :completion ivy)
;;          :desc "Jump to symbol in current workspace" "j"   #'lsp-ivy-workspace-symbol
;;          :desc "Jump to symbol in any workspace"     "J"   #'lsp-ivy-global-workspace-symbol)
;;         (:when (modulep! :completion helm)
;;          :desc "Jump to symbol in current workspace" "j"   #'helm-lsp-workspace-symbol
;;          :desc "Jump to symbol in any workspace"     "J"   #'helm-lsp-global-workspace-symbol)
;;         (:when (modulep! :completion vertico)
;;          :desc "Jump to symbol in current workspace" "j"   #'consult-lsp-symbols
;;          :desc "Jump to symbol in any workspace"     "J"   (cmd!! #'consult-lsp-symbols 'all-workspaces))
;;         (:when (modulep! :ui treemacs +lsp)
;;          :desc "Errors list"                         "X"   #'lsp-treemacs-errors-list
;;          :desc "Incoming call hierarchy"             "y"   #'lsp-treemacs-call-hierarchy
;;          :desc "Outgoing call hierarchy"             "Y"   (cmd!! #'lsp-treemacs-call-hierarchy t)
;;          :desc "References tree"                     "R"   (cmd!! #'lsp-treemacs-references t)
;;          :desc "Symbols"                             "S"   #'lsp-treemacs-symbols))

;;**** EGLOT

(general-define-key
 :keymaps 'eglot-mode-map
 :prefix "M-g"
 :wk-full-keys nil
 "c" '(:ignore t :which-keys "CODE")
 "cs" #'consult-eglot-symbols
 "ca" #'eglot-code-actions
 ;; "cai" #'eglot-code-action-inline

 "cf" '(:ignore t :which-keys "FIND")
 "cfd" #'eglot-find-declaration
 "cfi" #'eglot-find-implementation
 "cft" #'eglot-find-typeDefinition

 ;; "cj" #'consult-eglot-symbols
 "cr" #'eglot-rename)


;; TODO: eglot requires too many keystrokes
;; (define-key eglot-mode-map (kbd "C-c C-a") #'eglot-code-actions)
;; (define-key eglot-mode-map (kbd "C-c C-r") #'eglot-rename)

;;*** e EVAL

;;*** f FILE

;; "c"   #'editorconfig-find-current-editorconfig)
;; "e"   #'doom/find-file-in-emacsd
;; "E"   #'doom/browse-in-emacsd
;; "F"   #'+default/find-file-under-here
;; "p"   #'doom/find-file-in-private-config
;; "P"   #'doom/open-private-config

;; "y"   #'+default/yank-buffer-path
;; "Y"   #'+default/yank-buffer-path-relative-to-project
;; "x"   #'doom/open-scratch-buffer
;; "X"   #'doom/switch-to-scratch-buffer)

;;*** i INSERT

(leader-def
  :keymaps 'global
  :wk-full-keys nil
  "ie" #'emojify-insert-emoji
  ;; "if" #'+default/insert-file-path
  ;; "iF" (cmd!! #'+default/insert-file-path t)
  "is" #'yas-insert-snippet
  ;; "iy" #'+default/yank-pop
  "iu" #'insert-char)

;;*** l LOCAL
;;*** m M/CURSOR

(leader-def
  :keymaps 'global
  "ml" #'mc/edit-lines
  "mn" #'mc/mark-next-like-this
  "mN" #'mc/unmark-next-like-this
  "mp" #'mc/mark-previous-like-this
  "mP" #'mc/unmark-previous-like-this
  "mt" #'mc/mark-all-like-this
  "mm" #'mc/mark-all-like-this-dwim
  "me" #'mc/edit-ends-of-lines
  "ma" #'mc/edit-beginnings-of-lines
  "ms" #'mc/mark-sgml-tag-pair
  "md" #'mc/mark-all-like-this-in-defun
  "m <mouse-1>" #'mc/add-cursor-on-click)

;;*** n ORG

;;  ... wellll that is unfortunate
;; (nthcdr 0 '(lambda (foo bar) '(1 2 3 )))
;; (nthcdr 1 #'(lambda (foo bar) '(1 2 3 )))
;; (nthcdr -2 #'(lambda (foo bar) '(1 2 3 )))
;; (eql t 1)
;; (declare-function org-clock-in-complex-plane)
;; (apply-partially #'org-clock-in (lsh 1 2)) ;req setf then push
;;
;; it makes a lot more sense after seeing it evaluated,
;; the names/terminology were confusing.
;; it does exactly what you're told that lisp does

(defun dc/org-clock-in-recent (&optional select start-time)
  "SELECT from a list of recent TODO's to clock the user in."
  (interactive "P")
  (org-clock-in (list (ash #x1 2)) start-time))

(defun dc/org-clock-in-and-mark-default (&optional select start-time)
  "SELECT from a list of recent TODO's to clock the user in."
  (interactive "P")
  (org-clock-in (list (ash 1 4)) start-time))

(defun dc/org-clock-in-continue-from-last-timestamp (&optional
                                                     select start-time)
  "SELECT from a list of recent TODO's to clock the user in."
  (interactive "P")
  (org-clock-in (list (ash 1 6)) start-time))

(global-leader-def
  :keymaps '(org-mode-map)
  :wk-full-keys nil

  "1" '(:ignore t :wk "AGENDA")
  "12" '(:ignore t :wk "ORG")

  "1s" #'org-schedule
  "1d" #'org-deadline
  "1c" #'org-ctrl-c-ctrl-c

  ;; wut control characters
  "1 <tab>" #'dc/org-clock-in-recent    ; 4 select from recent
  "12 <tab>" #'dc/org-clock-in-continue-from-last-timestamp  ; 64 continuously
  "123 <tab>" #'dc/org-clock-in-and-mark-default   ; 16 mark default

  ;; "12a" #'org-archive-subtree-default
  ;; "12b" #'org-toggle-checkbox
  "12c" #'org-columns
  "12e" #'org-clock-modify-effort-estimate
  "12f" #'org-emphasize
  "12j" #'org-clock-goto
  "12l" #'org-latex-preview
  "12n" #'org-next-link
  "12o" #'org-clock-out
  "12p" #'org-previous-link
  "12q" #'org-clock-cancel
  ;; "12r" #'org-toggle-radio-button
  ;; "12s" #'org-archive-subtree
  "12t" #'org-toggle-time-stamp-overlays
  ;; "12u" #'org-dblock-update
  "12v" #'org-toggle-inline-images
  ;; "12w" #'org-cut-special
  "12x" #'org-clock-in-last
  ;; "12y" #'org-paste-special
  "12z" #'org-resolve-clocks)

(global-leader-def
  :keymaps '(global org-mode-map)
  :wk-full-keys nil

  "1o" #'org-clock-out
  "1 M-c" #'org-clock-cancel            ; and remove start time
  ;; "1C" #'+org/toggle-last-clock
  "1g" #'org-clock-goto

  "1z" #'org-resolve-clocks
  "1q" #'org-clock-cancel "1j" #'org-clock-goto
  "1d" #'org-clock-display
  "1x" #'org-clock-in-last
  "1e" #'org-clock-modify-effort-estimate)

(leader-def
  :keymaps 'global
  :wk-full-keys nil
  "n" '(:ignore t :wk "ORG/NOTES")
  "na" #'org-agenda                     ; nan
  ;; "ns" #'org-schedule
  ;; "nd" #'org-deadline

  ;; "nC" #'+org/toggle-last-clock
  "n M-C" #'org-clock-cancel            ; and remove start time
  "ng" #'org-clock-goto
  "no" #'org-clock-goto

  ;; "n." #'+default/search-notes-for-symbol-at-point
  ;; "nb" #'citar-open-notes

  "nl" #'org-store-link
  "n M-l" #'org-insert-link
  "nm" #'org-tags-view

  ;; TODO: "oc"  '(org-capture t :which-key "capture"); universal arg?
  "nn" #'org-capture
  "nN" #'org-capture-goto-target
  ;; "n M-n"  #'org-toggle-narrow-to-subtree
  "nt" #'org-todo-list

  ;; "ns" #'+default/org-notes-search
  ;; "nS" #'+default/org-notes-headlines

  "nv" #'org-search-view
  ;; "ny" #'+org/export-to-clipboard
  ;; "nY" #'+org/export-to-clipboard-as-rich-text

  "nr" '(:ignore t :wk "ROAM")

  "nra" #'org-roam-node-random
  "nrf" #'org-roam-node-find
  "nrF" #'org-roam-ref-find
  "nrg" #'org-roam-graph
  "nri" #'org-roam-node-insert
  "nrL" #'org-roam-link-replace-all
  "nrn" #'org-roam-capture
  "nrr" #'org-roam-buffer-toggle
  "nrs" #'org-roam-db-sync
  "nrt" #'org-roam-tag-add
  "nrT" #'org-roam-tag-remove

  ;; in terms of paredit commands
  "nr M-}" #'org-roam-extract-subtree       ; barf
  "nr M-)" #'org-roam-refile                ; slurp
  "nr M-r" #'org-roam-promote-entire-buffer ; raise (or M-u, up)
  "nr M-?" #'org-roam-demote-entire-buffer  ; convolute (or M-d, down)

  "nrd" '(:ignore t :wk "DAILY")
  "nrd-" #'org-roam-dailies-find-directory
  "nrdb" #'org-roam-dailies-goto-previous-note
  "nrdd" #'org-roam-dailies-goto-date
  "nrdD" #'org-roam-dailies-capture-date
  "nrdf" #'org-roam-dailies-goto-next-note
  "nrdm" #'org-roam-dailies-goto-tomorrow
  "nrdM" #'org-roam-dailies-capture-tomorrow
  "nrdn" #'org-roam-dailies-capture-today
  "nrdt" #'org-roam-dailies-goto-today
  "nrdT" #'org-roam-dailies-capture-today
  "nrdy" #'org-roam-dailies-goto-yesterday
  "nrdY" #'org-roam-dailies-capture-yesterday)

;; (general-define-key
;;  :keymaps 'search-map
;;  "M-r" #'dw-counsel-rg-org-files)

(leader-def
  :keymaps 'org-mode-map
  :wk-full-keys nil
  "4" 'org-archive-subtree

  "n" '(:ignore t :wk "ORG/NOTES")
  "nr" '(:ignore t :wk "ROAM")
  "nrR" #'org-roam-buffer-display-dedicated
  "nr M-r" #'org-roam-link-replace-all

  "nx" '(org-export-dispatch t)
  ;; TODO: "nx" '(org-export-dispatch t) ; universal arg?
  )

(local-leader-def
  :keymaps 'org-mode-map
  "a" #'org-roam-alias-add
  "A" #'org-roam-alias-remove
  "t" #'org-roam-tag-add
  "T" #'org-roam-tag-remove
  "r" #'org-roam-ref-add
  "R" #'org-roam-ref-remove)

;;*** o OPEN

;; "o" nil ; we need to unbind it first as Org claims this prefix
;; "o" . "open")
;; "b"  #'browse-url-of-file
;; "d"  #'+debugger/start
;; "r"  #'+eval/open-repl-other-window
;; "R"  #'+eval/open-repl-same-window
;;        :desc "Dired"              "-"  #'dired-jump

;;        (:when (modulep! :term vterm)
;; "t" #'+vterm/toggle
;; "T" #'+vterm/here)
;;        (:when (modulep! :term eshell)
;; "e" #'+eshell/toggle
;; "E" #'+eshell/here)

;;        (:when (modulep! :tools docker)
;; "D" #'docker)

;;        (:when (modulep! :email mu4e)
;; "m" #'=mu4e)

;;        (:when (modulep! :email notmuch)
;; "m" #'=notmuch)

;;*** p PROJECTILE

;; TODO: rebind C-x C-f to #'project-find-file, C-u C-x C-f to #'find-file

(general-define-key
 :keymaps 'global

 "C-x p f" #'project-find-file)

(defun dc/instead-use-M-g ()
  (interactive)
  (dc/forcing-function "use M-s instead of C-x p f for #'consult-ripgrep"))

(general-define-key
 :keymaps 'project-prefix-map
 ;; "k" #'dw/close-project-tab
 "k" #'project-kill-buffers
 ;; "f" #'consult-ripgrep
 ;; "f" #'dc/instead-use-M-g
 "f" #'project-find-file)

;;**** CMake

;; applied to project-prefix-map

;; "t" 'project-cmake-test
;; "m" 'project-cmake-build
;; "C" 'project-cmake-configure
;; "s" 'project-cmake-shell
;; "SK" 'project-cmake-select-kit
;; "SE" 'project-cmake-edit-settings
;; "SS" 'project-cmake-save-settings
;; "SL" 'project-cmake-load-settings
;; "U" 'project-cmake-debug





;; "p" . "project")
;;        :desc "Search project for symbol"   "." #'+default/search-project-for-symbol-at-point
;; "F" #'doom/find-file-in-other-project
;; "s" #'+default/search-project
;; "t" #'magit-todos-list
;; "x" #'doom/open-project-scratch-buffer
;; "X" #'doom/switch-to-project-scratch-buffer
;;        (:when (and (modulep! :tools taskrunner)
;;                    (or (modulep! :completion ivy)
;;                        (modulep! :completion helm)))
;; "z" #'+taskrunner/project-tasks)
;;        ;; later expanded by projectile
;; "4" . "in other window"))
;; "5" . "in other frame")))

;;*** q QUIT

;;*** r REF

(leader-def
  :keymaps '(org-mode-map latex-mode-map)
  :wk-full-keys nil
  "r" '(:ignore t :which-key "REF")
  ;; "r]" #'org-ref-insert-cite-link
  ;; "r]" #'org-ref-insert-cite-link
  "r]" #'org-ref-insert-cite-link

  ;; TODO: google scholar? (mostly in hydra)

  ;; org-link: adds arxiv
  "ra" '(:ignore t :which-key "arXiv")
  "rab" #'arxiv-get-pdf-add-bibtex-entry
  "raB" #'arxiv-add-bibtex-entry
  "rap" #'arxiv-get-pdf

  "rb" '(:ignore t :which-key "bibtex")
  "r M-b" #'org-ref-build-full-bibliography

  "rd" '(:ignore t :which-key "doi")

  "rh" '(:ignore t :which-key "hydras")
  "rhb" #'org-ref-bibtex-hydra/body
  "rhc" #'org-ref-citation-hydra/body
  "rhd" #'doi-link-follow/body
  "rhi" #'org-ref-insert-link-hydra/body
  "rhS" #'scopus-hydra/body

  "ri" '(:ignore t :which-key "isbn")
  "rib" #'isbn-to-bibtex
  "ril" #'isbn-to-bibtex-lead
  "ric" #'org-ref-isbn-clean-bibtex-entry
  "rio" #'isbn-to-bibtex-open-library

  ;; org-link: adds pmid, pmcid, nihmsid, pubmed-search, pubmed-clinical
  "rp" '(:ignore t :which-key "pubmed")
  "rpp" #'pubmed
  "rpC" #'pubmed-clinical-search
  "rpC" #'pubmed-clinical
  "rps" #'pubmed-simple-search
  "rpS" #'pubmed-advanced

  ;; org-link: adds eid, scopus-search, scopus-advanced-search, scopusid
  "rS" '(:ignore t :which-key "scopus")
  "rSs" #'scopus-basic-search
  "rSS" #'scopus-advanced-search
  "rSe" #'scopus-open-eid
  "rSa" #'scopus-related-by-author-url
  "rSk" #'scopus-related-by-keyword-url
  "rSr" #'scopus-related-by-references-url)

;; Doom: REMOTE

;;       (:when (modulep! :tools upload)
;;        (:prefix-map ("r" . "remote")
;;         :desc "Browse remote"              "b" #'ssh-deploy-browse-remote-base-handler
;;         :desc "Browse relative"            "B" #'ssh-deploy-browse-remote-handler
;;         :desc "Download remote"            "d" #'ssh-deploy-download-handler
;;         :desc "Delete local & remote"      "D" #'ssh-deploy-delete-handler
;;         :desc "Eshell base terminal"       "e" #'ssh-deploy-remote-terminal-eshell-base-handler
;;         :desc "Eshell relative terminal"   "E" #'ssh-deploy-remote-terminal-eshell-handler
;;         :desc "Move/rename local & remote" "m" #'ssh-deploy-rename-handler
;;         :desc "Open this file on remote"   "o" #'ssh-deploy-open-remote-file-handler
;;         :desc "Run deploy script"          "s" #'ssh-deploy-run-deploy-script-handler
;;         :desc "Upload local"               "u" #'ssh-deploy-upload-handler
;;         :desc "Upload local (force)"       "U" #'ssh-deploy-upload-handler-forced
;;         :desc "Diff local & remote"        "x" #'ssh-deploy-diff-handler
;;         :desc "Browse remote files"        "." #'ssh-deploy-browse-remote-handler
;;         :desc "Detect remote changes"      ">" #'ssh-deploy-remote-changes-handler))

;;*** s SEARCH

;; "s" . "search")
;;        :desc "Search project for symbol"    "." #'+default/search-project-for-symbol-at-point
;; "b"
;;        (cond ((modulep! :completion vertico)   #'consult-line)
;;              ((modulep! :completion ivy)       #'swiper)
;;              ((modulep! :completion helm)      #'swiper))
;; "B"
;;        (cond ((modulep! :completion vertico)   (cmd!! #'consult-line-multi 'all-buffers))
;;              ((modulep! :completion ivy)       #'swiper-all)
;;              ((modulep! :completion helm)      #'swiper-all))

;; "d" #'+default/search-cwd
;; "D" #'+default/search-other-cwd

;;        :desc "Search .emacs.d"              "e" #'+default/search-emacsd
;;        :desc "Locate file"                  "f" #'+lookup/file
;;        :desc "Jump to visible link"         "l" #'link-hint-open-link
;;        :desc "Jump to link"                 "L" #'ffap-menu
;;        :desc "Look up online"               "o" #'+lookup/online
;;        :desc "Look up online (w/ prompt)"   "O" #'+lookup/online-select
;;        :desc "Look up in local docsets"     "k" #'+lookup/in-docsets
;;        :desc "Look up in all docsets"       "K" #'+lookup/in-all-docsets
;;        :desc "Search project"               "p" #'+default/search-project
;;        :desc "Search other project"         "P" #'+default/search-other-project
;;        :desc "Search buffer"                "s" #'+default/search-buffer
;;        :desc "Search buffer for thing at point" "S"
;;        (cond ((modulep! :completion vertico)   #'+vertico/search-symbol-at-point)
;;              ((modulep! :completion ivy)       #'swiper-isearch-thing-at-point)
;;              ((modulep! :completion helm)      #'swiper-isearch-thing-at-point))
;;        :desc "Dictionary"                   "t" #'+lookup/dictionary-definition
;;        :desc "Thesaurus"                    "T" #'+lookup/synonyms)

;;*** t TOGGLE

;;**** Toggle Variables
;; call without keybind for now
(dc/toggleable-boolean native-comp-async-report-warnings-errors)
(dc/toggleable-boolean custom-buffer-verbose-help)
(dc/toggleable-boolean completion-ignore-case)
(dc/toggleable-boolean read-buffer-completion-ignore-case)
(dc/toggleable-boolean read-file-name-completion-ignore-case)

;; -: centered cursor
;; _: centered cursor
;; b: big-mode
;; c: fill-column indicator
;; D: desc
;; f: flycheck

(leader-def
  :keymaps 'global
  :wk-full-keys nil
  "t1" #'flycheck-mode
  "t!" #'flymake-mode
  "tc" '(:ignore t :wk "COMPLETION")
  "tcc" #'dc/toggle-completion-ignore-case
  "tcb" #'dc/toggle-read-buffer-completion-ignore-case
  "tcf" #'dc/toggle-read-file-name-completion-ignore-case
  "tC" #'global-display-fill-column-indicator-mode
  "t M-C" #'corfu-mode
  "t M-c" #'corfu-popupinfo-mode
  "t D" #'toggle-debug-on-error
  "tF" #'format-other-mode
  "t M-f" #'toggle-frame-fullscreen
  "tG" #'git-timemachine-toggle
  "t M-g" #'gud-tooltip-mode
  ;; "ti" #'highlight-indent-guides-mode
  ;; "tI" #'doom/toggle-indent-style"
  "tl" #'display-line-numbers-mode
  ;; "tp" #'org-tree-slide-mode
  "ts" #'flyspell-mode
  "tß" #'superword-mode
  "t§" #'subword-mode
  "tv" #'visual-line-mode
  "tV" #'visual-fill-column-mode
  "t C-v" #'vertico-multiform-mode
  "t M-v" #'visible-mode
  ;; "tw" #'visual-line-mode
  ;; "tw" #'+word-wrap-mode
  "tN" #'dc/toggle-native-comp-async-report-warnings-errors)

;; TODO map this to a list of *-ts-modes
(leader-def
  :keymaps '(prog-mode)
  "tt" #'treesit-explore-mode)

;; the toggle-map overwrites the keybind
(leader-def
  :keymaps 'org-mode-map
  "t ©" #'cdlatex-mode
  "tt" #'org-todo)

;;**** dired toggles
(leader-def
  :keymaps '(dired-mode-map)
  :wk-full-keys nil
  "td" '(:ignore t :wk "DIRED")
  "tda" #'dired-async-mode
  "tdA" #'all-the-icons-dired-mode
  "tdc" #'dired-collapse-mode
  "tdf" #'dired-filter-mode
  "tdg" #'turn-on-gnus-dired-mode
  "tdh" #'dired-hide-details-mode
  "tdi" #'dired-utils-format-information-line-mode
  "tdo" #'dired-omit-mode
  "tdv" #'dired-virtual-mode
  "tdc" #'dired-collapse-mode)

;;**** org toggles
(leader-def
  :keymaps '(org-mode-map)
  "tf" #'org-table-toggle-formula-debugger
  "to" #'org-table-toggle-coordinate-overlays)

;;**** markdown toggles
(leader-def
  :keymaps '(markdown-mode-map)
  "te" #'markdown-toggle-math
  "tf" #'markdown-toggle-fontify-code-blocks-natively
  "ti" #'markdown-toggle-inline-images
  "tl" #'markdown-toggle-url-hiding
  "tm" #'markdown-toggle-markup-hiding
  "tw" #'markdown-toggle-wiki-links
  "tx" #'markdown-toggle-gfm-checkbox
  )

;;*** v VCS

(leader-def
  :keymaps '(global)
  "vg" #'magit-status)

;; TODO: forge keybinds/functionality

;; ... forge shows pullreqs/issues in the magit-status buffer?

;; ... github.com/from/timbuktu

;; forge-create/edit-mark, forge-toggle-mark (non-interactive)
;; forge-edit-topic-marks
;; forge-list-assigned-issues
;; forge-list-authored-issues
;; forge-list-owned-issues
;; -- forge-create-pullreq-from-issue

;; forge-list-repositories
;; forge-list-owned-repositories
;; forge-add-organization-repositories (req. sync)

;; dir-locals forge-toggle-display-in-status-buffer

(general-define-key
 :keymaps '(forge-topic-mode-map)
 :wk-full-keys nil
 "c" #'forge-create-post
 "e" '(:ignore t :which-key "EDIT")
 "ea" #'forge-edit-topic-assignees
 "ed" #'forge-edit-topic-draft
 "ek" #'forge-delete-comment
 "el" #'forge-edit-topic-labels
 "em" #'forge-edit-topic-marks
 ;; "eM" #'forge-merge
 "en" #'forge-edit-topic-note
 "ep" #'forge-edit-post
 "er" #'forge-edit-topic-review-requests
 "es" #'forge-edit-topic-state
 "et" #'forge-edit-topic-title)

;; (phundrak/major-leader-key
;;  :keymaps 'forge-topic-mode-map
;;  "c"  #'forge-create-post
;;  "e"  '(:ignore t :which-key "edit"))

;; (:prefix-map ("v" . "versioning")
;;        :desc "Git revert file"             "R"   #'vc-revert
;;        :desc "Kill link to remote"         "y"   #'+vc/browse-at-remote-kill
;;        :desc "Kill link to homepage"       "Y"   #'+vc/browse-at-remote-kill-homepage
;;        (:when (modulep! :ui vc-gutter)
;;         :desc "Git revert hunk"            "r"   #'+vc-gutter/revert-hunk
;;         :desc "Git stage hunk"             "s"   #'+vc-gutter/stage-hunk
;;         :desc "Git time machine"           "t"   #'git-timemachine-toggle
;;         :desc "Jump to next hunk"          "n"   #'+vc-gutter/next-hunk
;;         :desc "Jump to previous hunk"      "p"   #'+vc-gutter/previous-hunk)
;;        (:when (modulep! :tools magit)
;;         :desc "Magit dispatch"             "/"   #'magit-dispatch
;;         :desc "Magit file dispatch"        "."   #'magit-file-dispatch
;;         :desc "Forge dispatch"             "'"   #'forge-dispatch
;;         :desc "Magit status"               "g"   #'magit-status
;;         :desc "Magit status here"          "G"   #'magit-status-here
;;         :desc "Magit file delete"          "x"   #'magit-file-delete
;;         :desc "Magit blame"                "B"   #'magit-blame-addition
;;         :desc "Magit clone"                "C"   #'magit-clone
;;         :desc "Magit fetch"                "F"   #'magit-fetch
;;         :desc "Magit buffer log"           "L"   #'magit-log-buffer-file
;;         :desc "Git stage file"             "S"   #'magit-stage-file
;;         :desc "Git unstage file"           "U"   #'magit-unstage-file
;;         (:prefix ("f" . "find")
;;          :desc "Find file"                 "f"   #'magit-find-file
;;          :desc "Find gitconfig file"       "g"   #'magit-find-git-config-file
;;          :desc "Find commit"               "c"   #'magit-show-commit
;;          :desc "Find issue"                "i"   #'forge-visit-issue
;;          :desc "Find pull request"         "p"   #'forge-visit-pullreq)
;;         (:prefix ("o" . "open in browser")
;;          :desc "Browse file or region"     "."   #'+vc/browse-at-remote
;;          :desc "Browse homepage"           "h"   #'+vc/browse-at-remote-homepage
;;          :desc "Browse remote"             "r"   #'forge-browse-remote
;;          :desc "Browse commit"             "c"   #'forge-browse-commit
;;          :desc "Browse an issue"           "i"   #'forge-browse-issue
;;          :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
;;          :desc "Browse issues"             "I"   #'forge-browse-issues
;;          :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs)
;;         (:prefix ("l" . "list")
;;          (:when (modulep! :tools gist)
;;           :desc "List gists"               "g"   #'gist-list)
;;          :desc "List repositories"         "r"   #'magit-list-repositories
;;          :desc "List submodules"           "s"   #'magit-list-submodules
;;          :desc "List issues"               "i"   #'forge-list-issues
;;          :desc "List pull requests"        "p"   #'forge-list-pullreqs
;;          :desc "List notifications"        "n"   #'forge-list-notifications)
;;         (:prefix ("c" . "create")
;;          :desc "Initialize repo"           "r"   #'magit-init
;;          :desc "Clone repo"                "R"   #'magit-clone
;;          :desc "Commit"                    "c"   #'magit-commit-create
;;          :desc "Fixup"                     "f"   #'magit-commit-fixup
;;          :desc "Issue"                     "i"   #'forge-create-issue
;;          :desc "Pull request"              "p"   #'forge-create-pullreq)))

;;*** w WORKSPACE

;; TODO: ensure that similar functionality is accounted for
;; - with tabspaces/project/projectile

;;         :desc "Display workspaces"           "d" #'+workspace/display
;;         :desc "Rename workspace"             "r" #'+workspace/rename
;;         :desc "Create workspace"             "c" #'+workspace/new
;;         :desc "Create named workspace"       "C" #'+workspace/new-named
;;         :desc "Delete workspace"             "k" #'+workspace/delete
;;         :desc "Save workspace"               "S" #'+workspace/save
;;         :desc "Switch to other workspace"    "o" #'+workspace/other
;;         :desc "Switch to left workspace"     "p" #'+workspace/switch-left
;;         :desc "Switch to right workspace"    "n" #'+workspace/switch-right

;;        :desc "Autosave session"             "a" #'doom/quicksave-session
;;        :desc "Save session"                 "s" #'doom/save-session
;;        :desc "Load session"                 "l" #'doom/load-session
;;        :desc "Load last autosaved session"  "L" #'doom/quickload-session
;;        :desc "Undo window config"           "u" #'winner-undo
;;        :desc "Redo window config"           "U" #'winner-redo

;;** Lang

;; The f5-f8 keys should be reserved for translation to prefixes on a
;; per-major-mode basis. Hopefully, I can assume that no other key bindings will
;; interfere

;;*** comint-mode
;; repls go here

;;**** geiser-repl-mode

;;*** lisp-mode

(local-leader-def
  :keymaps '(emacs-lisp-mode-map)

  ;; to debug with lispy, use xe (like C-x C-e) which i think works now
  ;; that i have a new versiion of lispy
  "i" #'edebug-instrument-callee
  "I" #'edebug-remove-instrumentation)

;;**** clojure-mode

;;**** emacs-lisp-mode

;;**** scheme-mode

;;**** geiser-mode

;;*** prog-mode

;;**** js2-mode

;;**** :js2-refactor
;; '(:prefix ("r" . "refactor")
;;           (:prefix ("a" . "add/arguments"))
;;           (:prefix ("b" . "barf"))
;;           (:prefix ("c" . "contract"))
;;           (:prefix ("d" . "debug"))
;;           (:prefix ("e" . "expand/extract"))
;;           (:prefix ("i" . "inject/inline/introduce"))
;;           (:prefix ("l" . "localize/log"))
;;           (:prefix ("o" . "organize"))
;;           (:prefix ("r" . "rename"))
;;           (:prefix ("s" . "slurp/split/string"))
;;           (:prefix ("t" . "toggle"))
;;           (:prefix ("u" . "unwrap"))
;;           (:prefix ("v" . "var"))
;;           (:prefix ("w" . "wrap"))
;;           (:prefix ("3" . "ternary")))

;;**** terraform-mode



;;*** text-mode

;;**** org-mode
(general-define-key
 :keymaps '(org-mode-map org-sidebar-map org-sidebar-tree-map)
 :wk-full-keys nil
 :prefix "<f7>"
 "SPC" #'org-sidebar-tree-toggle
 "C-SPC" #'org-sidebar-toggle)

;;**** sgml-mode

;;**** html-mode

;;** Mode-Specific

;; ahhhh ok, so the x509 "e" key shows you the params for the command
(local-leader-def
  :keymaps '(x509-mode-map)
  "d" #'x509-dwim)

;;* Keys Post

;;** Run Looped Keybinds
;; TODO there must be something affecting how help bindings are setup
;; this must run after emacs sets up
;; the f1 bindings aren't translated from C-h. must be happening in emacs
(add-hook 'emacs-startup-hook #'dc/init-keybinds-quick)
(add-hook 'emacs-startup-hook #'dc/init-keybinds-help)

;;** Map leader-keys to all maps

(general-translate-key
  nil '(global)
  "<f2>" "C-x"
  "<f12>" "C-c")
;; (dc/translated-keymaps-set-keys
;;  'global
;;  "<f2>" "C-x"
;;  "<f12>" "C-c")

(defvar dc/translated-keypairs
  '("<f2>" "C-x"
    "<f12>" "C-c"))

(defun dc/keymaps-list-collect ()
  (cl-loop for s being the symbols if
           (and (boundp s)
                (keymapp (symbol-value s)))
           collect s))

(defun dc/keymaps-list-collect-atoms ()
  (let (r)
    (mapatoms
     (lambda (s) (if (and (boundp s)
                          (keymapp (symbol-value s)))
                     (push s r)))) r))

;; causes errors like "void variable menu-function-18" if function-based keymaps
;; are mixed-in. see easy-menu.el:
;; https://web.mit.edu/Emacs/source/emacs/lisp/emacs-lisp/easymenu.el
(defvar dc/keymaps-list
  (dc/keymaps-list-collect))

(defun dc/translated-keymaps-set-keys (kms pairs)
  "Set PAIRS to be translated on KMS keymaps."
  (apply #'general-translate-key nil kms pairs))

(defun dc/ensure-translated-keymap-variables-set ()
  "Using General.el, ensure desired pairs of key combinations are
translated on keymaps."
  (dc/translated-keymaps-set-keys
   (seq-filter (lambda (km) (not (fboundp km)))
               dc/keymaps-list)
   dc/translated-keypairs))

;; an approach using (define-key ...) seems to require reading the prefix from
;; each keymap, even if using (defvar-keymap)
;; (defvar dc/translated-keypairs '(("<f2>" "C-x") ("<f12>" "C-c")))

;; (mapc (lambda (km) (fboundp km)) (take (length dc/keymaps-list) dc/keymaps-list))
;; (length (seq-filter (lambda (km) (fboundp km)) (take (length dc/keymaps-list) dc/keymaps-list)))
;; (length (seq-filter (lambda (km) (boundp km)) (take (length dc/keymaps-list) dc/keymaps-list)))
;; (length (seq-filter (lambda (km) (not (fboundp km))) (take (length dc/keymaps-list) dc/keymaps-list)))

(dc/ensure-translated-keymap-variables-set)

(provide 'dc-keys)

;;*** Embark (Doom)
;; (  (embark-define-keymap +vertico/embark-doom-package-map
;;     "Keymap for Embark package actions for packages installed by Doom."
;;     ("h" doom/help-packages)
;;     ("b" doom/bump-package)
;;     ("c" doom/help-package-config)
;;     ("u" doom/help-package-homepage))
;;   (setf (alist-get 'package embark-keymap-alist) #'+vertico/embark-doom-package-map)
;;   (map! (:map embark-file-map
;;          :desc "Open target with sudo"        "s"   #'doom/sudo-find-file
;;          (:when (modulep! :tools magit)
;;           :desc "Open magit-status of target" "g"   #'+vertico/embark-magit-status)
;;          (:when (modulep! :ui workspaces)
;;                 :desc "Open in new workspace"       "TAB" #'+vertico/embark-open-in-new-workspace))))

;; *** General.el Examples

;; :major-modes
;; (general-define-key
;;  :keymaps 'emacs-lisp-mode-map
;;  :major-modes t
;;  ...)

;; (general-define-key
;;  :keymaps '(no-follow-convention-mode-keymap1
;;             org-mode-map)
;;  :major-modes '(no-follow-convention-mode t)
;;  ...)

;; (map! "C-:" #'company-box-doc-manually
;;       "C-<tab>" #'company-yasnippet
;;       "C-M-;" #'company-yasnippet)

;;*** ./.emacs.doom/modules/config/default/+emacs-bindings.el

;; (map! :leader
;;       :desc "Evaluate line/region"        "e"   #'+eval/line-or-region

;;       (:prefix ("l" . "<localleader>")) ; bound locally

;;       ;;; <leader> q --- quit/restart
;;       (:prefix-map ("q" . "quit/restart")
;;        :desc "Restart emacs server"         "d" #'+default/restart-server
;;        :desc "Delete frame"                 "f" #'delete-frame
;;        :desc "Clear current frame"          "F" #'doom/kill-all-buffers
;;        :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
;;        :desc "Quit Emacs"                   "q" #'kill-emacs
;;        :desc "Save and quit Emacs"          "Q" #'save-buffers-kill-terminal
;;        :desc "Quick save current session"   "s" #'doom/quicksave-session
;;        :desc "Restore last session"         "l" #'doom/quickload-session
;;        :desc "Save session to file"         "S" #'doom/save-session
;;        :desc "Restore session from file"    "L" #'doom/load-session
;;        :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
;;        :desc "Restart Emacs"                "R" #'doom/restart)

;;       ;;; <leader> & --- snippets

;;       ;; APPs
;;       ;;; <leader> M --- mu4e
;;       (:when (modulep! :email mu4e)
;;        (:prefix-map ("M" . "mu4e")
;;         :desc "Open email app" "M" #'=mu4e
;;         :desc "Compose email"  "c" #'+mu4e/compose))

;;       ;;; <leader> I --- IRC
;;       (:when (modulep! :app irc)
;;        (:prefix-map ("I" . "irc")
;;         :desc "Open irc app"       "I" #'=irc
;;         :desc "Next unread buffer" "a" #'tracking-next-buffer
;;         :desc "Quit irc"           "q" #'+irc/quit
;;         :desc "Reconnect all"      "r" #'circe-reconnect-all
;;         :desc "Send message"       "s" #'+irc/send-message
;;         (:when (modulep! :completion ivy)
;;          :desc "Jump to channel"  "j" #'+irc/ivy-jump-to-channel)
;;         (:when (modulep! :completion vertico)
;;          :desc "Jump to channel"  "j" #'+irc/vertico-jump-to-channel)))

;;; Global & plugin keybinds

;; (map! "C-'" #'imenu

;;       ;;; search
;;       (:when (modulep! :completion ivy)
;;         "C-S-s"        #'swiper
;;         "C-S-r"        #'ivy-resume)
;;       (:when (modulep! :completion helm)
;;         "C-S-s"        #'swiper-helm
;;         "C-S-r"        #'helm-resume)

;;       ;;; buffer management
;;       "C-x b"       #'switch-to-buffer
;;       "C-x 4 b"     #'switch-to-buffer-other-window
;;       (:when (modulep! :ui workspaces)
;;         "C-x B"       #'switch-to-buffer
;;         "C-x 4 B"     #'switch-to-buffer-other-window
;;         (:when (modulep! :completion ivy)
;;           "C-x 4 b"   #'+ivy/switch-workspace-buffer-other-window))
;;       "C-x C-b"     #'ibuffer
;;       "C-x K"       #'doom/kill-this-buffer-in-all-windows

;;       ;;; company-mode
;;       "C-;" #'+company/complete
;;       (:after company
;;         :map company-active-map
;;         "C-o"        #'company-search-kill-others
;;         "C-n"        #'company-select-next
;;         "C-p"        #'company-select-previous
;;         "C-h"        #'company-quickhelp-manual-begin
;;         "C-S-h"      #'company-show-doc-buffer
;;         "C-s"        #'company-search-candidates
;;         "M-s"        #'company-filter-candidates
;;         [C-tab]      #'company-complete-common-or-cycle
;;         [tab]        #'company-complete-common-or-cycle
;;         [backtab]    #'company-select-previous
;;         "C-RET"      (cond ((modulep! :completion vertico)  #'completion-at-point)
;;                            ((modulep! :completion ivy)      #'counsel-company)
;;                            ((modulep! :completion helm)     #'helm-company))
;;         "C-<return>" (cond ((modulep! :completion vertico)  #'completion-at-point)
;;                            ((modulep! :completion ivy)      #'counsel-company)
;;                            ((modulep! :completion helm)     #'helm-company))
;;         :map company-search-map
;;         "C-n"        #'company-search-repeat-forward
;;         "C-p"        #'company-search-repeat-backward
;;         "C-s"        (cmd! (company-search-abort) (company-filter-candidates)))

;;       ;;; expand-region
;;       "C-="  #'er/expand-region

;;       ;;; flycheck
;;       (:after flycheck
;;         :map flycheck-error-list-mode-map
;;         "C-n" #'flycheck-error-list-next-error
;;         "C-p" #'flycheck-error-list-previous-error
;;         "RET" #'flycheck-error-list-goto-error)

;;       ;;; help and info
;;       (:after help-mode
;;         :map help-mode-map
;;         "o" #'link-hint-open-link
;;         ">" #'help-go-forward
;;         "<" #'help-go-back
;;         "n" #'forward-button
;;         "p" #'backward-button)
;;       (:after helpful
;;         :map helpful-mode-map
;;         "o" #'link-hint-open-link)
;;       (:after apropos
;;         :map apropos-mode-map
;;         "o" #'link-hint-open-link
;;         "n" #'forward-button
;;         "p" #'backward-button)
;;       (:after info
;;         :map Info-mode-map
;;         "o" #'link-hint-open-link)

;;       ;;; ivy & counsel
;;       (:when (modulep! :completion ivy)
;;         (:after ivy
;;           :map ivy-minibuffer-map
;;           "TAB"   #'ivy-alt-done
;;           "C-g"   #'keyboard-escape-quit)
;;         (:after counsel
;;           :map counsel-ag-map
;;           "C-SPC" #'ivy-call-and-recenter ; preview
;;           "M-RET" #'+ivy/git-grep-other-window-action)
;;         "C-M-y"   #'counsel-yank-pop)

;;       ;;; neotree
;;       (:when (modulep! :ui neotree)
;;         "<f9>"    #'+neotree/open
;;         "<C-f9>"  #'+neotree/find-this-file
;;         (:after neotree
;;           :map neotree-mode-map
;;           "q"     #'neotree-hide
;;           "RET"   #'neotree-enter
;;           "SPC"   #'neotree-quick-look
;;           "v"     #'neotree-enter-vertical-split
;;           "s"     #'neotree-enter-horizontal-split
;;           "c"     #'neotree-create-node
;;           "D"     #'neotree-delete-node
;;           "g"     #'neotree-refresh
;;           "r"     #'neotree-rename-node
;;           "R"     #'neotree-refresh
;;           "h"     #'+neotree/collapse-or-up
;;           "l"     #'+neotree/expand-or-open
;;           "n"     #'neotree-next-line
;;           "p"     #'neotree-previous-line
;;           "N"     #'neotree-select-next-sibling-node
;;           "P"     #'neotree-select-previous-sibling-node))

;;       ;;; popups
;;       (:when (modulep! :ui popup)
;;         "C-x p"   #'+popup/other
;;         "C-`"     #'+popup/toggle
;;         "C-~"     #'+popup/raise)

;;       ;;; smartparens
;;       (:after smartparens
;;         :map smartparens-mode-map
;;         "C-M-a"           #'sp-beginning-of-sexp
;;         "C-M-e"           #'sp-end-of-sexp
;;         "C-M-f"           #'sp-forward-sexp
;;         "C-M-b"           #'sp-backward-sexp
;;         "C-M-n"           #'sp-next-sexp
;;         "C-M-p"           #'sp-previous-sexp
;;         "C-M-u"           #'sp-up-sexp
;;         "C-M-d"           #'sp-down-sexp
;;         "C-M-k"           #'sp-kill-sexp
;;         "C-M-t"           #'sp-transpose-sexp
;;         "C-M-<backspace>" #'sp-splice-sexp)

;;       ;;; treemacs
;;       (:when (modulep! :ui treemacs)
;;         "<f9>"   #'+treemacs/toggle
;;         "<C-f9>" #'treemacs-find-file))

;; (map! :leader
;;       (:when (modulep! :editor fold)
;;        (:prefix ("C-f" . "fold")
;;         "C-d"     #'vimish-fold-delete
;;         "C-a C-d" #'vimish-fold-delete-all
;;         "C-f"     #'+fold/toggle
;;         "C-a C-f" #'+fold/close-all
;;         "C-u"     #'+fold/open
;;         "C-a C-u" #'+fold/open-all)))
