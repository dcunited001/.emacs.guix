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

(use-package kmacro :straight (:type built-in)
  :delight " K♫ ")

;;** Unbind Keys
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

    ;; should [remap] dired. not sure how to remap to a which-key prefix
    "C-x d"                             ;make dired a map
    "C-x f"                             ;set-fill-column
    "C-x x f"                           ;font-lock-update => C-x x M-f
    ))

;;*** trying to pack a lambda into a symbol
;; (fset 'dc/unbind-key (macroexpand-all '(unbind-key k)))
;; (seq-do (symbol-function 'dc/unbind-key) dc/keys-unbound-at-init)

(defun dc/unbind-keys (key-names &optional keymap)
  (seq-do (lambda (key)
            (if keymap
                (unbind-key key keymap)
              (unbind-key key)))
          key-names))

(dc/unbind-keys dc/keymaps-unbound-at-init)

;;** Which Keys

;;*** Special Keys

;; NOTE: which-keys-special-keys will assemble a regexp like the following
;;       which, in the which-keys--propertize-key, matches on the "key"

;; "\\(!\\|@\\|&\\|_\\|1\\|2\\|3\\|4\\|5\\|6\\|7\\|8\\| ...

;; "M" "C-M" "C-.+" "H"  ;only highlights commands starting with modkey

;; regexps like [:punct:], \\[:punct:\\], etc don't seem to work (req. pcre afaik)

;; NOTE: which-key-local-map-descriptions-face highlights what's listed
;;       on the (current-local-map) keymap

;; NOTE: reopen which-key

(use-package which-key :straight t
  ;; TODO: check for loading
  ;; :demand t
  :hook
  (ef-themes-post-load-hook
   . (lambda ()
       (dc/update-face 'which-key-separator-face
		       'epa-string)
       (dc/update-face 'which-key-local-map-description-face
		       'ef-themes-heading-5))))

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

;;** Keymaps

;; defining these AoT, since general-create-definer given :prefix-map will
;; create if it doesn't exist. not much of a difference, in the end, but i may
;; bind these smaller keymaps to other keys (like mouse buttons)

;; there may be a problem with trying to bind C-c to a keymap globally,
;; depending on how emacs assembles its keymaps.
(defvar dc/leader-map (make-sparse-keymap)
  "Global `C-c' keymap so prefixes can be rebound.")

;;*** Definers

;; only use keybindings in map variable names when they are global. and even
;; then, it's better to create small composable keymaps.

(general-create-definer cx-def
  :prefix-map 'ctl-x-map
  :prefix-command 'ctl-x-map)

(general-create-definer leader-def
  :prefix-map 'dc/leader-map
  :prefix-command 'dc/leader-map)

(general-define-key
 :keymap 'global
 ;; "<f2>" '(:prefix-command ctl-x-map)
 ;; "<f12>" '(:prefix-command dc/leader-map)
 "C-c" '(:prefix-command dc/leader-map))

;; mapping the function keys above only affects general.el keybindings
;;
;; TODO: consider removing <f2> and <f12> remaps (removes hands from home row
;; and there's little value in configuring code ¶oint chars if my keybindings
;; are not console compatible)
;;
;; (keymap-global-set "<f2>" ctl-x-map)
;; (keymap-global-set "<f12>" dc/leader-map)

;;*** Keymap Aliases

;; see also:
;; - https://github.com/noctuid/general.el#keymapstate-aliases
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Keys.html

;;**** Notes on keymap aliases

;; -------------------------------------------------------------------
;; setting function keys as prefixes doesn't work very well in xterm/console

;; console: req. kmscon or loadkeys (which is hardware specific) and even then
;; doesn't work well AFAIR

;; xterm (or alacritty/konsole/etc): this does seem to work, but may require
;; some custom config ... per terminal-emulator

;; -------------------------------------------------------------------
;; (keymap-global-set "<f12>" ctl-c-map)

;; there is no ctl-c map. the approach in dc-keys-old.el is messy and screws up
;; the which-key labels, but it does completely set <f12> as C-c where you need
;; it.

;; -------------------------------------------------------------------
;; TODO xkb: setup "AltGr-<f_x>" -> "<f_x+12>"
;; and if you buy right now, we'll double your function keys
;; chromebooks and macbooks not applicable
;; -------------------------------------------------------------------

;;* Keys

;;** Help

;; (defun dc/embark)

(general-define-key
 :keymaps 'help-map

 ;; "<f2>" '(:keymap dc/quick-map)

 ;; can insert values with embark
 "M-v" #'getenv

;;*** help-map: keybindings
 "M-k" #'describe-keymap
 ;; not interactive, also not sure whether it works
 ;; "C-k" (apply-partially #'embark-bindings-at-point)

;*** bindings
 "B" #'embark-bindings
 "M-b" #'embark-bindings-in-keymap
 "M-m" #'consult-minor-mode-menu
 "M-f" #'list-faces-display)

;;*** Info

(general-define-key
 :keymaps '(Info-mode-map)
 "a" #'info-apropos
 "C-o" #'casual-info-tmenu)

;;** Quick Map

;; NOTE: need to specify both global & help so f1 will substitute as C-h
;;
;; when general new prefixes maps on C-h help, which-key descriptions are not
;; set on <f1> prefixs
;;
;; general.el suggests managing which-key alists directly in some cases

(general-create-definer quick-def
  :prefix-map 'dc/quick-map
  :prefix-command 'dc/quick-map)

(general-define-key
 :keymaps 'help-map
 "<f2>" '(:prefix-command dc/quick-map :wk "QUICK"))

;; (defun dc/init-keybinds-quick ())
;; (dolist (pfx '("C-h" "<f1>")))

;; TODO: how to unquote/splat (append (list ...) (list)) directly with
;; `(,identity 'foo 'bar)

(quick-def
  "a" '(:ignore t :which-key "ALERT")
  "ao" #'alert--log-open-log
  "a M-c" #'alert--log-clear-log
  "ad" #'alert--log-enable-debugging
  "aD" #'alert--log-disable-debugging
  "al" #'alert--log-disable-logging
  "aL" #'alert--log-enable-logging
  "am" #'alert--log-enable-messaging
  "aM" #'alert--log-disable-messaging

  "@" '(:ignore t :which-key "ACTIVITIES")

  "@@" #'activities-list
  "@m" #'activities-mode
  "@k" #'activities-kill
  "@d" #'activities-discard
  "@n" #'activities-new
  "@r" #'activities-resume
  "@d" #'activities-revert
  "@$" #'activities-save-all
  "@q" #'activities-suspend
  "@b" #'activities-switch
  "@t" #'activities-tabs-mode

  "M-a" '(:ignore t :which-key "SWAGG")

  "M-a r" #'swagg-request
  "M-a R" #'swagg-request-with-rest-block
  "M-a f" #'swagg-request-with-fetch
  "M-a i" #'swagg-invalidate-cache
  "M-a d" #'swagg-display-headers


  "b" '(:ignore t :which-key "BUFFER ENV")
  "bd" #'buffer-env-describe
  "bu" #'buffer-env-update
  "br" #'buffer-env-reset

  "d" #'docker

  ;; already accessible from docker
  ;; "M-d" #'docker-compose

  "D" '(:ignore t :which-key "DESKTOP")
  "Ds" #'desktop-save-in-desktop-dir
  "DS" #'desktop-save
  "Dr" #'desktop-read

  "e" #'envrc-command-map
  "E" '(:ignore t :which-key "ENVRC")
  "Er" #'envrc-reload-all
  "Eg" #'envrc-global-mode

  "h" #'shortdoc

  ;; lookups
  "l" '(:ignore t :which-key "LOOKUP")
  "ln" '(:ignore t :which-key "NIST")
  "lnf" #'nist-webbook-formula
  "lnn" #'nist-webbook-name

  ;; local variables
  "M-l" '(:ignore t :which-key "LOCAL VARS")
  "M-l ad" #'add-dir-local-variable
  "M-l aF" #'add-file-local-variable
  "M-l af" #'add-file-local-variable-prop-line
  "M-l dd" #'delete-dir-local-variable
  "M-l dF" #'delete-file-local-variable
  "M-l df" #'delete-file-local-variable-prop-line
  "M-l k" #'kill-local-variable
  "M-l m" #'make-local-variable
  "M-l M" #'make-variable-buffer-local
  "M-l h" #'apropos-local-variable
  "M-l H" #'array-display-local-variables

  "O" #'aw-show-dispatch-help

  ;; "p"  '(:ignore t :wk "POPUP")
  "p" #'proced
  "P" #'pomm

  ;; "#" '(:ignore t :which-key "TRAMP")
  "T" '(:ignore t :which-key "TRAMP")
  "Tb" #'tramp-cleanup-all-buffers
  "Tc" #'tramp-cleanup-connection
  "TC" #'tramp-cleanup-this-connection
  "T M-c" #'tramp-cleanup-all-connections
  "TM" #'tramp-compat-set-file-modes
  "Td" #'tramp-setup-debug-buffer

  ;; tramp-change-syntax
  ;; tramp-rename-files
  ;; tramp-rename-these-files
  ;; tramp-unload-tramp
  ;; tramp-change-syntax?

  "r" #'repology

  "t" '(:ignore t :which-key "THEME")
  "tr" #'ef-themes-load-random
  "ts" #'ef-themes-select
  "tt" #'ef-themes-toggle

  ;; TODO check tramp-completion-use-auth-sources
  "Tg" #'tramp-crypt-add-directory

  ;; vterm
  "M-v" #'vterm

  ;; needs tramp-default-rename-alist
  ;; "Tr" #'tramp-rename-these-files
  ;; "TR" #'tramp-rename-files
  ;; "Td" #'tramp-setup-debug-buffer
  ;; #'tramp-change-syntax ;; default/simplified/separate

  "$" '(:ignore t :which-key "STRAIGHT")

  ;; straight -* utils
  "$4" #'straight-get-recipe
  "$$" #'straight-pull-recipe-repositories
  "$v" #'straight-visit-package
  "$V" #'straight-visit-package-website
  "$d" #'straight-dependencies
  "$D" #'straight-dependents
  ;; "$" #'straight-use-package

  ;; straight -package
  "$F" #'straight-pull-package
  "$ M-F" #'straight-fetch-package
  "$P" #'straight-push-package
  "$C" #'straight-check-package
  "$R" #'straight-rebuild-package
  "$M" #'straight-merge-package
  "$N" #'straight-normalize-package

  ;; straight -and-deps
  "$&" '(:ignore t :which-key "AND DEPS")
  "$& M-f" #'straight-fetch-package-and-deps
  "$&F" #'straight-pull-package-and-deps
  "$&m" #'straight-merge-package-and-deps

  ;; straight -all
  "$f" #'straight-pull-all
  "$ M-f" #'straight-fetch-all
  "$p" #'straight-push-all
  "$c" #'straight-check-all
  "$r" #'straight-rebuild-all
  "$M" #'straight-merge-all             ;oh boy!
  "$N" #'straight-normalize-all

  ;; thesaurus
  "M-t t" #'synosaurus-choose-and-insert
  "M-t M-t" #'synosaurus-choose-and-insert

  ;; vcs ops: merge, normalize
  ;; pull/fetch/merge-package-and-deps
  ;; freeze/thaw-versions
  ;; use-package-mode
  ;; watcher-start/stop
  "0" '(:ignore t :which-key "0x0")
  "0-" #'0x0-dwim
  "0t" #'0x0-upload-text
  "0f" #'0x0-upload-file
  "0k" #'0x0-upload-kill-ring
  "0p" #'0x0-popup
  "0u" #'0x0-shorten-uri)

(when (featurep 'consult-gh)
  (quick-def
    ;; use #'embark-select with SPC for embark integration
    "g" '(:ignore t :which-key "GH")
    "gc" #'consult-gh-repo-clone
    "gf" #'consult-gh-find-file
    "g M-f" #'consult-gh-repo-fork
    "gi" #'consult-gh-search-issues
    "go" #'consult-gh-orgs
    "gr" #'consult-gh-search-repos
    "gR" #'consult-gh-default-repos))

;;** Globals

(general-define-key
 :keymaps '(global)

 "C-M-|" #'dc/indent-buffer)

;;*** global-leader-key (C-x, f2)
;; this helps balance keyboard usage, giving and gives your pinky a break

;; this prefix should find itself associated with
;; editor features, global state and outward-looking functions

(cx-def
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

  ;; find-sibling-file commands need to (let ((sibling-file-rules)) ...)

  ;; find .h files for .c
  ;; ("\\([^/]+\\)\\.c\\'" "\\1.h")

  ;; find other worktrees/versions:
  ;; ("src/emacs/[^/]+/\\(.*\\)\\'" "src/emacs/.*/\\1\\'")

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

(cx-def
  "(" #'ignore
  ")" #'ignore)

;;*** leader-key (C-c, f12)

;; this prefix should find itself associated with project mgmt, minor mode
;; features and inward-looking functions

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

(general-define-key
 :keymaps '(global-map)
 "<f3>" #'casual-avy-tmenu)

(leader-def
  :wk-full-keys nil

  "j"   '(:ignore t :which-key "JUMP")
  "jj"  '(avy-goto-char :which-key "jump to char")
  "jw"  '(avy-goto-word-0 :which-key "jump to word")
  "jl"  '(avy-goto-line :which-key "jump to line"))

;; redirect F2 -> C-c (doesn't show everything on which-keys)
;; (general-define-key
;;  :keymaps 'global
;;  :wk-full-keys nil
;;  "<f2>" '(:prefix-command global-leader-prefix-command))

;; (general-define-key
;;  :keymaps 'global
;;  :wk-full-keys nil
;;  "<f7>" '(:prefix ctl-x-map))

;; (define-key global-map "")

;; (general-define-key
;;  :keymaps 'global
;;  :wk-full-keys nil
;;  "<f9>" '(:def ctl-x-map :keymap ctl-x-map))

;; this works in vanilla emacs
;; (keymap-global-set "<f7>" ctl-x-map)
;; (keymap-local-set "<f8>" ctl-x-map)

;;*** minibuffer-local-map

(general-define-key
 :keymaps 'minibuffer-local-map

 ;; instead just use "C-S-<backspace>" #'kill-whole-line (no need to remap defaults)
 "C-<backspace>" #'delete-minibuffer-contents

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

;; TODO: transition these to use [remap ...]

(general-define-key
 :keymaps 'global
 :wk-full-keys nil

 "C-x d" '(:ignore t :which-key "DIR")
 "C-x d a" #'consult-dir                ;consult-dir all sources
 "C-x d d" #'dc/consult-dir-recentf
 "C-x C-d" #'dired

 "C-s" #'isearch-forward
 "C-S" #'consult-line

 "C-M-j" #'consult-buffer
 "C-M-l" #'consult-imenu
 ;; "C-M-." #'embark-act

 "C-c M-x" #'consult-mode-command
 "C-c h" #'consult-history

 ;; C-x bindings (ctl-x-map
 "C-x M-:" #'consult-complex-command ;; orig. repeat-complex-command

 ;; TODO: remap these?
 "C-x b"   #'consult-buffer
 "C-x M-b" #'ibuffer
 "C-x ·" #'bufler                       ; C-x altgr-b

 "C-x C-b" #'bufler-switch-buffer
 "C-x B"   #'display-buffer

 ;; "C-x 4 b" #'consult-buffer-other-window ;; orig. switch-to-buffer-other-window
 ;; "C-x 5 b" #'consult-buffer-other-frame ;; orig. switch-to-buffer-other-frame
 ;; "C-x r b" #'consult-bookmark ;; orig. bookmark-jump
 "C-x p b" #'consult-project-buffer ;; orig. project-switch-to-buffer

 ;; ;; Custom M-# bindings for fast register access
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
 "f" '(:ignore t :wk "FLY")
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
 "rl" #'consult-org-roam-forward-links)

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
 "S" #'swiper
 "M-s" #'consult-yasnippet
 "M-S" #'consult-yasnippet-visit-snippet-file
 "u" #'consult-focus-lines

 ;; Isearch integration
 "e" #'consult-isearch-history)

(general-define-key
 :keymaps 'isearch-mode-map

 ;; "<f2>" #'casual-isearch-tmenu
 "C-o" #'casual-isearch-tmenu

 "M-e" #'consult-isearch-history   ;; orig. isearch-edit-string
 "M-s e" #'consult-isearch-history ;; orig. isearch-edit-string
 "M-s l" #'consult-line            ;; needed by consult-line to detect isearch
 "M-s L" #'consult-line-multi      ;; needed by consult-line to detect isearch
 )

;; (leader-def
;;   :keymaps 'global
;;   "a" #'embark-act)

;;**** vertico-map

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

;;**** corfu-map

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

(general-define-key
 :keymaps '(ctl-x-r-map)
 :wk-full-keys nil
 "B" '(:ignore t :which-key "BURLY")
 "Bo" #'burly-open-bookmark
 "BO" #'burly-open-url
 "Bw" #'burly-bookmark-windows
 "Bf" #'burly-bookmark-frames
 "BB" #'burly-kill-buffer-url
 "BF" #'burly-kill-frames-url
 "BW" #'burly-kill-windows-url)

;;*** Window Management

;;*** Shell

;; TODO: map C-c M-! to :which-key and allow selecting shell scripts
;; that paste output to buffer
;;
;; edit so output is more friendly to insertion
;; guix search emacs-srv | recsel -p name,description

;;*** Lispy

;; TODO: determine why the lispy-x hydra req. manual eval to run
;;  shows in message buffer sometimes.

;; (defun dc/special-lispy-eval-and-comment ()
;;   (interactive))

;; unused in lispy: ?XTYUL

(general-define-key
 :keymaps '(lispy-mode-map)
 "M-<up>" #'outline-backward-same-level ;; #'lispy-outline-prev
 "M-<down>" #'outline-forward-same-level ;; #'lispy-outline-next
 "M-<left>" #'outline-up-heading ;; #'lispy-outline-left
 "M-<right>" #'lispy-outline-right
 ;; [remap lispy-outline-promote] #'outline-promote
 ;; [remap lispy-outline-demote] #'outline-demote

 "M-S-<up>" #'outline-move-subtree-up
 "M-S-<down>" #'outline-move-subtree-down
 "M-S-<left>" #'outline-demote
 "M-S-<right>" #'outline-promote

 ;; "E" #'lispy-eval-and-comment
 "é" #'lispy-eval-and-insert            ;e

 "å" #'lispy-to-cond                    ;w
 "á" #'lispy-to-lambda                  ;a
 "ß" #'lispy-to-ifs                     ;d
 "ð" #'lispy-to-defun                   ;s

 "»" #'lispy-stringify                  ;]
 "”" #'lispy-unstringify                ;}

 ;; altgr+jk
 "œ" #'lispy-insert-outline-left        ;j
 "ï"  #'lispy-insert-outline-below

 "¬" #'outline-insert-heading           ;/
 [remap lispy-shifttab] #'outline-cycle-buffer)

;; | nN pP | ñÑ öÖ |
;; | fF bB | fF ·/ | (same f)
;; | aA eE | áé ÁÉ |
;; | dD uU | ðÐ úÚ |
;; | vV    | ®™    |

;; composed altgr chars can be mapped?
;; "ɇ" #'outline-insert-heading ; (altgr+b)e

;;  ;; altgr+wads (on us intl altgr + deadkeys, maybe custom keyboard)
;;  "Å" #'lispy-move-up                    ;W
;;  ;; "Á" #'lispy-move-left               ;A
;;  "§" #'lispy-move-down                  ;D
;;  ;; "Ð" #'lispy-move-right              ;S

;;  ;; "Å" #'lispy-move-outline-up      ;W
;;  ;; "§" #'lispy-move-outline-up      ;S
;;  "»" #'lispy-stringify
;;  "”" #'lispy-unstringify

;;  ;; altgr+jk
;;  "œ" #'lispy-insert-outline-left
;;  "ï" #'lispy-insert-outline-below

;; most of these are already bound under the lispy-x functionality
;; https://github.com/abo-abo/lispy#features

;;** Doom

;;*** Prefixes (C-c)

;; these prefixes should only be defined once. i think the which-key problem
;; results from the labels not being rebuilt. instead of the commands you'd
;; expect to see, these labels on the global map are shown

(leader-def
  :wk-full-keys nil
  "c" '(:ignore t :wk "CODE")
  "e" '(:ignore t :wk "EVAL")
  "f" '(:ignore t :wk "FILE")
  "i" '(:ignore t :wk "INSERT")
  ;; "m" '(:ignore t :wk "M/CURSOR")
  ;; "n" '(:ignore t :wk "ORG/NOTES")
  "o" '(:ignore t :wk "OPEN")
  "q" '(:ignore t :wk "QUIT")
  "r" '(:ignore t :wk "REMOTE")
  "s" '(:ignore t :wk "SEARCH")
  ;; "t" '(:ignore t :wk "TOGGLE")
  "v" '(:ignore t :wk "VCS")
  "w" '(:ignore t :wk "WORKSPACE")

  ;; "&" '(:ignore t :wk "SNIPPET")
  ;; "7" '(:ignore t :wk "SNIPPET")
  "!" '(:ignore t :wk "FLYMAKE")
  "1" '(:ignore t :wk "FLYCHECK")

  ;; "C-f" '(:ignore t :wk "FOLD") ;; imenu > folding
  )

;;*** & 7 SNIPPETS

;; TODO consider moving the snippets bindings (and just use consult)

(general-define-key
 :keymaps '(yas-minor-mode-map)
 :prefix "C-c"
 :wk-full-keys nil

 "7" '(:ignore t :wk "SNIPPETS")
 "7n" #'yas-new-snippet
 "7i" #'yas-insert-snippet
 "7/" #'yas-visit-snippet-file
 "7r" #'yas-reload-all
 ;; "7c" #'aya-create
 ;; "7e" #'aya-expand

 "&" '(:ignore t :wk "SNIPPETS")
 "&n" #'yas-new-snippet
 "&i" #'yas-insert-snippet
 "&/" #'yas-visit-snippet-file
 "&r" #'yas-reload-all)

;; this looped setup runs fine
;; (dc/init-keybinds-yasnippet)

;;*** ! 1 Flycheck

;; the +FLY prefix shows up the most often in which-keys
;; flycheck-mode-map requires having invoked the mode
;; (with-eval-after-load 'flycheck
;;   (general-translate-key
;;     nil '(flycheck-mode-map)
;;     "C-c 1" "C-c !"
;;     "<f12> 1" "C-c !"
;;     "<f12> 1" "C-c !"))

;;*** c CODE

(leader-def
  :wk-full-keys nil
  "cc" #'compile
  "cC" #'recompile
  "cw" #'delete-trailing-whitespace
  ;; "W" #'doom/deletr-trailing-newlines
  ;; "x" #'+default/diagnostics
  )

;;**** LSP
;; TODO: LSP UI bindings?

;; :when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))

;; :desc "LSP Code actions"
;; "a"   #'lsp-execute-code-action
;; :desc "LSP Organize imports"
;; "o"   #'lsp-organize-imports
;; :desc "LSP Rename"
;; "r"   #'lsp-rename
;; :desc "LSP"
;; "l"   #'+default/lsp-command-map

;; :when (modulep! :completion vertico)

;; :desc "Jump to symbol in current workspace"
;; "j"   #'consult-lsp-symbols
;; :desc "Jump to symbol in any workspace"
;; "J"   (cmd!! #'consult-lsp-symbols 'all-workspaces)

;;**** EGLOT

;; see the `eglot-menu', whose options change depending on the result of
;; (eglot--server-capable :lsp-capability)

(general-define-key
 :keymaps 'eglot-mode-map
 :prefix "M-g"
 :wk-full-keys nil
 "c" '(:ignore t :which-keys "CODE")
 "cs" #'consult-eglot-symbols
 "cd" #'eldoc-doc-buffer

 ;; eglot delegates to flymake for a error interface (NOT FLYCHECK GDMT)
 "c M-d" '(:ignore t :which-keys "diagnostics")
 "c M-d d" #'flymake-show-buffer-diagnostics
 "c M-d M-d" #'flymake-show-project-diagnostics

 ;; code actions (still haven't ever seen one of these happen so IDK)
 "ca" '(:ignore t :which-keys "ACTIONS")
 "caa" #'eglot-code-actions
 "cao" #'eglot-code-action-organize-imports
 "cax" #'eglot-code-action-extract
 "cai" #'eglot-code-action-inline
 "car" #'eglot-code-action-rewrite
 "caq" #'eglot-code-action-quickfix

 "cf" '(:ignore t :which-keys "FIND")
 "cfd" #'eglot-find-declaration
 "cfi" #'eglot-find-implementation
 "cft" #'eglot-find-typeDefinition
 ;; xref-find-definitions
 ;; xref-find-apropos
 ;; xref-find-find-references

 "cF" #'eglot-format
 "c M-f" #'eglot-format-buffer

 ;; "cj" #'consult-eglot-symbols
 "cr" #'eglot-rename)

(general-define-key
 :keymaps 'eglot-mode-map
 :prefix "M-G"
 :wk-full-keys nil
 "E" '(:ignore t :which-keys "EGLOT")
 "EB" #'eglot-events-buffer
 "E <SPC>" #'eglot-show-workspace-configuration
 "EE" #'eglot-stderr-buffer
 "EI" #'eglot-inlay-hints-mode
 "EC" #'eglot-clear-status
 "EU" #'eglot-upgrade-eglot
 "EL" #'eglot-list-connections)


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
  :wk-full-keys nil
  "ie" #'emojify-insert-emoji
  ;; "if" #'+default/insert-file-path
  ;; "iF" (cmd!! #'+default/insert-file-path t)
  ;; "iy" #'+default/yank-pop
  "iu" #'insert-char)

;;*** m M/CURSOR

(general-create-definer multiple-cursors-def
  :prefix-map 'dc/multiple-cursors-map
  :prefix-command 'dc/multiple-cursors-map)

(general-define-key
 :keymaps 'dc/leader-map
 "m" '(:prefix-command dc/multiple-cursors-map :wk "MULTIBALL"))

(multiple-cursors-def
  "l" #'mc/edit-lines
  "n" #'mc/mark-next-like-this
  "N" #'mc/unmark-next-like-this
  "p" #'mc/mark-previous-like-this
  "P" #'mc/unmark-previous-like-this
  "t" #'mc/mark-all-like-this
  "m" #'mc/mark-all-like-this-dwim
  "e" #'mc/edit-ends-of-lines
  "a" #'mc/edit-beginnings-of-lines
  "s" #'mc/mark-sgml-tag-pair
  "d" #'mc/mark-all-like-this-in-defun
  "<mouse-1>" #'mc/add-cursor-on-click)

;;*** n ORG

(general-create-definer org-x1-def
  :prefix-map 'dc/org-x1-map
  :prefix-command 'dc/org-x1-map)

(general-define-key
 :keymaps 'org-mode-map
 "C-x 1" '(:prefix-command dc/org-x1-map :wk "ORG"))

(general-create-definer org-agenda-global-def
  :prefix-map 'dc/org-agenda-global-map
  :prefix-command 'dc/org-agenda-global-map)

(general-define-key
 :keymaps 'dc/leader-map
 "r" '(:prefix-command dc/org-agenda-global-map :wk "AGENDA"))

(general-create-definer org-roam-global-def
  :prefix-map 'dc/org-roam-global-map
  :prefix-command 'dc/org-roam-global-map)

(general-define-key
 :keymaps 'dc/leader-map
 "nr" '(:prefix-command dc/org-roam-global-map :wk "ROAM"))

(general-create-definer org-clock-global-def
  :prefix-map 'dc/org-clock-global-map
  :prefix-command 'dc/org-clock-global-map)

(general-define-key
 :keymaps 'ctl-x-map
 "1" '(:prefix-command dc/org-clock-global-map :wk "CLOCK"))

;;  ... wellll that is unfortunate
;; (nthcdr 0 '(lambda (foo bar) '(1 2 3 )))
;; (nthcdr 1 #'(lambda (foo bar) '(1 2 3 )))
;; (nthcdr -2 #'(lambda (foo bar) '(1 2 3 )))
;; (eql t 1)
;; (declare-function org-clock-in-complex-plane)
;; (apply-partially #'org-clock-in (lsh 1 2)) ;req setf then push

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

(org-x1-def
  "s" #'org-schedule
  "d" #'org-deadline
  "c" #'org-ctrl-c-ctrl-c

  "2" '(:ignore t :wk "ORG")
  " <tab>" #'dc/org-clock-in-recent                        ; 4 select from recent
  "2 <tab>" #'dc/org-clock-in-continue-from-last-timestamp ; 64 continuously
  "23 <tab>" #'dc/org-clock-in-and-mark-default            ; 16 mark default

  ;; "2a" #'org-archive-subtree-default
  ;; "2b" #'org-toggle-checkbox
  "2c" #'org-columns
  "2e" #'org-clock-modify-effort-estimate
  "2f" #'org-emphasize
  "2j" #'org-clock-goto
  "2l" #'org-latex-preview
  "2n" #'org-next-link
  "2o" #'org-clock-out
  "2p" #'org-previous-link
  "2q" #'org-clock-cancel
  ;; "2r" #'org-toggle-radio-button
  ;; "2s" #'org-archive-subtree
  "2t" #'org-toggle-time-stamp-overlays
  ;; "2u" #'org-dblock-update
  "2v" #'org-toggle-inline-images
  ;; "2w" #'org-cut-special
  "2x" #'org-clock-in-last
  ;; "2y" #'org-paste-special
  "2z" #'org-resolve-clocks)

(org-clock-global-def
  "o" #'org-clock-out
  "M-c" #'org-clock-cancel               ; and remove start time
  ;; "1C" #'+org/toggle-last-clock
  "g" #'org-clock-goto
  "z" #'org-resolve-clocks
  "q" #'org-clock-cancel
  "j" #'org-clock-goto
  "d" #'org-clock-display
  "x" #'org-clock-in-last
  "e" #'org-clock-modify-effort-estimate)

(org-agenda-global-def
  "a" #'org-agenda                     ; nan

  "M-C" #'org-clock-cancel            ; and remove start time
  "g" #'org-clock-goto
  "o" #'org-clock-goto

  "l" #'org-store-link
  " M-l" #'org-insert-link
  "m" #'org-tags-view

  "n" #'org-capture
  "N" #'org-capture-goto-target
  "t" #'org-todo-list


  "v" #'org-search-view)

;; "C" #'+org/toggle-last-clock
;; "." #'+default/search-notes-for-symbol-at-point
;; "b" #'citar-open-notes
;; TODO: "oc"  '(org-capture t :which-key "capture"); universal arg?
;; " M-n"  #'org-toggle-narrow-to-subtree
;; "s" #'+default/org-notes-search
;; "S" #'+default/org-notes-headlines
;; "y" #'+org/export-to-clipboard
;; "Y" #'+org/export-to-clipboard-as-rich-text

(org-roam-global-def
  "a" #'org-roam-node-random
  "f" #'org-roam-node-find
  "F" #'org-roam-ref-find
  "g" #'org-roam-graph
  "i" #'org-roam-node-insert
  "L" #'org-roam-link-replace-all
  "n" #'org-roam-capture
  "r" #'org-roam-buffer-toggle
  "s" #'org-roam-db-sync
  "t" #'org-roam-tag-add
  "T" #'org-roam-tag-remove

  ;; in terms of paredit commands
  "M-}" #'org-roam-extract-subtree       ; barf
  "M-)" #'org-roam-refile                ; slurp
  "M-r" #'org-roam-promote-entire-buffer ; raise (or M-u, up)
  "M-?" #'org-roam-demote-entire-buffer  ; convolute (or M-d, down)

  "d" '(:ignore t :wk "DAILY")
  "d-" #'org-roam-dailies-find-directory
  "db" #'org-roam-dailies-goto-previous-note
  "dd" #'org-roam-dailies-goto-date
  "dD" #'org-roam-dailies-capture-date
  "df" #'org-roam-dailies-goto-next-note
  "dm" #'org-roam-dailies-goto-tomorrow
  "dM" #'org-roam-dailies-capture-tomorrow
  "dn" #'org-roam-dailies-capture-today
  "dt" #'org-roam-dailies-goto-today
  "dT" #'org-roam-dailies-capture-today
  "dy" #'org-roam-dailies-goto-yesterday
  "dY" #'org-roam-dailies-capture-yesterday)


(general-define-key
 :keymaps 'org-mode-map
 :prefix "C-c"
 :wk-full-keys nil
 "4" 'org-archive-subtree

 "n" '(:ignore t :wk "ORG/NOTES")
 "nr" '(:ignore t :wk "ROAM")
 "nrR" #'org-roam-buffer-display-dedicated
 "nr M-r" #'org-roam-link-replace-all

 ;; "nx" '(org-export-dispatch t)
 ;; TODO: "nx" '(org-export-dispatch t) ; universal arg?
 )

;; TODO: make a keymap for org-???-functions
;; (local-leader-def
;;   :keymaps 'org-mode-map

;;   "a" #'org-roam-alias-add
;;   "A" #'org-roam-alias-remove
;;   "t" #'org-roam-tag-add
;;   "T" #'org-roam-tag-remove
;;   "r" #'org-roam-ref-add
;;   "R" #'org-roam-ref-remove)

;;*** o OPEN

;; "o" nil ; we need to unbind it first as Org claims this prefix
;; "o" . "open"
;; "b"  #'browse-url-of-file
;; "d"  #'+debugger/start
;; "r"  #'+eval/open-repl-other-window
;; "R"  #'+eval/open-repl-same-window

;; :when (modulep! :term vterm)
;; "t" #'+vterm/toggle
;; "T" #'+vterm/here

;; :when (modulep! :term eshell)
;; "e" #'+eshell/toggle
;; "E" #'+eshell/here

;;*** p PROJECTILE

;; TODO: move project keybindings out of projectile section (C-c p #'capf)

;; TODO: rebind C-x C-f to #'project-find-file, C-u C-x C-f to #'find-file

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

;; "f" #'dc/project-find-regexp
;; "M-f" #'project-find-file


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

(general-define-key
 :keymaps '(org-mode-map latex-mode-map)
 :prefix "C-c"
 :wk-full-keys nil

 "t" '(:ignore t :which-key "TOGGLE")
 "t©" #'cdlatex-mode

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

(general-create-definer toggle-def
  :prefix-map 'dc/toggle-map
  :prefix-command 'dc/toggle-map)

(general-define-key
 :keymaps 'dc/leader-map
 "t" '(:prefix-command dc/toggle-map :wk "TOGGLE"))

(toggle-def
  :wk-full-keys nil
  "1" #'flycheck-mode
  "!" #'flymake-mode
  "b" #'bufler-sidebar
  "c" '(:ignore t :wk "COMPLETION")
  "cc" #'dc/toggle-completion-ignore-case
  "cb" #'dc/toggle-read-buffer-completion-ignore-case
  "cf" #'dc/toggle-read-file-name-completion-ignore-case
  "C" #'global-display-fill-column-indicator-mode
  "M-C" #'corfu-mode
  "M-c" #'corfu-popupinfo-mode
  "D" #'toggle-debug-on-error
  "F" #'format-other-mode
  "M-f" #'toggle-frame-fullscreen
  "G" #'git-timemachine-toggle
  "M-g" #'gud-tooltip-mode
  ;; "i" #'highlight-indent-guides-mode
  ;; "I" #'doom/toggle-indent-style"
  "l" #'display-line-numbers-mode
  ;; "p" #'org-tree-slide-mode
  "s" #'flyspell-mode
  "ß" #'superword-mode
  "§" #'subword-mode
  ;; "M-u" #'dc/toggle-casual-unicode
  "v" #'visual-line-mode
  "V" #'visual-fill-column-mode
  "C-v" #'vertico-multiform-mode
  "M-v" #'visible-mode
  ;; "w" #'visual-line-mode
  ;; "w" #'+word-wrap-mode
  "N" #'dc/toggle-native-comp-async-report-warnings-errors)

;; TODO map this to a list of *-ts-modes
;; NOTE for some reason, this seems to be overriding "t"'s self-insert functionality
(general-define-key
 :keymaps '(prog-mode-map)
 :prefix "C-c t"
 "t" #'treesit-explore-mode)

;;**** dired

;;***** dired toggles

(general-create-definer dired-toggle-def
  :prefix-map 'dc/dired-toggle-map
  :prefix-command 'dc/dired-toggle-map)

(general-define-key
 :keymaps 'dired-mode-map
 "C-c td" '(:prefix-command dc/dired-toggle-map :wk "DIRED")
 ;; altgr-i
 "í" #'dired-kill-subdir)

(dired-toggle-def
  "a" #'dired-async-mode
  "A" #'all-the-icons-dired-mode
  "c" #'dired-collapse-mode
  "f" #'dired-filter-mode
  "g" #'turn-on-gnus-dired-mode
  "h" #'dired-hide-details-mode
  "i" #'dired-utils-format-information-line-mode
  "o" #'dired-omit-mode
  "v" #'dired-virtual-mode
  "c" #'dired-collapse-mode)

;;**** org toggles

;; (general-unbind org-mode-map
;;   "tf"
;;   "to")

;; (general-define-key
;;  :keymaps 'org-mode-map
;;  "t" #'self-insert-command)

(general-create-definer org-toggle-def
  :prefix-map 'dc/org-toggle-map
  :prefix-command 'dc/org-toggle-map)

(general-define-key
 :keymaps 'org-mode-map
 "C-c t" '(:prefix-command dc/org-toggle-map :wk "TOGGLE"))

(general-define-key
 :keymaps '(dc/org-toggle-map)

 "f" #'org-table-toggle-formula-debugger
 "o" #'org-table-toggle-coordinate-overlays
 "t" #'treesit-explore-mode)

;;**** markdown toggles

(general-create-definer markdown-toggle-def
  :prefix-map 'dc/markdown-toggle-map
  :prefix-command 'dc/markdown-toggle-map)

(general-define-key
 :keymaps 'markdown-mode-map
 "C-c tm" '(:prefix-command dc/markdown-toggle-map :wk "MARKDOWN"))

(markdown-toggle-def
  "e" #'markdown-toggle-math
  "f" #'markdown-toggle-fontify-code-blocks-natively
  "i" #'markdown-toggle-inline-images
  "l" #'markdown-toggle-url-hiding
  "m" #'markdown-toggle-markup-hiding
  "w" #'markdown-toggle-wiki-links
  "x" #'markdown-toggle-gfm-checkbox)

;;*** v VCS

;; TODO can magit-wip-mode be localized?

(leader-def
  ;; git link
  "v M-g l" #'git-link
  "v M-g c" #'git-link-commit
  "v M-g h" #'git-link-homepage

  ;; magit
  "v SPC" #'magit-status

  ;; magit: recommended bindings
  "vg" #'magit-file-dispatch
  "vs" #'magit-stage-file
  "vs" #'magit-stage-buffer-file
  "vu" #'magit-unstage-file
  "vu" #'magit-unstage-buffer-file

  "v,x" #'magit-file-untrack
  "v,r" #'magit-file-rename
  "v,k" #'magit-file-delete
  "v,c" #'magit-file-checkout

  "vD" #'magit-diff
  "vd" #'magit-diff-buffer-file

  "vL" #'magit-log
  "vl" #'magit-log-buffer-file
  "vt" #'magit-log-trace-definition
  "vM" #'magit-log-merged

  "vB" #'magit-blame
  "vb" #'magit-blame-addition
  "vr" #'magit-blame-removal            ; only blobs
  "vf" #'magit-blame-reverse            ; only blobs
  "vm" #'magit-blame-echo
  "vq" #'magit-blame-quit

  "vp" #'magit-blob-previous
  "vn" #'magit-blob-next

  "vv" #'magit-find-file
  "vV" #'magit-blob-visit-file
  ;; "vg" #'magit-status-here
  "vG" #'magit-display-repository-buffer
  "vc" #'magit-commit
  "ve" #'magit-edit-line-commit

  ;; magit: there's always more
  ;; "r" #'magit-list-repositories
  ;; "g" #'magit-find-git-config-file
  ;; "f" #'magit-commit-fixup
  )

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

;;** Tools

;;*** Calc

(general-define-key
 :keymaps '(calc-mode-map calc-alg-map)
 "C-o" #'casual-calc-tmenu)

;;*** Proced

(general-define-key
 :keymaps 'proced-mode-map
 ;; can also sort by clicking the header
 "sh" #'proced-sort-header)

;;*** Systemd

;; auto-load-alist

;;*** Prodigy.el

;;*** Detached.el

;; TODO: move keybinds here from dc-tools.el

;;** Lang

;; The f5-f8 keys should be reserved for translation to prefixes on a
;; per-major-mode basis. Hopefully, I can assume that no other key bindings will
;; interfere

;;*** comint-mode
;; repls go here

;;**** geiser-repl-mode

;;*** lisp-mode

;; to debug with lispy, use xe (like C-x C-e) which i think works now
;; that i have a new versiion of lispy

(general-define-key
 :keymaps '(emacs-lisp-mode-map)
 :prefix "C-c l"

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
;;     (:prefix ("a" . "add/arguments"))
;;     (:prefix ("b" . "barf"))
;;     (:prefix ("c" . "contract"))
;;     (:prefix ("d" . "debug"))
;;     (:prefix ("e" . "expand/extract"))
;;     (:prefix ("i" . "inject/inline/introduce"))
;;     (:prefix ("l" . "localize/log"))
;;     (:prefix ("o" . "organize"))
;;     (:prefix ("r" . "rename"))
;;     (:prefix ("s" . "slurp/split/string"))
;;     (:prefix ("t" . "toggle"))
;;     (:prefix ("u" . "unwrap"))
;;     (:prefix ("v" . "var"))
;;     (:prefix ("w" . "wrap"))
;;     (:prefix ("3" . "ternary")))

;;**** terraform-mode

;;*** text-mode

;;**** org-mode

;;***** org-sidebar

;; split out to define separate keys?
(general-define-key
 :keymaps '(org-mode-map)
 :wk-full-keys nil
 :prefix "<f7>"
 "SPC" #'org-sidebar-tree-toggle
 "C-SPC" #'org-sidebar-toggle
 "q SPC" #'org-ql-view-sidebar
 "M-q" #'org-sidebar-ql)

;; org-sidebar will create empty/conusing results
;; org-ql-view-list-map
;; org-sidebar-map
;; org-sidebar-tree-map

;; org-sidebar-tree-map

;; | cycle               | <tab>            |
;; | cycle-global        | S-<iso-lefttab>  |
;; | cycle-global        | S-<tab>          |
;; | cycle-global        | <backtab>        |
;; | cycle-mouse         | <triple-mouse-2> |
;; | cycle-mouse         | <double-mouse-2> |
;; | cycle-mouse         | <mouse-2>        |
;; | jump                | <return>         |
;; | jump-branches-mouse | <drag-mouse-1>   |
;; | jump-entries-mouse  | <drag-mouse-2>   |
;; | jump-mouse          | <triple-mouse-1> |
;; | jump-mouse          | <double-mouse-1> |
;; | jump-mouse          | <mouse-1>        |

;;***** org-ql

(general-define-key
 :keymaps '(org-mode-map)
 :wk-full-keys nil
 :prefix "<f7>"
 "C-q r" #'org-ql-refile)

;; org-ql-sparse-tree
;; org-ql-view-recent-items
;; org-ql-search
;; org-ql-refile
;; org-ql-find (-in-agenda, in-org-directory)

;; org-ql-view-map:

;; org-ql-view-recent-items

;; | c      | org-ql-view-customize |
;; | g or r | org-ql-view-customize |
;; | v      | org-ql-view-dispatch  |

;; the org ql transient dispatch gives more commands

;;**** markup modes

;; NOTE: combobulate processes data from keybindings events
;; https://github.com/mickeynp/combobulate
;; changing combobulate prefix requires setting this (and maybe restart)

;;  (setq combobulate-key-prefix "C-c o")


(general-define-key
 :keymaps '(html-ts-mode-map web-mode-map mhtml-mode-map xml-mode-map)
 :prefix "<f3>"
 "<f3>" #'dc/emmet-expansion-push)

(general-define-key
 :keymaps '(html-ts-mode-map web-mode-map mhtml-mode-map xml-mode-map)
 "C-M-i" #'completion-at-point)

;; (general-define-key
;;  :keymaps '(html-ts-mode-map web-mode-map mhtml-mode-map xml-mode-map)
;;  :prefix "C-c o"
;;  "o" #'combobulate)


;;**** sgml-mode

;;**** html-mode

;;** Mode-Specific

;; TODO: bind this in the mode
(defvar x509-mode-map (make-sparse-keymap))

;; ahhhh ok, so the x509 "e" key shows you the params for the command
(general-define-key
 :keymaps '(x509-mode-map)
 :prefix "C-c l"
 "d" #'x509-dwim)

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

;;* Post

;; the keybindings riding on the help-map must initialize after emacs starts.
;; some caution is needed when breaking their components into keymaps.

;; (add-hook 'emacs-startup-hook #'dc/init-keybinds-quick)
;; (add-hook 'emacs-startup-hook #'dc/init-keybinds-help)
;; (add-hook 'emacs-startup-hook #'dc/keymaps-bind-to-maps)

;;** Main Keymaps

;; TODO: trying to bind these-maps more than once doesn't work (without breaking console)

;; These need to be rebound after general.el is done

;; (general-define-key
;;  :keymaps 'global
;;  "<f3>" '(:prefix-command search-map))

;; (general-define-key
;;  :keymaps 'global
;;  "<f4>" '(:prefix-command goto-map))

(provide 'dc-keys)
