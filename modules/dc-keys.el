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


;;* Keys
;; see ./emacs/$version/lisp/bindings.el for defaults



;;** Globals

(general-define-key
 :keymaps 'global

 "C-x o" #'ace-window
 "C-x C-d" #'consult-dir

 ;; (define-key evil-window-map "u" 'winner-undo)
 ;; (define-key evil-window-map "U" 'winner-redo)

 "C-M-k" #'tab-bar-switch-to-tab
 "C-<next>" #'tab-bar-switch-to-next-tab
 "C-<prior>" #'tab-bar-switch-to-prev-tab)

(general-define-key
 :keymaps '(global help)
 :prefix "C-h"

 ;; can insert values with embark
 "M-v" #'getenv
 "B" #'embark-bindings
 "M-b" #'embark-bindings-in-keymap)

;;*** unbind function keys
;; or use the following (which may only work for general definitions)
;; (general-auto-unbind-keys)

(defvar dc/keys-unbound-at-init
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

    ;; tab prev-next
    "C-<tab>"
    "C-S-<tab>"

    ;; unbind menubar shortcuts
    ;; TODO what to do about terminal compat
    "<f10>"                             ;menu-bar-open
    "S-<f10>"                           ;context-menu-open
    "C-<f10>"                           ;buffer-menu-open
    "M-<f10>"                           ;toggle-frame-maximized
    "ESC <f10>"))                ;toggle-frame-maximized

;;**** trying to pack a lambda into a symbol
;; (fset 'dc/unbind-key (macroexpand-all '(unbind-key k)))
;; (seq-do (symbol-function 'dc/unbind-key) dc/keys-unbound-at-init)

(seq-do (lambda (key) (unbind-key key)) dc/keys-unbound-at-init)

;; TODO xkb: setup "AltGr-<f_x>" -> "<f_x+12>"
;; and if you buy right now, we'll double your function keys
;; chromebooks and macbooks not applicable

;;*** global-leader-key (C-x, f2)
;; this helps balance keyboard usage, giving and gives your pinky a break

;; this prefix should find itself associated with
;; editor features, global state and outward-looking functions
(global-leader-def
  "g" #'guix
  "G" #'guix
  "T" #'tldr
  "<left>" #'winner-undo
  "<right>" #'winner-redo)

;;*** leader-key (C-c, f12)

;; this prefix should find itself associated with
;; project mgmt, minor mode features and inward-looking functions

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
 "C-c C-;" #'embark-export
 "C-c C-l" #'embark-collect
 ;; "C-c C-e" #'+vertico/embark-export-write

 "M-s" #'consult-history ;; orig. next-matching-history-element
 "M-r" #'consult-history

 "M-A" #'marginalia-cycle)

;;*** Consult

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
 [remap yank-pop]                      #'consult-yank-pop)

(general-define-key
 :keymaps 'global

 "C-x C-d" #'consult-dir
 "C-s" #'consult-line
 "C-M-j" #'consult-buffer
 "C-M-l" #'consult-imenu
 ;; "C-M-." #'embark-act

 "C-c M-x" #'consult-mode-command
 "C-c h" #'consult-history

 ;; C-x bindings (ctl-x-map
 "C-x M-:" #'consult-complex-command     ;; orig. repeat-complex-command
 "C-x b" #'consult-buffer                ;; orig. switch-to-buffer
 "C-x 4 b" #'consult-buffer-other-window ;; orig. switch-to-buffer-other-window
 "C-x 5 b" #'consult-buffer-other-frame  ;; orig. switch-to-buffer-other-frame
 "C-x r b" #'consult-bookmark            ;; orig. bookmark-jump
 "C-x p b" #'consult-project-buffer      ;; orig. project-switch-to-buffer

 ;; Custom M-# bindings for fast register access
 "M-#" #'consult-register-load
 "M-'" #'consult-register-store ;; orig. abbrev-prefix-mark (unrelated
 "C-M-#" #'consult-register

 ;; Other custom bindings
 "M-y" #'consult-yank-pop ;; orig. yank-pop
 )

;;**** goto-map (consult)
;; M-g bindings
(general-define-key
 :keymaps 'goto
 "e" #'consult-compile-error
 "f" #'consult-flymake     ;; Alternative: consult-flycheck
 "g" #'consult-goto-line   ;; orig. goto-line
 "M-g" #'consult-goto-line ;; orig. goto-line
 "o" #'consult-outline     ;; Alternative: consult-org-heading
 "m" #'consult-mark
 "k" #'consult-global-mark
 "i" #'consult-imenu
 "I" #'consult-imenu-multi
 )

;;**** search-map (consult)
 ;; M-s bindings
(general-define-key
 :keymaps 'search
 "d" #'consult-find
 "D" #'consult-locate
 "g" #'consult-grep
 "G" #'consult-git-grep
 "r" #'consult-ripgrep
 "l" #'consult-line
 "L" #'consult-line-multi
 "k" #'consult-keep-lines
 "u" #'consult-focus-lines

 ;; Isearch integration
 "e" #'consult-isearch-history
 )

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

 "C-x C-d" #'consult-dir
 "C-x C-j" #'consult-dir-jump-file
 )

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

 ;; for orderless queries, maintain completion with spaces
 "C-f" #'corfu-insert-separator

 ;; to access embark actions
 "M-m" #'corfu-move-to-minibuffer

 ;; very useful
 ;; "M-g" #'corfu-info-location
 ;; "M-h" #'corfu-info-documentation

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

;;** UI

;;*** Bookmarks, Registers

(global-leader-def
  :keymaps '(global)
  :wk-full-keys nil
  "rB" '(:ignore t :wk "BURLY")
  "rBo" #'burly-open-bookmark
  "rBO" #'burly-open-url
  "rBw" #'burly-bookmark-windows
  "rBf" #'burly-bookmark-frames
  "rBB" #'burly-kill-buffer-url
  "rBF" #'burly-kill-frames-url
  "rBW" #'burly-kill-windows-url)

;;*** Window Management


;;*** Shell

;; TODO: map C-c M-! to :which-key and allow selecting shell scripts
;; that paste output to buffer
;;
;; edit so output is more friendly to insertion
;; guix search emacs-srv | recsel -p name,description

;;*** Lispy

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
  "n" '(:ignore t :wk "NOTES")
  "o" '(:ignore t :wk "OPEN")
  "p" '(:ignore t :wk "PROJECT")
  "q" '(:ignore t :wk "QUIT")
  "r" '(:ignore t :wk "REMOTE")
  "s" '(:ignore t :wk "SEARCH")
  "t" '(:ignore t :wk "TOGGLE")
  "v" '(:ignore t :wk "VCS")
  "v" '(:ignore t :wk "WORKSPACE")

  "&" '(:ignore t :wk "SNIPPET")
  "7" '(:ignore t :wk "SNIPPET")

  ;; "C-f" '(:ignore t :wk "FOLD") ;; imenu > folding
  )

;;*** &/7 SNIPPETS

(cl-dolist (pfx '("&" "7"))
  (leader-def
    :keymaps 'global
    :wk-full-keys nil
    ;; doesn't generate :wk
    ;; :prefix (concat "C-c " pfx)

    pfx '(:ignore t :wk "SNIPPETS")
    (concat pfx "n") #'yas-new-snippet
    (concat pfx "i") #'yas-insert-snippet
    (concat pfx "/") #'yas-visit-snippet-file
    (concat pfx "r") #'yas-reload-all))
    ;; "c" #'aya-create
    ;; "e" #'aya-expand

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
;;**** EGLOT

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
;;        (:when (modulep! :tools lsp +eglot)
;;         :desc "LSP Execute code action"              "a" #'eglot-code-actions
;;         :desc "LSP Rename"                           "r" #'eglot-rename
;;         :desc "LSP Find declaration"                 "j" #'eglot-find-declaration
;;         (:when (modulep! :completion vertico)
;;          :desc "Jump to symbol in current workspace" "j" #'consult-eglot-symbols)))

;;*** e EVAL

;;*** f FILE
(leader-def
  :keymaps 'global
  :wk-full-keys nil
  )

;;        (:when (modulep! :tools editorconfig)
;;         :desc "Open project editorconfig"  "c"   #'editorconfig-find-current-editorconfig)
;;        :desc "Copy this file"              "C"   #'doom/copy-this-file
;;        :desc "Find directory"              "d"   #'dired
;;        :desc "Delete this file"            "D"   #'doom/delete-this-file
;;        :desc "Find file in emacs.d"        "e"   #'doom/find-file-in-emacsd
;;        :desc "Browse emacs.d"              "E"   #'doom/browse-in-emacsd
;;        :desc "Find file"                   "f"   #'find-file
;;        :desc "Find file from here"         "F"   #'+default/find-file-under-here
;;        :desc "Locate file"                 "l"   #'locate
;;        :desc "Rename/move this file"       "m"   #'doom/move-this-file
;;        :desc "Find file in private config" "p"   #'doom/find-file-in-private-config
;;        :desc "Browse private config"       "P"   #'doom/open-private-config
;;        :desc "Recent files"                "r"   #'recentf-open-files
;;        :desc "Recent project files"        "R"   #'projectile-recentf
;;        :desc "Sudo this file"              "u"   #'doom/sudo-this-file
;;        :desc "Sudo find file"              "U"   #'doom/sudo-find-file
;;        :desc "Yank file path"              "y"   #'+default/yank-buffer-path
;;        :desc "Yank file path from project" "Y"   #'+default/yank-buffer-path-relative-to-project
;;        :desc "Open scratch buffer"         "x"   #'doom/open-scratch-buffer
;;        :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer)

;;*** i INSERT

(leader-def
  :keymaps 'global
  :wk-full-keys nil
  "i" '(:ignore t :wk "INSERT")
  "ie" #'emojify-insert-emoji
  ;; "if" #'+default/insert-file-path
  ;; "iF" (cmd!! #'+default/insert-file-path t)
  "is" #'yas-insert-snippet
  ;; "iy" #'+default/yank-pop
  "iu" #'insert-char)

;;*** l LOCAL
;;*** m M/CURSOR

;;       (:when (modulep! :editor multiple-cursors)
;;        (:prefix-map ("m" . "multiple-cursors")
;;         :desc "Edit lines"         "l"         #'mc/edit-lines
;;         :desc "Mark next"          "n"         #'mc/mark-next-like-this
;;         :desc "Unmark next"        "N"         #'mc/unmark-next-like-this
;;         :desc "Mark previous"      "p"         #'mc/mark-previous-like-this
;;         :desc "Unmark previous"    "P"         #'mc/unmark-previous-like-this
;;         :desc "Mark all"           "t"         #'mc/mark-all-like-this
;;         :desc "Mark all DWIM"      "m"         #'mc/mark-all-like-this-dwim
;;         :desc "Edit line endings"  "e"         #'mc/edit-ends-of-lines
;;         :desc "Edit line starts"   "a"         #'mc/edit-beginnings-of-lines
;;         :desc "Mark tag"           "s"         #'mc/mark-sgml-tag-pair
;;         :desc "Mark in defun"      "d"         #'mc/mark-all-like-this-in-defun
;;         :desc "Add cursor w/mouse" "<mouse-1>" #'mc/add-cursor-on-click))

;;*** n NOTES


;;       (:prefix-map ("n" . "notes")
;;        :desc "Search notes for symbol"        "." #'+default/search-notes-for-symbol-at-point
;;        :desc "Org agenda"                     "a" #'org-agenda

;;        (:when (modulep! :tools biblio)
;;         :desc "Bibliographic notes"        "b"
;;         (cond ((modulep! :completion vertico)   #'citar-open-notes)
;;               ((modulep! :completion ivy)       #'ivy-bibtex)
;;               ((modulep! :completion helm)      #'helm-bibtex)))

;;        :desc "Toggle last org-clock"          "c" #'+org/toggle-last-clock
;;        :desc "Cancel current org-clock"       "C" #'org-clock-cancel

;;        :desc "Org store link"                 "l" #'org-store-link
;;        :desc "Tags search"                    "m" #'org-tags-view
;;        :desc "Org capture"                    "n" #'org-capture
;;        :desc "Goto capture"                   "N" #'org-capture-goto-target
;;        :desc "Active org-clock"               "o" #'org-clock-goto
;;        :desc "Todo list"                      "t" #'org-todo-list

;;        :desc "Search notes"                   "s" #'+default/org-notes-search
;;        :desc "Search org agenda headlines"    "S" #'+default/org-notes-headlines

;;        :desc "View search"                    "v" #'org-search-view
;;        :desc "Org export to clipboard"        "y" #'+org/export-to-clipboard
;;        :desc "Org export to clipboard as RTF" "Y" #'+org/export-to-clipboard-as-rich-text

;;        (:when (modulep! :lang org +roam2)
;;         (:prefix ("r" . "roam")
;;          :desc "Open random node"           "a" #'org-roam-node-random
;;          :desc "Find node"                  "f" #'org-roam-node-find
;;          :desc "Find ref"                   "F" #'org-roam-ref-find
;;          :desc "Show graph"                 "g" #'org-roam-graph
;;          :desc "Insert node"                "i" #'org-roam-node-insert
;;          :desc "Capture to node"            "n" #'org-roam-capture
;;          :desc "Toggle roam buffer"         "r" #'org-roam-buffer-toggle
;;          :desc "Launch roam buffer"         "R" #'org-roam-buffer-display-dedicated
;;          :desc "Sync database"              "s" #'org-roam-db-sync
;;          :desc "Tag"                        "t" #'org-roam-tag-add
;;          :desc "Un-tag"                     "T" #'org-roam-tag-remove

;;          (:prefix ("d" . "by date")
;;           :desc "Goto previous note"        "b" #'org-roam-dailies-goto-previous-note
;;           :desc "Goto date"                 "d" #'org-roam-dailies-goto-date
;;           :desc "Capture date"              "D" #'org-roam-dailies-capture-date
;;           :desc "Goto next note"            "f" #'org-roam-dailies-goto-next-note
;;           :desc "Goto tomorrow"             "m" #'org-roam-dailies-goto-tomorrow
;;           :desc "Capture tomorrow"          "M" #'org-roam-dailies-capture-tomorrow
;;           :desc "Capture today"             "n" #'org-roam-dailies-capture-today
;;           :desc "Goto today"                "t" #'org-roam-dailies-goto-today
;;           :desc "Capture today"             "T" #'org-roam-dailies-capture-today
;;           :desc "Goto yesterday"            "y" #'org-roam-dailies-goto-yesterday
;;           :desc "Capture yesterday"         "Y" #'org-roam-dailies-capture-yesterday
;;           :desc "Find directory"            "-" #'org-roam-dailies-find-directory))))

;;*** o OPEN

;; (leader-def
;;   :keymaps '(global)
;;   :wk-full-keys

;;   "o"   '(:ignore t :which-key "org mode")

;;   "oi"  '(:ignore t :which-key "insert")
;;   "oil" '(org-insert-link :which-key "insert link")

;;   "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")

;;   "os"  '(dw/counsel-rg-org-files :which-key "search notes")

;;   "oa"  '(org-agenda :which-key "status")
;;   "ot"  '(org-todo-list :which-key "todos")
;;   "oc"  '(org-capture t :which-key "capture")
;;   "ox"  '(org-export-dispatch t :which-key "export"))

;;       "o" nil ; we need to unbind it first as Org claims this prefix
;;       (:prefix-map ("o" . "open")
;;        :desc "Browser"            "b"  #'browse-url-of-file
;;        :desc "Debugger"           "d"  #'+debugger/start
;;        :desc "New frame"          "f"  #'make-frame
;;        :desc "REPL"               "r"  #'+eval/open-repl-other-window
;;        :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
;;        :desc "Dired"              "-"  #'dired-jump

;;        (:when (modulep! :term vterm)
;;         :desc "Toggle vterm popup"            "t" #'+vterm/toggle
;;         :desc "Open vterm here"               "T" #'+vterm/here)
;;        (:when (modulep! :term eshell)
;;         :desc "Toggle eshell popup"           "e" #'+eshell/toggle
;;         :desc "Open eshell here"              "E" #'+eshell/here)

;;        (:when (modulep! :os macos)
;;         :desc "Reveal in Finder"           "o" #'+macos/reveal-in-finder
;;         :desc "Reveal project in Finder"   "O" #'+macos/reveal-project-in-finder
;;         :desc "Send to Transmit"           "u" #'+macos/send-to-transmit
;;         :desc "Send project to Transmit"   "U" #'+macos/send-project-to-transmit
;;         :desc "Send to Launchbar"          "l" #'+macos/send-to-launchbar
;;         :desc "Send project to Launchbar"  "L" #'+macos/send-project-to-launchbar
;;         :desc "Open in iTerm"              "i" #'+macos/open-in-iterm
;;         :desc "Open in new iTerm window"   "I" #'+macos/open-in-iterm-new-window)

;;        (:when (modulep! :tools docker)
;;         :desc "Docker" "D" #'docker)

;;        (:when (modulep! :email mu4e)
;;         :desc "mu4e" "m" #'=mu4e)

;;        (:when (modulep! :email notmuch)
;;         :desc "notmuch" "m" #'=notmuch)

;;*** p PROJECTILE

;; TODO: rebind C-x C-f to #'project-find-file, C-u C-x C-f to #'find-file

(general-define-key
 :keymaps 'global

 "C-M-p" #'project-find-file)

(general-define-key
 :keymaps 'project-prefix-map
 "k" #'dw/close-project-tab
 "F" #'consult-ripgrep)

;;       (:prefix ("p" . "project")
;;        :desc "Search project for symbol"   "." #'+default/search-project-for-symbol-at-point
;;        :desc "Find file in other project"  "F" #'doom/find-file-in-other-project
;;        :desc "Search project"              "s" #'+default/search-project
;;        :desc "List project todos"          "t" #'magit-todos-list
;;        :desc "Open project scratch buffer" "x" #'doom/open-project-scratch-buffer
;;        :desc "Switch to project scratch buffer" "X" #'doom/switch-to-project-scratch-buffer
;;        (:when (and (modulep! :tools taskrunner)
;;                    (or (modulep! :completion ivy)
;;                        (modulep! :completion helm)))
;;         :desc "List project tasks"         "z" #'+taskrunner/project-tasks)
;;        ;; later expanded by projectile
;;        (:prefix ("4" . "in other window"))
;;        (:prefix ("5" . "in other frame")))

;;*** q QUIT
;;*** r REMOTE

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

;;       (:prefix-map ("s" . "search")
;;        :desc "Search project for symbol"    "." #'+default/search-project-for-symbol-at-point
;;        :desc "Search buffer"                "b"
;;        (cond ((modulep! :completion vertico)   #'consult-line)
;;              ((modulep! :completion ivy)       #'swiper)
;;              ((modulep! :completion helm)      #'swiper))
;;        :desc "Search all open buffers"      "B"
;;        (cond ((modulep! :completion vertico)   (cmd!! #'consult-line-multi 'all-buffers))
;;              ((modulep! :completion ivy)       #'swiper-all)
;;              ((modulep! :completion helm)      #'swiper-all))
;;        :desc "Search current directory"     "d" #'+default/search-cwd
;;        :desc "Search other directory"       "D" #'+default/search-other-cwd
;;        :desc "Search .emacs.d"              "e" #'+default/search-emacsd
;;        :desc "Locate file"                  "f" #'+lookup/file
;;        :desc "Jump to symbol"               "i" #'imenu
;;        :desc "Jump to visible link"         "l" #'link-hint-open-link
;;        :desc "Jump to link"                 "L" #'ffap-menu
;;        :desc "Jump to bookmark"             "m" #'bookmark-jump
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

;;**** Toggle Modes
;; -: centered cursor
;; _: centered cursor
;; b: big-mode
;; c: fill-column indicator
;; D: desc
;; f: flycheck
;;

;;**** Toggle keybinds
(leader-def
  :keymaps 'global
  "tC" #'global-display-fill-column-indicator-mode
  "t1" #'flycheck-mode
  "t!" #'flymake-mode
  "t M-f" #'toggle-frame-fullscreen
  ;; "ti" #'highlight-indent-guides-mode
  ;; "tI" #'doom/toggle-indent-style"
  "tl" #'display-line-numbers-mode
  ;; "tp" #'org-tree-slide-mode
  ;; "tr" #'read-only-mode
  "ts" #'flyspell-mode
  "tv" #'visible-mode
  ;; "tw" #'visual-line-mode
  ;; "tw" #'+word-wrap-mode
  "tN" #'dc/toggle-native-comp-async-report-warnings-errors)

;;**** Toggles: org
(leader-def
  :keymaps '(org-mode-map)
  "tf" #'org-table-toggle-formula-debugger
  "to" #'org-table-toggle-coordinate-overlays)

;;**** Toggles: markdown
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

;;*** text-mode

;;**** org-mode

;;**** sgml-mode

;;**** html-mode

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
  "Ensure desired pairs of key combinations are translated on keymaps."
  (dc/translated-keymaps-set-keys
   (seq-filter (lambda (km) (not (fboundp km)))
               dc/keymaps-list)
   dc/translated-keypairs))

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
