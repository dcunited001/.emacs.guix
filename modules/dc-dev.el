;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2021 David Wilson
;; Copyright © 2014-2022 Henrik Lissner
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

;;* Dev

;;*** Paren Matching

;; seems to screw with geiser and wayland pgtk
;; (require 'hideshow)
;; (add-hook 'prog-mode-hook #'hs-minor-mode)

;; (setup (:pkg hideshow)
;;   (:hook-into prog-mode))

;;*** Paren Matching

(setup (:pkg smartparens)
  (:hook-into prog-mode text-mode))

(with-eval-after-load 'smartparens
  ;; this would load smartparens for all the langs
  ;; essentially (require 'smartparens-lang)
  ;; (require 'smartparens-config)
  (require 'smartparens-python)
  (require 'smartparens-org)
  (require 'smartparens-latex)
  (require 'smartparens-markdown))

(with-eval-after-load 'yasnippet
  (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay))

(setup (:pkg rainbow-delimiters)
  (:hook-into prog-mode geiser-repl-mode))

(setup (:pkg rainbow-mode)
  (:hook-into org-mode
              emacs-lisp-mode
              web-mode
              typescript-mode
              js2-mode))

;;*** Direnv

(setup (:pkg envrc)
  (:option envrc-on-lighter '("│Æ=" (:propertize "on" face envrc-mode-line-on-face))
           envrc-none-lighter '("│Æ=" (:propertize "none" face envrc-mode-line-none-face))
           envrc-error-lighter '("│Æ=" (:propertize "err" face envrc-mode-line-error-face)))
  (add-to-list 'minions-prominent-modes 'envrc-mode)
  (add-hook 'emacs-startup-hook #'envrc-global-mode))

;; (setup (:pkg buffer-env)

;;   (:option buffer-env-script-name "manifest.scm")
;;   (add-hook 'comint-mode-hook #'hack-dir-local-variables-non-file-buffer)
;;   (add-hook 'hack-local-variables-hook #'buffer-env-update))

;; (defun dc/enable-lispy-in-dir-locals ()
;;   (if (string-match
;;        "\.dir-locals\.el$"
;;        (file-relative-name (or (buffer-file-name) "")))
;;       (lispy-mode +1)))

;; (add-hook 'lisp-data-mode-hook #'dc/enable-lispy-in-dir-locals)

;;** Docs

(setup xref
  (:option xref-file-name-display 'project-relative
           ;; conflicts with consult-xref config
           ;; xref-show-definitions-function #'xref-show-definitions-completing-read
           ;; xref-show-xrefs-function #'xref-show-definitions-buffer

           xref-search-program
           (cond
            ((executable-find "ugrep") 'ugrep)
            ((or (executable-find "ripgrep") (executable-find "rg")) 'ripgrep)
            (t 'grep))))

;;** Compiling

;;*** Comint

(setq comint-prompt-read-only t)

;;**** Sentinel for comint buffers

;; from wasamasa

(defun dc/shell-kill-buffer-sentinel (process event)
  (when (and (memq (process-status process) '(exit signal))
             (buffer-live-p (process-buffer process)))
    (kill-buffer)))

(defun dc/kill-process-buffer-on-exit ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'dc/shell-kill-buffer-sentinel))

(dolist (hook '(ielm-mode-hook term-exec-hook comint-exec-hook))
  (add-hook hook 'dc/kill-process-buffer-on-exit))

;;*** M-x compile

(defun dc/project-local-root (&optional may-prompt)
  ;; fails in a lot of cases
  ;; (and (project-current) (cdr (project-current)))
  (consult--default-project-function may-prompt))

;; seemed to be needed to fix consult's project lookup when projects provided
;; a vc-mode root
;;
;; (defun dc/consult-project-function (may-prompt)
;;   (let ((proj (consult--default-project-function may-prompt)))
;;     (cond
;;      ((stringp proj) proj)
;;      ((and (listp proj) (stringp (nth 1 proj)))
;;       (nth 1 proj))
;;      (t ))))

(defun dc/compilation-start-alert (proc)
  ;; file-name-???
  (let* ((project-dir (nth 1 (reverse (file-name-split
                                       (dc/project-local-root t)))))
         (project-name (or project-dir "Emacs"))
         ;; capital %S to properly escape from how (find-grep ...) reports event
         (alert-body (format "(%s) compilation-start: %S"
                             project-name
                             (string-join (process-command proc) " ")))
         (alert-title (process-name proc)))
    (alert alert-body :title alert-title)))

;; Emacs 39: Processes
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Processes.html
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Process-Information.html
;;
;; (comint-exec outbuf (downcase mode-name) shell-file-name nil
;;              `(,shell-command-switch ,command))
;; (start-file-process-shell-command (downcase mode-name) outbuf command)

;; process-{name,command,environment}

(defun dc/compilation-start-hook (proc)
  "Function called after starting a compilation. It gets passed PROC the
result of either `comint-exec' or
`start-file-process-shell-command' depending on whether the
compilation was initiated from compile-mode."

  ;; don't actually alert on find-grep events ... though that'd definitely break
  ;; on a compilation if the escaping isn't escaped properly so it can be
  ;; re-re-re-regexped
  (when (not (derived-mode-p 'grep-mode))
    (dc/compilation-start-alert proc)))

(setup compile
  (:option compilation-scroll-output t
           compilation-start-hook #'dc/compilation-start-hook))

;; TODO: compile-mode-hook

;; TODO: use (process-contact ...) with (alert ... ) to log specific commands?
;;
;; e.g. ssh/tramp
;;
;; (process-contact process &optional key no-block)

(setq compilation-environment '("TERM=xterm-256color"))

(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

(defun dw/auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

;;** Projects

;;** Treesitter
;; TODO setup major-mode-remap-alist
;; - https://www.reddit.com/r/emacs/comments/zqshfy/comment/j0zpwyo/?utm_source=reddit&utm_medium=web2x&context=3
(setup treesit
  ;; something is automatically setting up major-mode-remap-alist
  (:option treesit-language-source-alist
           '((yaml . ("https://github.com/ikatyang/tree-sitter-yaml")))))

;; TODO: where does this install? any way to reduce startup time after updates?
;; ... and by itself this isn't very useful. also, doom only provides treesitter
;;      navigation functionality to evil users
(cl-loop for lang-key
         in (a-keys treesit-language-source-alist)
         unless (treesit-language-available-p lang-key)
         do (treesit-install-language-grammar lang-key))

;;*** Combobulate

;; requires tempo,treesit

;; https://github.com/mickeynp/combobulate
(setup (:pkg combobulate :straight t :type git
             :host github :repo "mickeynp/combobulate"
             :files (:defaults)))

;; TODO: setup combobulate for python-ts and yaml-ts

;; TODO: decide on yaml-ts by default (setup major-mode-remap-alist)

;; combobulate binds transient ui to C-c o o

;;** LSP/Eglot

;;*** Eglot

(defun dc/eglot-organize-imports () (interactive)
	     (eglot-code-actions nil nil "source.organizeImports" t))

;; once hooked, eglot-managed-mode will toggle these
(defun dc/eglot-setup-buffer ()
  ;; generally: if depth <= 0, add-hook prepends and otherwise, it appends
  ;; it defaults to zero.
  (if (eglot-managed-p)
      (progn
        (add-hook 'before-save-hook #'eglot-format-buffer nil t)
        (add-hook 'before-save-hook #'dc/eglot-organize-imports nil t))
    (remove-hook 'before-save-hook #'dc/eglot-organize-imports t)
    (remove-hook 'before-save-hook #'eglot-format-buffer t)))

;; TODO: hook on eglot-managed-mode (via karthink & plt). gets eglot to work
;; well with eldoc
;;
;; (setq eldoc-documentation-strategy
;;       'eldoc-documentation-compose-eagerly)

(setup (:pkg eglot)
  (:option eglot-autoshutdown t
           eglot-sync-connect 1
           eglot-connect-timeout 15
           eglot-send-changes-idle-time 0.5
           ;; other common options: xref, imenu, eldoc
           ;; also see (eglot--setq-saving...)
           eglot-stay-out-of '(flymake)
           eglot-extend-to-xref t       ;TODO: assess eglot-extend-to-xref

           eglot-menu-string "Æ"

           ;; see note about popups/point in
           ;; .emacs.doom/modules/tools/lsp/+eglot.el
           ;; eglot-auto-display-help-buffer nil
           eglot-confirm-server-initiated-edits nil)

  ;; TODO: maybe hook this (aphelia may need to be toggled off)
  ;; (:with-hook eglot-managed-mode-hook
  ;;   (:hook #'dc/eglot-setup-buffer))

  (require 'eglot)

  ;; TODO: Is this needed now?
  (add-to-list
   'eglot-server-programs
   '((js2-mode typescript-mode) .
     ("typescript-language-server" "--stdio")))

  ;; TODO: c-mode-hook is hooked in c-mode-hook?
  ;; (:with-hook c-mode-hook
  ;;   (:hook eglot-ensure))

  ;; TODO: maybe configure eglot defaults
  ;; (setq-default eglot-workspace-configuration '(:lsp-server-key (:config ...)))

  ;; about to configure this for yaml, but it needs yaml schema-specific glob
  ;; patterns anyways
  )

(with-eval-after-load 'eglot
  ;; "emacs-consult-eglot" ;; 0.2.0 on guix does not include fix to #14
  (setup (:pkg consult-eglot :straight t :type git :flavor melpa
               :host github :repo "mohkale/consult-eglot")))

;;** Formatting

;;*** Aphelia

;; html tidy's usage of exit-status-codes doesn't jive with apheleia ... but i
;; reallllly don't want to install node _where it should be unnecessary_
;;
;; (html-tidy . ("tidy" "-q" "--tidy-mark" "no" "-indent"))
;; (xml-tidy . ("tidy" "-q" "--tidy-mark" "no" "-indent" "-xml"))
;; (add-to-list 'apheleia-mode-alist '(html-mode . html-tidy))
;; (add-to-list 'apheleia-mode-alist '(nxml-mode . xml-tidy))

;; incompatible interface
;;
;; (json-mode-beautify . json-mode-beautify)
;; (add-to-list 'apheleia-mode-alist '(json-mode . json-mode-beautify))
;; (add-to-list 'apheleia-mode-alist '(json-ts-mode . json-mode-beautify))

;; clang-format: .c,.cs,.cpp,.cu,.proto,.glsl,.java,.js,.ts,.m
(setq dc/apheleia-clang-modes
      '((c-mode          . ".c")
        (cc-mode         . ".c")
        (c-ts-mode       . ".c")
        (c++-mode        . ".cpp")
        (c++-ts-mode     . ".cpp")
        (cuda-mode       . ".cu")
        (glsl-mode       . ".cu")
        (protobuf-mode   . ".proto")
        (java-mode       . ".java")
        (java-ts-mode    . ".java")
        (js-mode         . ".js")
        ;; (js2-mode        . ".js")
        (js3-mode         . ".js")
        (js-ts-mode      . ".js")
        (typescript-mode . ".ts")))

(setq dc/apheleia-formatters
      '((yapf . ("yapf"))
        (clang-format . ("clang-format"
                         "-assume-filename"
                         (or (buffer-file-name)
                             (cdr (assoc major-mode
                                         dc/apheleia-clang-modes))
                             ".c")))))

;; (a-get apheleia-formatters 'html-tidy)
;; (a-get apheleia-mode-alist 'html-mode)
;; (a-get apheleia-mode-alist 'json-mode)

(setup (:pkg apheleia)
  (:option apheleia-mode-lighter "│¶"))

(with-eval-after-load 'apheleia
  ;; setup formatters
  (setq apheleia-formatters (a-merge apheleia-formatters
                                     dc/apheleia-formatters))

  ;; setup formatters per mode
  ;; (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent))
  (add-to-list 'apheleia-mode-alist '(lisp-data-mode . lisp-indent))
  (cl-dolist (aclang-mode dc/apheleia-clang-modes)
    (add-to-list 'apheleia-mode-alist `(,(car aclang-mode) . clang-format)))

  (add-to-list 'minions-prominent-modes 'apheleia-mode)
  (apheleia-global-mode +1))

;;*** Indentation

(defun dc/fix-highlight-indent-colors ()
  "Ensures colors for highlight-indent-guides-mode are compatible
 with dark ef-themes."
  (set-face-background
   'highlight-indent-guides-odd-face
   (car (alist-get 'bg-added-refine
                   (ef-themes--palette-value
                    (ef-themes--current-theme)))))
  (set-face-background
   'highlight-indent-guides-even-face
   (car (alist-get 'bg-added-faint
                   (ef-themes--palette-value
                    (ef-themes--current-theme))))))

;; TODO: on init, this runs before ef-themes has defined faces
;; Invalid face: highlight-indent-guides-odd-face
(setup (:pkg highlight-indent-guides)
  (:option highlight-indent-guides-method 'column)
  (:hook-into yaml-mode))

(with-eval-after-load 'highlight-indent-guides
  (add-hook 'ef-themes-post-load-hook #'dc/fix-highlight-indent-colors))

(defun dc/indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;; TODO: make it work starting from the beginning
;; TODO: make mode-specific
(defun dc/flush-blank-lines ()
  (interactive)
  (let ((to-replace "^
\\{2,\\}")
        (replace-with "
"))
    (while (re-search-forward to-replace nil t)
      (replace-match replace-with nil nil))
    ))

;;*** format-other-mode

;; NOTE: unused, need to rewrite to move point to top & back. apheleia is better

(define-minor-mode format-other-mode
  "Mode for formatting source buffers not covered by reformatter.el"
  :lighter nil
  (cond
   (format-other-mode
    (add-hook 'before-save-hook 'dc/flush-blank-lines nil t)
    (add-hook 'before-save-hook 'whitespace-cleanup nil t))

   (t
    (remove-hook 'before-save-hook 'dc/flush-blank-lines t)
    (remove-hook 'before-save-hook 'whitespace-cleanup t))))

;; TODO maybe load from eld file, check/warn on init
;; - but probably just set executables in .dir-locals.el for project
;; (defvar dc/formatters (thread-first dc/eld-path
;;                                     (expand-file-name "formatters.eld")
;;                                     (dc/eld-unserialize)))

;;*** xml-format

;; xmllint is typically included with libxml2
(defvar xml-format-xmllint-executable)

;; TODO: remove? wtf?
(setq dc/formatter-check-xml (dc/when-exec-found "xmllint"
                                                 'xml-format-xmllint-executable
                                                 (xml-format-on-save-mode +1)))
(setup (:pkg xml-format :straight t :type git :flavor melpa
             :host github :repo "wbolster/emacs-xml-format")
  (:with-hook nxml-mode-hook
    (:hook (dc/when-exec-found "xmllint"
                               'xml-format-xmllint-executable
                               (xml-format-on-save-mode +1)))))

;;** Lisps

(setup (:pkg lispy :straight t :type git :flavor melpa
             :host github :repo "abo-abo/lispy"
             :files (:defaults "lispy-clojure.clj"
                               "lispy-clojure.cljs"
                               "le-scheme.el"
                               "lispy-pkg.el"))

  (:option lispy-compat '(cider edebug))
  (:hook #'turn-off-smartparens-mode)
  (:hook-into emacs-lisp-mode
              lisp-data-mode
              scheme-mode
              ielm-mode
              scheme-mode
              geiser-repl-mode
              ;; racket-mode
              ;; hy-mode
              ;; lfe-mode
              ;; dune-mode
              ;; fennel-mode
              clojure-mode)

  ;; TODO if overriding doesn't work, catch the error and switch
  ;; (defun dc/lispy-catch-goto-symbol ())

  (advice-add 'lispy-goto-symbol-elisp :override #'xref-find-definitions '(name "dc/nanon")))

;;*** Emacs Lisp

(setup emacs-lisp-mode)

(setq dash-fontify-mode-lighter "DASH")

(add-to-list 'minions-prominent-modes 'edebug-mode)

;; NOTE: doesn't work (separate debugging system, hoped to get lucky)
;; (add-to-list 'gud-tooltip-modes 'emacs-lisp-mode)

;; exhibits the same problem as helpful-mode:
;; - https://github.com/doomemacs/doomemacs/issues/6127
;;    - workaround removed https://github.com/doomemacs/doomemacs/commit/b57d4c8d710e39c70c2fc9fa61cf0e85159ef0bb
;; - read-symbol-positions-list void in emacs29
;;   - https://github.com/Wilfred/elisp-refs/issues/35
;; (setup (:pkg elisp-depmap :straight t :host gitlab :repo "mtekman/elisp-depmap.el")
;;   (:option elisp-depmap-exec-file (getenv "GRAPHVIZ_DOT"))
;;   (:bind
;;    ("C-c M-d" . elisp-depmap-graphviz-digraph)
;;    ("C-c M-g" . elisp-depmap-graphviz)
;;    ("C-c M-s" . elisp-depmap-makesummarytable))
;;   (defvar read-symbol-positions-list nil))

;;*** Common Lisp

(setup lisp-mode
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.asd\\'" . lisp-mode)))

(setup (:pkg sly))

;;*** Scheme

;; Include .sld library definition files
(setup (:pkg scheme-mode)
  (:file-match "\\.sld\\'"))

;;**** GEISER

(setup (:pkg geiser)
  (:option geiser-default-implementation 'guile
           ;; evaluating scheme with lispy req. emacs-guile-racket loaded
           geiser-active-implementations '(guile racket)
           geiser-implementations-alist '(((regexp "\\.scm$") guile))

           ;; these need to be correct
           geiser-repl-per-project-p t
           geiser-repl-add-project-paths t

           geiser-debug-always-display-sexp-after t
           ;; geiser-debug-long-sexp-lines 6

           ;; requires guile-colorized (ice-9 colorized)
           geiser-debug-treat-ansi-colors 'colors

           geiser-repl-highlight-output-p t))

;;***** Guile prompt regex

;; These don't exactly support unicode chars
;; (how to keep track of multiple project repls)

;; geiser-guile--prompt-regexp
;; "^[^@(
;; ]+@([^)]*)>"

;; geiser-guile--debugger-prompt-regexp "^[^@(
;; ]+@([^)]*?) \\[\\([0-9]+\\)\\]> "

;;***** Guile init files

(setup (:pkg geiser-guile)
  (:option geiser-guile-manual-lookup-other-window t ;; default: nil
           ;; geiser-guile-extra-keywords nil
           geiser-guile-show-debug-help t
           geiser-guile-warning-level 'medium))

;; The Paths are added to both %`load-path' and %load-compiled path,
;; and only if they are not already present. (in .dir-locals.el)
;; geiser-guile-load-path

;; NOTE: it loads geiser-guile, even _without_ the emacs-geiser-guile package
;; i had compat. issues with this about 6 months ago (just in case)
(with-eval-after-load 'geiser-guile
  ;; TODO: (add-to-list 'geiser-guile-manual-lookup-nodes '(...))
  ;; default: '("Guile" "guile-2.0")
  (add-to-list 'geiser-guile-manual-lookup-nodes "Geiser")
  (add-to-list 'geiser-guile-manual-lookup-nodes "Guile Reference")
  (add-to-list 'geiser-guile-manual-lookup-nodes "Guile Library")
  (add-to-list 'geiser-guile-manual-lookup-nodes "Guix"))

;; geiser will load ~/.guile-geiser and not ~/.guile (defaults)
;; (setq geiser-guile-init-file "~/.guile-geiser")
;; (setq geiser-guile-load-init-file nil)

;; NOTE autodoc does not seem to be crashing REPLs anymore
;; (setq geiser-repl-autodoc-p nil)

;;*****  Racket
;; evaluating scheme with lispy req. emacs-guile-racket loaded
;; (setup (:pkg geiser-racket))

;; racket is too large and req. compilation. instead use this hack
(defun geiser-racket--language () 'racket)

;;*****  Gambit Scheme

;; (setq geiser-default-implementation 'gambit)
;; (setq geiser-active-implementations '(gambit guile)))
;; (setq geiser-repl-default-port 44555) ; For Gambit Scheme
;; (setq geiser-implementations-alist '(((regexp "\\.scm$") gambit)
;;                                      ((regexp "\\.sld") gambit))

;;**** Mesche

;; (setup mesche
;;   (:load-path "~/Projects/Code/mesche/mesche-emacs")
;;   (:with-mode mesche-mode
;;     (:file-match "\\.msc\\'"))
;;   (require 'mesche))

;;** Lang

;;*** Haskell

;;** Shell

;;*** VTerm

;; TODO: per-project vterm
;; https://github.com/doomemacs/doomemacs/blob/master/modules/term/vterm/autoload.el

(setup (:pkg vterm)
  (:option vterm-max-scrollback 1000
           vterm-set-bold-hightbright t))

;;** Snippets

;;*** Snippets
(setup (:pkg emmet)
  (:hook-into sgml-mode css-mode nxml-mode html-mode))

;; yas-snippet-dirs:
;;
;; (doom-snippets-dir
;; "~/.emacs.g/etc/yasnippet/snippets/")

;; TODO yas creates new snippets in yas--default-user-snippets-dir

;;*** Yasnippet Snippets

;; NOTE: all yas/ functions are deprecated

(defun dc/yasnippet-set-default ()
  (let ((yas-def (org-file-contents (dc/emacs-etc "yasnippet/yasdefault"))))
    (setq yas-new-snippet-default yas-def)))

(setup (:pkg yasnippet)
  (:with-hook org-mode-hook
    (:hook yas-minor-mode))
  (:with-hook LaTeX-mode-hook
    (:hook yas-minor-mode))
  (:with-hook yaml-mode-hook
    (:hook yas-minor-mode))
  (:with-hook prog-mode-hook
    (:hook yas-minor-mode))
  (:with-hook window-setup-hook
    (:hook #'dc/yasnippet-set-default)
    (:hook #'yas-reload-all)))

(with-eval-after-load 'yasnippet
  (when dc/guix-checkout-path
    (add-to-list 'yas-snippet-dirs dc/guix-checkout-path nil)))

(setup (:pkg yasnippet-snippets))

;;*** Doom Snippets

;; (with-eval-after-load 'doom-snippets
;;   (setup (:pkg yasnippet)
;;     ;; This should work with multiple hooks, but doesn't seem to add them
;;     ;; also doesn't seem to add prog-mode-hooks either
;;     ;; (:with-hook org-mode-hook
;;     ;;   (:hook yas-minor-mode))
;;     ;; (:with-hook prog-mode-hook
;;     ;;   (:hook yas-minor-mode))
;;     (require 'yasnippet)
;;     (doom-snippets-initialize)
;;     (yas-reload-all)))

;; (setup (:pkg doom-snippets :straight t :type git :host github
;;              :repo "dcunited001/snippets" :files ("*.el" "*"))
;;   (:option doom-snippets-enable-short-helpers t))

(provide 'dc-dev)
