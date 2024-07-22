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

(use-package smartparens :straight t :demand t
  :hook
  ((prog-mode text-mode) . smartparens-mode)
  :config
  ;; this would load smartparens for all the langs
  ;; essentially (require 'smartparens-lang)
  ;; (require 'smartparens-config)
  (require 'smartparens-python)
  (require 'smartparens-org)
  (require 'smartparens-latex)
  (require 'smartparens-markdown))

(use-package smartparens :straight t
  :after yasnippet
  (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay))

(use-package rainbow-delimiters :straight t :demand t
  :hook
  ((prog-mode geiser-repl-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode :straight t :demand t
  :hook
  ((org-mode  emacs-lisp-mode  web-mode  typescript-mode js2-mode) . rainbow-mode))

;;*** Direnv

(use-package envrc
  ;; TODO: delight?
  :custom
  (envrc-on-lighter '("│Æ=" (:propertize "on" face envrc-mode-line-on-face)))
  (envrc-none-lighter '("│Æ=" (:propertize "none" face envrc-mode-line-none-face)))
  (envrc-error-lighter '("│Æ=" (:propertize "err" face envrc-mode-line-error-face)))
  :hook (emacs-startup-hook . envrc-global-mode))

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

;;** Compiling

;;*** Comint

(use-package comint :straight (:type built-in)
  :custom
  (comint-prompt-read-only t))

;;**** Sentinel for comint buffers

;; from wasamasa

(defun dc/shell-kill-buffer-sentinel (process event)
  (when (and (memq (process-status process) '(exit signal))
             (buffer-live-p (process-buffer process)))
    (kill-buffer)))

(defun dc/kill-process-buffer-on-exit ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'dc/shell-kill-buffer-sentinel))

(use-package ielm :straight (:type built-in) :demand t
  :hook
  ((ielm-mode-hook term-exec-hook comint-exec-hook) . dc/kill-process-buffer-on-exit))

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

;;*** Compile


;; example of customizing comint
;;
;; https://github.com/hvesalai/emacs-sbt-mode/blob/master/sbt-mode-comint.el

;; compilation-shell-minor-mode works with shell-mode (or comint-mode derivatives)
;;
;; https://www.masteringemacs.org/article/compiling-running-scripts-emacs

(use-package compile :straight (:type built-in)
  :custom
  (compilation-scroll-output t)
  (compilation-start-hook #'dc/compilation-start-hook)
  (compilation-environment '("TERM=xterm-256color"))
  :config
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))


;;**** Compile Multi
(use-package compile-multi :straight t :after compile)

;; TODO: configure (:option consult-compile-multi-narrow ...)
(use-package consult-compile-multi :straight t :after compile-multi)

;; TODO: compile-mode-hook

;; TODO: use (process-contact ...) with (alert ... ) to log specific commands?
;;
;; e.g. ssh/tramp
;;
;; (process-contact process &optional key no-block)

(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))

(defun dw/auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

;;** Projects

;;** Treesitter
;; TODO setup major-mode-remap-alist
;; - https://www.reddit.com/r/emacs/comments/zqshfy/comment/j0zpwyo/?utm_source=reddit&utm_medium=web2x&context=3
(use-package treesit :straight (:type built-in)
  :demand t
  ;; something is automatically setting up major-mode-remap-alist
  :custom
  (treesit-extra-load-path
   (list (expand-file-name ".local/lib/tree-sitter" (getenv "HOME"))))
  (treesit-language-source-alist
   '((yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
     (astro . ("https://github.com/virchau13/tree-sitter-astro"))))
  :config

  (cl-loop for lang-key
           in (a-keys treesit-language-source-alist)
           unless (treesit-language-available-p lang-key)
           do (treesit-install-language-grammar lang-key)))

;; (qml . ("https://github.com/yuja/tree-sitter-qmljs"))

;; tree sitter extra grammars get installed to
;; .emacs.g/tree-sitter/libtree-sitter-qml.so

;; not used in config
(defun dc/treesit-bump-extra-load-path ()
  "Prepend the `lib/treesit' from the symlinked guix profile to
`treesit-extra-load-path'. This avoids the need to restart emacs
when a new treesitter gramar has been added to the Guix profile."
  (let* ((emacs-g-profile (dc/guix-profile-get-default-path))
         (fresh-treesit (expand-file-name "lib/tree-sitter" emacs-g-profile)))
    (add-to-list 'treesit-extra-load-path fresh-treesit)))

;;*** Combobulate

;; NOTE: need to understand how the macro (with-navigation-nodes ...) operates
;; on -manipulation-edit-procedures (see -edit-cluster-dwim)
;;
;; the edit procedures are defined per-language (html, json, js, yaml, etc), but
;; how do i know whether I'm on one of these special node types?

;; requires tempo,treesit

;; https://github.com/mickeynp/combobulate

(use-package combobulate :straight t :defer t
  :hook
  ((html-ts-mode css-mode) . combobulate-mode))

;; TODO: setup combobulate for python-ts and yaml-ts

;; TODO: decide on yaml-ts by default (setup major-mode-remap-alist)

;; combobulate binds transient ui to C-c o o

;;** LSP/Eglot

;;*** Eglot

;; these are unnecessary, since dc/eglot-setup-buffer adds buffer-local hooks
(defun dc/eglot-organize-imports ()
  (interactive)
  (if (eglot-managed-p)
	    (eglot-code-actions nil nil "source.organizeImports" t)))

(defun dc/eglot-format-buffer ()
  (interactive)
  (if (and (eglot-managed-p)
           (not (a-get apheleia-mode-alist major-mode)))
      (eglot-format-buffer)))

;; eglot, apheleia and ws-butler don't always get along
(defun dc/eglot-setup-buffer ()
  ;; to locally update once hooked, eglot-managed-mode will toggle these

  ;; generally: if depth <= 0, add-hook prepends and otherwise, it appends
  ;; it defaults to zero.

  ;; if it's managed
  (if (eglot-managed-p)
      ;; set depth (10) to organize imports before formatting
      (add-hook 'before-save-hook #'dc/eglot-organize-imports 10 t)
    (remove-hook 'before-save-hook #'dc/eglot-organize-imports t))

  ;; if it's managed and unless apheleia formats
  (if (and (eglot-managed-p)
           (not (a-get apheleia-mode-alist major-mode)))
      (add-hook 'before-save-hook #'eglot-format-buffer 11 t)
    (remove-hook 'before-save-hook #'eglot-format-buffer t))

  (if ws-butler-mode
      (if (eglot-managed-p)
          (remove-hook 'before-save-hook #'ws-butler-before-save t)
        (add-hook 'before-save-hook #'ws-butler-before-save nil 5 t))))

;; TODO: hook on eglot-managed-mode (via karthink & plt). gets eglot to work
;; well with eldoc
;;
;; (setq-default eldoc-documentation-strategy
;;       'eldoc-documentation-compose-eagerly)

(defun dc/toggle-eglot-events-buffer-size (&optional arg)
  "Toggle `:size' and `:format' for `eglot-events-buffer'."
  (interactive "p")
  ;; TODO: what to do with arg?

  ;; local? default?
  (let ((eglot-defaults (alist-get 'default dc/eglot-events-buffer-configs))
        (eglot-debug (alist-get 'debug dc/eglot-events-buffer-configs)))
    (setq-local eglot-events-buffer-config
                (if (eq eglot-events-buffer-config eglot-defaults)
                    eglot-debug
                  eglot-defaults))))

(use-package consult-eglot :straight t :after eglot :demand t)
(use-package consult-eglot-embark :straight t :after eglot :demand t)

;;**** eglot automation

;; a dir-local class may be more appropriate

(defun dc/emit-dir-local-hook (hook fn)
  "Configure a hook stored in dir-locals."
  ;; TODO test to see if add-dir-local-variable does this
  (message "Need to read in and re-emit the dir-locals to add evals"))

;;**** eglot configs via lsp-mode

(defvar dc/lsp-url-pattern "https://raw.githubusercontent.com/emacs-lsp/lsp-mode/master/clients/lsp-%s.el")

;; dc/read-lisp-data-into-list moved to 'dc-support

(require 'url)
(defun dc/download-lsp-into-temp (mode)
  (let* ((url (format dc/lsp-url-pattern mode))
         (tmp (make-temp-file "emacs-lsp-download"))
         (tmpfile (url-copy-file url tmp t))
         (lsp-mode-list (dc/read-lisp-into-list tmp)))
    ;; lsp-mode-list
    (delete-file tmp)
    (cl-loop for el in (cadr lsp-mode-list)
             until (eq (car el) 'lsp-register-custom-settings)
             finally return el)))

;; use with C-u C-x C-e, then transform with regexp
(defun dc/lsp-mode-config-emit (mode)
  (let ((lsp-cfg (dc/download-lsp-into-temp mode)))
    (cadadr lsp-cfg)))

;; (dc/lsp-mode-config-emit "html")
;; (("html.trace.server" lsp-html-trace-server)
;;  ("html.autoClosingTags" lsp-html-auto-closing-tags t)
;;  ("html.validate.styles" lsp-html-validate-styles t)
;;  ("html.validate.scripts" lsp-html-validate-scripts t)
;;  ("html.suggest.html5" lsp-html-suggest-html5 t)
;;  ("html.format.wrapAttributes" lsp-html-format-wrap-attributes)
;;  ("html.format.extraLiners" lsp-html-format-extra-liners)
;;  ("html.format.endWithNewline" lsp-html-format-end-with-newline t)
;;  ("html.format.indentHandlebars" lsp-html-format-indent-handlebars t)
;;  ("html.format.maxPreserveNewLines" lsp-html-format-max-preserve-new-lines)
;;  ("html.format.preserveNewLines" lsp-html-format-preserve-new-lines t)
;;  ("html.format.indentInnerHtml" lsp-html-format-indent-inner-html t)
;;  ("html.format.contentUnformatted" lsp-html-format-content-unformatted)
;;  ("html.format.unformatted" lsp-html-format-unformatted)
;;  ("html.format.wrapLineLength" lsp-html-format-wrap-line-length)
;;  ("html.format.enable" lsp-html-format-enable t)
;;  ("html.experimental.customData" lsp-html-experimental-custom-data))

;; lsp-client-settings is a local var in lsp-mode.el, set per mode.
;; it affects the (lsp-configuration-section "html") behavior
;; (lsp-client-settings downloaded-lsp-config)
;; setting (lsp-use-plists t) affects lsp-mmode internals

;; this returns JSON with unevaled lsp-mode symbols as strings
;; (json-serialize (lsp-configuration-section "html"))

;; this chokes on unknown symbols
;; (json-encode-plist (lsp-configuration-section "html"))

;; this works, oddly enough, but requires recursively calling yaml-fn on all branches
;; (yaml--hash-table-to-plist (lsp-configuration-section "html"))

;; i'm not sure why cadadr works (it may be a object like a closure)

;; (let ((downloaded-lsp-config (cadadr tmp-lisp))
;;       (lsp-client-settings downloaded-lsp-config))
;;   (lsp-configuration-section "html"))


;;** Formatting

;;*** Apheleia



;; html tidy's extra warnings doesn't jive with apheleia ... but i reallllly
;; don't want to install node _where it should be unnecessary_ (i gave up)
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
                             ".c")))

        ;; NOTE: tidy just seems to error out with apheleia
        (html-tidy "tidy" "--quiet" "yes"
                   "--tidy-mark" "no"
                   "--vertical-space" "no" ;; prevents wrapping inside empty
                   "--show-body-only" "auto"
                   "--doctype" "omit"
                   "-indent"

                   (when (derived-mode-p 'nxml-mode) "-xml")
                   (apheleia-formatters-indent "--indent-with-tabs" "--indent-spaces"
                                               (cond
                                                ((derived-mode-p 'nxml-mode)
                                                 'nxml-child-indent)
                                                ((derived-mode-p 'web-mode)
                                                 'web-mode-indent-style)))
                   (apheleia-formatters-fill-column "-wrap"))
        (eglot . #'eglot-format-buffer)))

;; (a-get apheleia-formatters 'html-tidy)
;; (a-get apheleia-mode-alist 'html-mode)
;; (a-get apheleia-mode-alist 'json-mode)

;; (t :type git :flavor melpa :host github :repo "radian-software/apheleia"
;;    :files (:defaults ("scripts" "scripts/formatters") "apheleia-pkg.el"))

(use-package apheleia :straight t :demand t
  :delight "│¶" ;; apheleia-mode-lighter

  :config
  (setq apheleia-formatters
        (a-merge apheleia-formatters dc/apheleia-formatters))

  ;; clang formatters
  (add-to-list 'apheleia-mode-alist '(lisp-data-mode . lisp-indent))

  (cl-dolist (aclang-mode dc/apheleia-clang-modes)
    (add-to-list 'apheleia-mode-alist `(,(car aclang-mode) . clang-format)))

  ;; web formatters
  (dolist (webml '(html-mode html-ts-mode mhtml-mode web-mode))
    ;; aphelia: prettier instead of prettier-html, so it relies on .prettierrc
    ;; (setq apheleia-mode-alist (delete `(,webml . prettier-html) apheleia-mode-alist))
    (add-to-list 'apheleia-mode-alist `(,webml . prettier)))
  
  ;; (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent))
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
(use-package highlight-indent-guides :straight t :defer t
  :custom (highlight-indent-guides-method 'column)
  :hook
  (yaml-mode . highlight-indent-guides-mode)
  (ef-themes-post-load-hook . dc/fix-highlight-indent-colors))

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
(setq-default dc/formatter-check-xml
              (dc/when-exec-found "xmllint"
                                  'xml-format-xmllint-executable
                                  (xml-format-on-save-mode +1)))

(use-package xml-format :straight t :defer t
  :hook
  (nxml-mode-hook . dc/formatter-check-xml))

;;** Lisps

(use-package lispy :straight (:type git :flavor melpa
                                    :host github :repo "abo-abo/lispy"
                                    :files (:defaults "lispy-clojure.clj"
                                                      "lispy-clojure.cljs"
                                                      "le-scheme.el"
                                                      "lispy-pkg.el"))
  :demand t
  :custom
  (lispy-compat '(cider edebug))

  :config
  (advice-add 'lispy-goto-symbol-elisp
              :override #'xref-find-definitions '(name "dc/nanon"))

  :after smartparens

  :hook
  ;; (lispy-mode . #'turn-off-smartparens-mode)
  ((emacs-lisp-mode
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
    clojure-mode) . lispy-mode)

  ;; TODO if overriding doesn't work, catch the error and switch
  ;; (defun dc/lispy-catch-goto-symbol ())
  )

;;*** Emacs Lisp

;; thanks for the advice djgoku
(use-package elisp-demos :straight t :demand t
  :config
  (advice-add 'describe-function-1
              :after #'elisp-demos-advice-describe-function-1))

(setq dash-fontify-mode-lighter "DASH")

;; TODO: toggle-debug-on-quit; (edebug-goto-here)

;; TODO: setup casual-edebug

;; - set trace mode (step?, trace, go?)
;; - see also eros (overlays)

;; NOTE: doesn't work (separate debugging system, hoped to get lucky)
;; (add-to-list 'gud-tooltip-modes 'emacs-lisp-mode)

;; exhibits the same problem as helpful-mode:
;; - https://github.com/doomemacs/doomemacs/issues/6127
;;    - workaround removed https://github.com/doomemacs/doomemacs/commit/b57d4c8d710e39c70c2fc9fa61cf0e85159ef0bb
;; - read-symbol-positions-list void in emacs29
;;   - https://github.com/Wilfred/elisp-refs/issues/35

;;**** Elisp Depmap

;; this works, but the graphs have the "longitis." it's not counting edges
;; properly. The graph doesn't have edges, so it then doesn't balance well.

;; if you use -Kfdp, it kinda balances them, but still no edges.

;; its perhaps still useful to manually add them to the graphviz lol

;; `M-x elisp-depmap-makesummarytable` is still very useful. while the info is
;; mostly already available from emacs help (and perhaps embark), this gives an
;; org-table that's easy to chunk up if more analysis is needed.

;; so, for example, if i wanted to quickly analyze the codebases of arel.el for
;; Guile to integrate it with lispy (add a le-arel.el), then it helps to to have
;; a central list of things to find connections for in arel, lispy, sesman,
;; cider and geiser.

;; this defvar is definitely still required for elisp-depmap
(defvar read-symbol-positions-list nil)

;; (defun dc/project-elisp-depmap-graphviz-digraph ()
;;   (interactive))


;; NOTE: seems to filter defvar read-symbol-positions-list from results
(defun dc/project-elisp-depmap-graphviz-digraph (&optional fnshapekeys)
  (interactive)
  ;; TODO: check or handle non-file buffers
  (let* ((elisp-depmap-exec-file
          (expand-file-name
           (format "_%s.dot"
                   (file-name-base (buffer-file-name (current-buffer))))
           (project-root (project-current t))))
         ;; (elisp-subcluster-groups
         ;;  (or function-shapes elisp-depmap-graph-subclustergroups))
         (elisp-depmap-parse-function-shapes
          (or (and fnshapekeys
                   (listp fnshapekeys)
                   (map-filter (lambda (k) (memq k fnshapekeys))
                               elisp-depmap-parse-function-shapes))
              elisp-depmap-parse-function-shapes)))

    (elisp-depmap-graphviz-digraph)))

;; (let ((fnshapekeys '(defun defcustom)))
;;   (map-filter (lambda (k) (memq k fnshapekeys))
;;               elisp-depmap-parse-function-shapes))

;; (:bind
;;  ("C-c M-d" . elisp-depmap-graphviz-digraph)
;;  ("C-c M-g" . elisp-depmap-graphviz)
;;  ("C-c M-s" . elisp-depmap-makesummarytable))

(use-package elisp-depmap :straight t :defer t

  ;; (getenv "GRAPHVIZ_DOT") ;; -exec-file is a path to the created .dot file
  :custom
  (elisp-depmap-exec-file
   ;; (expand-file-name  "depmap/elisp-graphviz.dot" dc/emacs-d)

   "Not intended to be an absolute path.")
  (elisp-depmap-exec-outext "svg")
  ;; "-Kfdp"
  (elisp-depmap-exec-commandargs
   nil "tried: fdp, sfdp, neato, dot.")

  (elisp-depmap-graph-linemod 5)

  ;; (elisp-depmap-graph-subcluster-groups
  ;;  (:variables (setq setq-local defvar defvar-local defcustom))
  ;;  (:functions (defun defsubst defmacro)))

  ;; reduce complexity of graph for now
  (elisp-depmap-parse-function-shapes
   (;; (setq . underline)
    ;; (setq-local . underline)
    ;; (defvar . underline)
    ;; (defvar-local . underline)
    (defcustom . plain)
    (defun . tab)
    ;; (defsubst . component)
    (defmacro . trapezium)
    (defgeneric . trapezium)
    (defmethod . trapezium))))

;;*** Common Lisp


(use-package lisp-mode :straight (:type built-in)
  :mode ((rx "." (| "lisp" "asd") eos) . lisp-mode))


(use-package sly :straight t :defer t
  :custom
  (inferior-lisp-program "sbcl"))

;;*** Scheme

;; Include .sld library definition files
(use-package scheme-mode :straight (:type built-in)
  :mode ((rx "." (| "sld") eos) . scheme-mode))

;;**** ARES

;; sesman-browser-link-with-buffer: guix.el needed to make this simple

(use-package sesman :straight t :defer t)
(use-package arei :straight (:host sourcehut :repo "abcdw/emacs-arei")
  :defer t :after sesman
  (arei-mode-auto t))

;; When `arei-mode-auto' is non-nil, arei-mode enabled in scheme buffers when
;; required. The executables from `guile-ares-rs' need to be in path. It could
;; be added to projects. Until I have more time, I'm worried about potential
;; conflicts if Geiser/Arei access the same Emacs features. Geiser has been a
;; huge readblack. :(

;; For simplicity, avoid Lispy's evaluation functionality, since that expects
;; Geiser. Most of Lispy's features should work, but some may init Geiser. Lispy
;; is unlikely for now, since each implementation is REPL-specific and usually
;; around 3,000 LOC... The CIDER implementation should be a good reference, but
;; it would still be challenging (not something I'm likely to do well...)

;; doesn't disable loading the hooks because, `arei-mode-auto' retains its
;; compiled-in value from the final `with-eval-after-load' default. Hopefully
;; I can start using it soon

;;**** GEISER

(use-package geiser :straight t :defer t
  :custom
  (geiser-default-implementation 'guile)
  (geiser-implementations-alist '(((regexp "\\.scm$") guile)))

  (geiser-active-implementations
   '(guile racket) "lispy-eval on scheme req. emacs-guile-racket loaded")

  ;; these need to be correct
  (geiser-repl-per-project-p t)
  (geiser-repl-add-project-paths t)

  (geiser-debug-always-display-sexp-after t)
  ;; (geiser-debug-long-sexp-lines 6)

  (geiser-debug-treat-ansi-colors
   'colors "Requires guile-colorized (ice-9 colorized)")

  (geiser-repl-highlight-output-p t))

;;***** Guile prompt regex

;; These don't exactly support unicode chars
;; (how to keep track of multiple project repls)

;; geiser-guile--prompt-regexp
;; "^[^@(
;; ]+@([^)]*)>"

;; geiser-guile--debugger-prompt-regexp "^[^@(
;; ]+@([^)]*?) \\[\\([0-9]+\\)\\]> "

;;***** Guile init files

(use-package geiser-guile :straight t :defer t :after geiser
  :custom
  (geiser-guile-manual-lookup-other-window t) ;; default: nil
  ;; geiser-guile-extra-keywords nil
  (geiser-guile-show-debug-help t)
  (geiser-guile-warning-level 'medium)

  ;; The Paths are added to both %`load-path' and %load-compiled path,
  ;; and only if they are not already present. (in .dir-locals.el)
  ;; geiser-guile-load-path

  ;; NOTE: it loads geiser-guile, even _without_ the emacs-geiser-guile package
  ;; i had compat. issues with this about 6 months ago (just in case)

  :config
  ;; TODO: (add-to-list 'geiser-guile-manual-lookup-nodes '(...))
  ;; default: '("Guile" "guile-2.0")
  (add-to-list 'geiser-guile-manual-lookup-nodes "Geiser")
  (add-to-list 'geiser-guile-manual-lookup-nodes "Guile Reference")
  (add-to-list 'geiser-guile-manual-lookup-nodes "Guile Library")
  (add-to-list 'geiser-guile-manual-lookup-nodes "Guix")
  (add-to-list 'dc/org-babel-load-languages '(scheme . t)))

;; geiser will load ~/.guile-geiser and not ~/.guile (defaults)
;; (setq-default geiser-guile-init-file "~/.guile-geiser")
;; (setq-default geiser-guile-load-init-file nil)

;; NOTE autodoc does not seem to be crashing REPLs anymore
;; (setq-default geiser-repl-autodoc-p nil)

;;*****  Racket
;; evaluating scheme with lispy req. emacs-guile-racket loaded
;; (setup (:pkg geiser-racket))

;; racket is too large and req. compilation. instead use this hack
(defun geiser-racket--language () 'racket)

;;*****  Gambit Scheme

;; (setq-default geiser-default-implementation 'gambit)
;; (setq-default geiser-active-implementations '(gambit guile)))
;; (setq-default geiser-repl-default-port 44555) ; For Gambit Scheme
;; (setq-default geiser-implementations-alist '(((regexp "\\.scm$") gambit)
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

;; there is also flycheck-shellcheck, but the same functionality is built into emacs
;; (require 'flymake-shellcheck)
(use-package flymake-shellcheck :straight (:type built-in)
  :commands flymake-shellcheck-load
  ;; the command loads the diagnostics locally the script's buffer.
  ;; load it like this bc maybe you don't want it on all the time.
  ;; you still need to hook flymake
  :config
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;;*** VTerm

;; TODO: per-project vterm
;; https://github.com/doomemacs/doomemacs/blob/master/modules/term/vterm/autoload.el

(use-package vterm :straight t :defer t
  :custom
  (vterm-max-scrollback 1000)
  (vterm-set-bold-hightbright t))

;;** Snippets

;;*** Yasnippet

;;*** Emmet

(defvar dc/kill-ring '() "A list of interesting lines from this session")

;; not actually what i was going for (ends up in kill-ring, but not in expr)
(defun dc/kill-ring-push (arg)
  (interactive "P")
  (let* ((here (point))
         (beg (when (use-region-p) (region-beginning)))
         (end (when (use-region-p) (region-end)))
         (expr (copy-region-as-kill beg end)))
    ;; (pp `(,beg ,end ,expr))
    (add-to-list 'dc/emmet-expansions expr)))

(defvar dc/emmet-expansions '() "A list of emmet expansions from this session")
(defun dc/emmet-expansion-push (arg)
  (interactive "P")
  (add-to-list 'dc/emmet-expansions (emmet-expr-on-line)))

;; previous body (something gets nil i donno)
;; here is maybe necessary bc emmet-expr-on-line is a macro
;; (let* ((here (point))
;;        (beg (when (use-region-p) (region-beginning)))
;;        (end (when (use-region-p) (region-end)))
;;        (expr (copy-region-as-kill beg end)))
;;   (add-to-list 'dc/emmet-expansions (emmet-expr-on-line))
;; (goto-char here)
;;)

(use-package emmet-mode :straight t :defer t
  :hook
  ((sgml-mode
    css-mode
    nxml-mode
    html-ts-mode
    web-mode
    html-mode
    mhtml-mode) . emmet-mode))

;; yas-snippet-dirs:
;;
;; (doom-snippets-dir
;; "~/.emacs.g/etc/yasnippet/snippets/")

;; TODO yas creates new snippets in yas--default-user-snippets-dir

(provide 'dc-dev)
