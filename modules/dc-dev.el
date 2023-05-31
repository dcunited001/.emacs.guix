;; -*- lexical-binding: t; -*-
;;
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

;;* Dev

;;*** Paren Matching

(setup (:pkg smartparens)
  (:hook-into prog-mode))

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
  (:option xref-show-definitions-function #'xref-show-definitions-completing-read
           xref-show-xrefs-function #'xref-show-definitions-buffer
           xref-file-name-display 'project-relative
           xref-search-program
           (cond
            ((executable-find "ugrep") 'ugrep)
            ((or (executable-find "ripgrep") (executable-find "rg")) 'ripgrep)
            (t 'grep))))

;;** Checking

;;*** Flycheck
;; This is a quick survey of flycheck and org-babel functionality
;; https://github.com/jkitchin/scimax/commit/9a039cfc5fcdf0114a72d23d34b78a8b3d4349c9
(setup (:pkg flycheck)
  (:option flycheck-emacs-lisp-load-path 'inherit
           flycheck-highlighting-mode 'columns

           ;; enable most on per-buffer/per-project basis only
           flycheck-global-modes '(emacs-lisp))
  (:also-load flycheck-guile)
  (:also-load flycheck-package))

;; see .emacs.doom/modules/checkers/javascript/config.el
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)
;; (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
;; (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode)
;; tide not completely compatible with LSP

(with-eval-after-load 'flycheck
  (setup (:pkg consult-flycheck :straight t))
  (global-flycheck-mode))

;;*** Flymake

;;** Compiling

;;*** M-x compile

(defun dc/project-local-root ()
  (and (project-current) (cdr (project-current))))

(defun dc/compilation-start-alert (proc)
  ;; file-name-???
  (let* ((project-dir (nth 1 (reverse (file-name-split
                                       (dc/project-local-root)))))
         (project-name (or project-dir "Emacs"))
         (alert-body (format "(%s) compilation-start: %s"
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

(defun dc/compilation-start-hook (proc)
  "Function called after starting a compilation. It gets passed PROC the
result of either `comint-exec' or
`start-file-process-shell-command' depending on whether the
compilation was initiated from compile-mode."

  ;; process-{name,command,environment}
  (dc/compilation-start-alert proc))


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

;;*** project.el

(defun dw/current-project-name ()
  (file-name-nondirectory
   (directory-file-name
    (project-root (project-current)))))

(defun dw/switch-project-action ()
  (interactive)
  (let* ((project-name (dw/current-project-name))
         (tab-bar-new-tab-choice #'magit-status)
         (tab-index (tab-bar--tab-index-by-name project-name)))
    (if tab-index
        (tab-bar-select-tab (1+ tab-index))
      (tab-bar-new-tab)
      (tab-bar-rename-tab project-name))))

(defun dw/close-project-tab ()
  (interactive)
  (let* ((project-name (dw/current-project-name))
         (tab-index (tab-bar--tab-index-by-name project-name)))
    (project-kill-buffers t)
    (when tab-index
      (tab-bar-close-tab (1+ tab-index)))))

(setup (:pkg project)
  (setq project-switch-commands #'magit-status))

;; TODO finish setting up projectile
(setq projectile-project-search-path '(("/data/repo/" . 1)
                                       ("/data/ecto/" . 3)))
;; projectile-auto-discover is nil
;; trigger project auto-discovery with projectile-discover-projects-in-search-path

;;** Treesitter
;; TODO setup major-mode-remap-alist
;; - https://www.reddit.com/r/emacs/comments/zqshfy/comment/j0zpwyo/?utm_source=reddit&utm_medium=web2x&context=3
(setup treesit)

;;** LSP/Eglot

;;*** Eglot

(setup (:pkg eglot)
  (:option eglot-autoshutdown t
           eglot-sync-connect 1
           eglot-connect-timeout 15
           eglot-send-changes-idle-time 0.5
           ;; other common options: xref, imenu, eldoc
           ;; also see (eglot--setq-saving...)
           eglot-stay-out-of '(flymake)
           eglot-extend-to-xref t       ;TODO: assess eglot-extend-to-xref

           ;; see note about popups/point in
           ;; .emacs.doom/modules/tools/lsp/+eglot.el
           ;; eglot-auto-display-help-buffer nil
           eglot-confirm-server-initiated-edits nil)

  (require 'eglot)
  (define-key eglot-mode-map (kbd "C-c C-a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c C-r") #'eglot-rename)

  ;; TODO: Is this needed now?
  (add-to-list
   'eglot-server-programs
   '((js2-mode typescript-mode) .
     ("typescript-language-server" "--stdio")))
  (add-to-list
   'eglot-server-programs
   '(python-mode . ("pylsp")))
  ;; TODO: c-mode-hook is hooked in c-mode-hook?
  ;; (:with-hook c-mode-hook
  ;;   (:hook eglot-ensure))
  )

(with-eval-after-load 'eglot
  ;; "emacs-consult-eglot" ;; 0.2.0 on guix does not include fix to #14
  (setup (:pkg consult-eglot :straight t :type git :flavor melpa
               :host github :repo "mohkale/consult-eglot")))

;;** VCS

;;*** Magit

(setup (:pkg magit)
  (:global "C-M-;" magit-status)
  (:option magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setup (:pkg magit-todos)
  (:with-hook emacs-startup-hook
    (:hook magit-todos-mode)))

;; interface to git-tbdiff, gives better control over git ranges
(setup (:pkg magit-tbdiff :straight t))

;; TODO: interactive: magit-tbdiff-ranges
;; TODO: interactive: magit-tbdiff-revs
;; TODO: interactive: magit-tbdiff-with-base
;; TODO: interactive: magit-tbdiff-save

;;*** Git

(setup (:pkg git-link)
  (:option git-link-open-in-browser t))

;;*** Git Timemachine
;; control-f8, like facebook's conference
(setup (:pkg git-timemachine))

;; TODO: DOOM: defadvice! +vc-support-git-timemachine-a (fn)
;; TODO: DOOM: defadvice! +vc-update-header-line-a (revision)
;; TODO: DOOM: keybindings
  ;; (map! :map git-timemachine-mode-map
  ;;       :n "C-p" #'git-timemachine-show-previous-revision
  ;;       :n "C-n" #'git-timemachine-show-next-revision
  ;;       :n "gb"  #'git-timemachine-blame
  ;;       :n "gtc" #'git-timemachine-show-commit)
;;*** Ghub

(defun dc/ensure-ghub-graphql ()
  "Emacs really did not want to load this code"
  (require 'ghub-graphql)
  (require 'glab)
  (require 'gtea))

(setup (:pkg ghub)
  (:with-hook emacs-startup-hook
    (:hook #'dc/ensure-ghub-graphql)))

;;*** Forge
(setup (:pkg forge)
  (:option forge-pull-notifications t))

(setup (:pkg repology))
;; TODO: repology interactives/customs:
;; https://github.com/emacs-straight/repology/blob/master/repology.el

;;*** Sr.ht
(setup (:pkg srht)
  (:option srht-username user-mail-address))

;; (srht :type git :host github :repo "emacs-straight/srht" :files ("*" (:exclude ".git")))

;;*** Repo
;; For Google Repo
(setup (:pkg repo))
;; TODO: repo interactives/customs: repo-status, repo-init...

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

(with-eval-after-load 'apheleia
  ;; setup formatters
  (setq apheleia-formatters (a-merge apheleia-formatters
                                     dc/apheleia-formatters))

  ;; setup formatters per mode
  (add-to-list 'apheleia-mode-alist '(python-mode . yapf))
  ;; (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent))
  (add-to-list 'apheleia-mode-alist '(lisp-data-mode . lisp-indent))
  (cl-dolist (aclang-mode dc/apheleia-clang-modes)
    (add-to-list 'apheleia-mode-alist `(,(car aclang-mode) . clang-format))))

(setup (:pkg apheleia)
  (apheleia-global-mode +1))

;;*** Indentation

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

;;*** formatter-check

(defun dc/formatter-check (cmd &optional remote sym)
  "Check for the presence of formatter command using exectable-find,
preferring the value of sym if present"
  (let ((cmd (or (bound-and-true-p sym) cmd)))
    (executable-find cmd remote)))

(defmacro dc/formatter-check-for (formatter-cmd &optional formatter-sym &rest body)
  `(lambda ()
     (let ((remote? (file-remote-p default-directory)))
       (if-let* ((formatter-loc (dc/formatter-check ,formatter-cmd
                                                    remote?
                                                    ,formatter-sym)))
           ;; (progn
           ;;   (warn "%s found" formatter-loc)
           ;;   ,@body)
           ,@body
         (warn "Could not find %s (remote: %s)" ,formatter-cmd remote?)))))

;;*** xml-format

;; xmllint is typically included with libxml2
(defvar xml-format-xmllint-executable)
(setq dc/formatter-check-xml (dc/formatter-check-for "xmllint"
                                                     'xml-format-xmllint-executable
                                                     (xml-format-on-save-mode +1)))
(setup (:pkg xml-format :straight t :type git :flavor melpa
             :host github :repo "wbolster/emacs-xml-format")
  (:with-hook nxml-mode-hook
    (:hook (dc/formatter-check-for "xmllint"
                                   'xml-format-xmllint-executable
                                   (xml-format-on-save-mode +1)))))

;;** Lisps

(setup (:pkg lispy :straight t :type git :flavor melpa
             :host github :repo "abo-abo/lispy"
             :files (:defaults "lispy-clojure.clj"
                               "lispy-clojure.cljs"
                               "lispy-python.py"
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

(setup (:pkg sly)
  (:disabled)
  (:file-match "\\.lisp\\'"))

;;*** Scheme

;; Include .sld library definition files
(setup (:pkg scheme-mode)
  (:file-match "\\.sld\\'"))

;;**** GEISER

(setup (:pkg geiser)
  ;; (setq geiser-default-implementation 'gambit)
  ;; (setq geiser-active-implementations '(gambit guile))
  ;; (setq geiser-implementations-alist '(((regexp "\\.scm$") gambit)
  ;;                                      ((regexp "\\.sld") gambit)))
  ;; (setq geiser-repl-default-port 44555) ; For Gambit Scheme
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-repl-default-port 44555) ; For Gambit Scheme
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile)))

  ;; TODO determine whether autodoc still crashes REPL's
  ;; (setq geiser-repl-autodoc-p nil)
  )

;;**** Mesche

(setup mesche
  (:load-path "~/Projects/Code/mesche/mesche-emacs")
  (:with-mode mesche-mode
    (:file-match "\\.msc\\'"))
  (require 'mesche))

;;** Lang

;;*** Haskell

;;** Shell

;;*** VTerm
(setup (:pkg vterm)
  (:option vterm-max-scrollback 1000))



;;** Snippets

;;*** Snippets
(setup (:pkg emmet)
  (:hook-into sgml-mode css-mode nxml-mode html-mode))

;; yas-snippet-dirs:
;;
;; (doom-snippets-dir
;; "~/.emacs.g/etc/yasnippet/snippets/")

;;*** Yasnippet Snippets

(setup (:pkg yasnippet)
  (:with-hook org-mode-hook
    (:hook yas-minor-mode))
  (:with-hook prog-mode-hook
    (:hook yas-minor-mode)))

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

;; (with-eval-after-load 'yasnippet
;;   (add-hook 'org-mode-hook #'yas-minor-mode)
;;   (add-hook 'prog-mode-hook #'yas-minor-mode))

;; (setup (:pkg doom-snippets :straight t :type git :host github
;;              :repo "dcunited001/snippets" :files ("*.el" "*"))
;;   (:option doom-snippets-enable-short-helpers t))



(provide 'dc-dev)
