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
  (:hook-into prog-mode))

(setup (:pkg rainbow-mode)
  (:hook-into org-mode
              emacs-lisp-mode
              web-mode
              typescript-mode
              js2-mode))

;;*** Environments

(setup (:pkg buffer-env)
  (:option buffer-env-script-name "manifest.scm")
  (add-hook 'comint-mode-hook #'hack-dir-local-variables-non-file-buffer)
  (add-hook 'hack-local-variables-hook #'buffer-env-update))

(defun dc/enable-lispy-in-dir-locals ()
  (if (string-match
       "\.dir-locals\.el$"
       (file-relative-name (or (buffer-file-name) "")))
      (lispy-mode +1)))

(add-hook 'lisp-data-mode-hook #'dc/enable-lispy-in-dir-locals)

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

(setup compile
  (:option compilation-scroll-output t))

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
;; TODO setup treesit-extra-load-path
;; TODO setup major-mode-remap-alist
;; - https://www.reddit.com/r/emacs/comments/zqshfy/comment/j0zpwyo/?utm_source=reddit&utm_medium=web2x&context=3
(defvar major-mode-remap-alist '())
(setup treesit
  (:option treesit-extra-load-path
           (list (file-name-as-directory (expand-file-name "tree-sitter" dc/emacs-d)))))

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

           ;; see note about popups/point in
           ;; .emacs.doom/modules/tools/lsp/+eglot.el
           ;; eglot-auto-display-help-buffer nil
           eglot-confirm-server-initiated-edits nil)

  (require 'eglot)
  (define-key eglot-mode-map (kbd "C-c C-a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c C-r") #'eglot-rename)

  ;; TODO: Is this needed now?
  (add-to-list 'eglot-server-programs
               '((js2-mode typescript-mode) . ("typescript-language-server" "--stdio")))
  ;; TODO: c-mode-hook is hooked in c-mode-hook?
  (:with-hook c-mode-hook
    (:hook eglot-ensure)))

(with-eval-after-load 'eglot
  (setup (:pkg consult-eglot)))

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

;;*** Forge
(setup (:pkg forge))

(setup (:pkg repology))
;; TODO: repology interactives/customs:
;; https://github.com/emacs-straight/repology/blob/master/repology.el

;;*** Repo
;; For Google Repo
(setup (:pkg repo))
;; TODO: repo interactives/customs: repo-status, repo-init...

;;** Formatting

(setup (:pkg apheleia)
  (apheleia-global-mode +1))

;;** Lisps

(setup (:pkg lispy)
  (:option lispy-compat '(cider edebug))
  (:hook #'turn-off-smartparens-mode)
  (:hook-into emacs-lisp-mode
              scheme-mode
              ielm-mode
              scheme-mode
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


;;** Snippets

;;*** Snippets
(setup (:pkg emmet)
  (:hook-into sgml-mode css-mode nxml-mode html-mode))

(setup (:pkg yasnippet)
  (require 'yasnippet)
  (require 'doom-snippets)
  (:with-hook org-mode-hook prog-mode-hook
              (:hook yas-minor-mode))
  ;; doom-snippets-dir
  ;; doom-snippets-enable-short-helpers t

  ;; (:load-after (yas-reload-all))
  (yas-reload-all))

;;** Shell

;;*** VTerm
(setup (:pkg vterm)
  (:option vterm-max-scrollback 1000))

(provide 'dc-dev)
