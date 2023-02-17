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

;;** Dev

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

;;*** Buffer Environments

(setup (:pkg buffer-env)
  (:option buffer-env-script-name "manifest.scm")
  (add-hook 'comint-mode-hook #'hack-dir-local-variables-non-file-buffer)
  (add-hook 'hack-local-variables-hook #'buffer-env-update))

;;** Checking

;;*** Flycheck
;; enable on per-buffer basis only
(setq flycheck-highlighting-mode 'columns
      flycheck-global-modes nil)
;; flycheck-global-modes: the modes in which to enable flycheck

;;**** Flycheck RC's
;; ... or just M-x customize-group flycheck... SMH y u no ask greybeard?
;; ... or just C-h describe-variables "^flycheck-.*rc$" and C-; embark-dwim
;; ... goddamn i needed to explore these packages earlier.
;; C-u M-: dc/find-symbols-like "^flycheck-.*rc$"
;; then, comment and add space at end of each line
;;   with M-% '" "' -> ' \n;; ' (newline with C-q C-j)
;; then, C-u C-x C-e to eval and insert string

;; flycheck-chktexrc ".chktexrc"
;; flycheck-coffeelintrc ".coffeelint.json"
;; flycheck-ember-template-lintrc ".template-lintrc.js"
;; flycheck-flake8rc ".flake8rc"
;; flycheck-hlintrc "HLint.hs"
;; flycheck-jshintrc ".jshintrc"
;; flycheck-luacheckrc ".luacheckrc"
;; flycheck-perlcriticrc ".perlcriticrc"
;; flycheck-puppet-lint-rc ".puppet-lint.rc"
;; flycheck-pylintrc ".pylintrc"
;; flycheck-reekrc nil
;; flycheck-rubocoprc ".rubocop.yml"
;; flycheck-ruby-standardrc ".standard.yml"
;; flycheck-rubylintrc nil
;; flycheck-ruumbarc ".ruumba.yml"
;; flycheck-sass-lintrc ".sass-lint.yml"
;; flycheck-scalastylerc nil
;; flycheck-scss-lintrc ".scss-lint.yml"
;; flycheck-stylelintrc nil
;; flycheck-tidyrc ".tidyrc"
;; flycheck-yamllintrc ".yamllint"

;;**** flycheck-emacs-lisp
;; (setq flycheck-emacs-lisp-check-declare nil
;;       flycheck-emacs-lisp-initialize-packages 'auto
;;       flycheck-emacs-lisp-load-path nil
;;       flycheck-emacs-lisp-package-user-dir nil)

;; flycheck-emacs-lisp-load-path nil
;; flycheck-emacs-lisp-executable nil
;; flycheck-emacs-lisp-check-declare nil
;; flycheck-emacs-lisp-initialize-packages auto
;; flycheck-emacs-lisp-package-user-dir nil
;; flycheck-emacs-lisp-checkdoc-executable nil

;; contain lists
;; flycheck-emacs-lisp-checkdoc-variables
;; flycheck-emacs-lisp-check-form
;; flycheck-emacs-lisp-checkdoc-form
;; flycheck-emacs-lisp-package-initialize-form

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
  (:global "C-M-p" project-find-file)
  (:with-map project-prefix-map
    (:bind "k" dw/close-project-tab)
    (:bind "F" consult-ripgrep))

  (setq project-switch-commands #'magit-status))

;;** LSP/Eglot

;;*** Eglot

(setup (:pkg eglot)
  ;; TODO: Don't load until needed
  (require 'eglot)
  (define-key eglot-mode-map (kbd "C-c C-a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c C-r") #'eglot-rename)
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil)
  ;; TODO: Is this needed now?
  (add-to-list 'eglot-server-programs
               '((js2-mode typescript-mode) . ("typescript-language-server" "--stdio"))))

(with-eval-after-load 'eglot
  (add-hook 'c-mode-hook 'eglot-ensure))

;;** VCS

;;*** Magit

(setup (:pkg magit)
  (:also-load magit-todos)
  (:global "C-M-;" magit-status)
  (:option magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (setup (:pkg magit-todos))

;;*** Git

(setup (:pkg git-link)
  (:option git-link-open-in-browser t))

;;** Formatting

(setup (:pkg apheleia)
  (apheleia-global-mode +1))

;;** Lisps

(setup (:pkg lispy)
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
              clojure-mode))

;;*** Emacs Lisp

(setup emacs-lisp-mode)

;;*** Common Lisp

(setup (:pkg sly)
  (:disabled)
  (:file-match "\\.lisp\\'"))

;;*** Scheme

;; Include .sld library definition files
(setup (:pkg scheme-mode)
  (:file-match "\\.sld\\'"))

(setup (:pkg geiser)
  ;; (setq geiser-default-implementation 'gambit)
  ;; (setq geiser-active-implementations '(gambit guile))
  ;; (setq geiser-implementations-alist '(((regexp "\\.scm$") gambit)
  ;;                                      ((regexp "\\.sld") gambit)))
  ;; (setq geiser-repl-default-port 44555) ; For Gambit Scheme
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-repl-default-port 44555) ; For Gambit Scheme
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile))))

;;*** Mesche

(setup mesche
  (:load-path "~/Projects/Code/mesche/mesche-emacs")
  (:with-mode mesche-mode
    (:file-match "\\.msc\\'"))
  (require 'mesche))

;;** Snippets

;;*** Snippets
(setup (:pkg emmet)
  (:hook-into sgml-mode css-mode))

(setup (:pkg yasnippet)
  (:load-after emmet)
  (require 'yasnippet)
  (require 'doom-snippets)
  ;; doom-snippets-dir
  ;; doom-snippets-enable-short-helpers t
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (yas-reload-all))

(provide 'dc-dev)
