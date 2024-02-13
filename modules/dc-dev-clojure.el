;; -*- lexical-binding: t; -*-
;;; dc-doom-popup.el --- Description
;;
;; Copyright © 2023 David Conner
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;;; Commentary:
;;
;; Hmmmm ... I assembled this like voltron from dotfiles across the web
;; neatly collected into https://github.com/ectorepo/x.files
;;
;;; Code:

;; abo-abo/csetq, shorter than phundrak/csetq
;; (defmacro csetq (variable value)
;;   `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

(require 'dash)
(require 'a)

(setq-default dc/major-mode-clojure
              '(("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode)
                ("\\.cljs\\'" . clojurescript-mode)
                ("\\.cljc\\'" . clojurec-mode)
                ("\\.\\(clj\\|cljd\\|dtm\\|edn\\)\\'" . clojure-mode)))

(defun dc/toggle-clojure-ts-remaps ()
  "Toggle clojurelike modes from handling being handled by tree-sitter."
  (interactive)
  (if (memq 'clojure-ts-mode (a-vals major-mode-remap-alist))
      (let ((major-mode-remaps-to-remove
             (map-filter
              (lambda (k v) (equal 'clojure-ts-mode v))
              major-mode-remap-alist)))
        (cl-dolist (m major-mode-remaps-to-remove)
          ;; it won't delete the last entry?
          (setq major-mode-remap-alist
                (delete m major-mode-remap-alist)))))
  (progn
    (add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode))
    (add-to-list 'major-mode-remap-alist '(clojurescript-mode . clojure-ts-mode))
    (add-to-list 'major-mode-remap-alist '(clojurec-mode . clojure-ts-mode))))

;;*** Clojure
(setup (:pkg clojure-mode)
  (:option clojure-toplevel-inside-comment-form t
           clojure-thread-all-but-last t

           ;; always-align, always-indent, align-arguments
           clojure-indent-style 'always-align
           clojure-align-forms-automatically t)

  ;; (:hook eglot-ensure)
  ;; (:hook format-other-mode)
  )

(setup (:pkg clojure-ts-mode
             :straight t
             :host github
             :repo "clojure-emacs/clojure-ts-mode"
             :build (:not autoloads)))

;; TODO jarchive-setup (karthink)

(setup (:pkg clj-refactor)
  (:option

   ;; ns
   cljr-favor-prefix-notation nil
   cljr-favor-private-functions nil

   ;; formatting
   cljr-insert-newline-after-require nil
   ;; cljr-assume-language-context "clj"

   ;; tests
   ;; cljr-clojure-test-declaration "[clojure.test :refer [deftest is testing]]"
   ;; cljr-expectations-test-declaration "[expectations :refer :all]"
   ;; tests (NOTE: requires option* ...)
   ;; cljr-cljs-clojure-test-declaration cljr-clojure-test-declaration
   ;; cljr-cljc-clojure-test-declaration cljr-clojure-test-declaration

   ;; eval
   ;; cljr-warn-on-eval nil
   ))

(setup (:pkg parseedn))
(setup (:pkg parseclj))

;; TODO: zprint-mode?
;; (add-hook 'clojure-mode-hook 'zprint-mode)
;; (add-hook 'clojurescript-mode-hook 'zprint-mode)

;; TODO kaocha-runner?

;;**** LSP (clojure)

;;**** CIDER
;; (add-hook 'cider-mode-hook #'clj-refactor-mode)
;; (setq-default org-babel-clojure-backend 'cider)

(setup (:pkg cider)
  (:option cider-use-overlays t
           cider-save-file-on-load nil  ; t(magnars)
           cider-prompt-for-symbol nil
           cider-offer-to-open-cljs-app-in-browser nil

           ;; appearance
           cider-font-lock-dynamically nil ;nilkoa ;abo-abo
           cider-font-lock-reader-conditionals nil
           cider-repl-use-pretty-printing t

           cider-print-options '(("length" 80)
                                 ("level" 20)
                                 ("right-margin" 80))

           ;; connection
           cider-jack-in-default 'clojure-cli ;abo-abo
           cider-repl-pop-to-buffer-on-connect nil
           cider-repl-display-in-current-window nil
           cider-eval-spinner-type 'half-circle
           ;; cider-mode-line-show-connection nil  ;abo-abo
           cider-repl-pop-to-buffer-on-connect nil

           ;; clojure-cli
           ;; cider-clojure-cli-aliases nil      ;abo-abo

           ;; history
           cider-merge-sessions 'project
           cider-history-file (expand-file-name "nrepl-history" dc/emacs-d)

           ;; debug/errors
           cider-repl-popup-stacktraces t
           cider-auto-select-error-buffer t

           ;; help
           ;; cider-repl-display-help-banner nil ;abo-abo

           ;; Automatically download all available .jars with Java sources and
           ;; javadocs, allowing you to navigate to Java sources and javadocs
           ;; in your Clojure projects. (magnars)
           cider-enrich-classpath t

           ;; nrepl
           ;; nrepl-hide-special-buffers t
           ;; nrepl-sync-request-timeout nil
           ;; nrepl-use-ssh-fallback-for-remote-hosts t

           ;; misc

           ;; cljs
           ;; shadow
           cider-default-cljs-repl 'shadow

           ;; figwheel
           ;; cider-default-cljs-repl 'figwheel-main
           cider-figwheel-main-default-options ":dev"
           ;; cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"
           )
  (:hook eldoc-mode)
  (require 'ob-clojure)
  (:option org-babel-clojure-backend 'cider))

(provide 'dc-dev-clojure)
