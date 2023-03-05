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

;;*** Clojure
(setup (:pkg clojure-mode)
  (:option clojure-toplevel-inside-comment-form t

           )

  ;; (:hook eglot-ensure)
  )

;; TODO jarchive-setup (karthink)

(setup (:pkg clj-refactor))
(setup (:pkg parseedn))
(setup (:pkg parseclj))

;; TODO: zprint-mode?
;; (add-hook 'clojure-mode-hook 'zprint-mode)
;; (add-hook 'clojurescript-mode-hook 'zprint-mode)

;; TODO kaocha-runner?

;;**** LSP (clojure)

;;**** CIDER
;; (add-hook 'cider-mode-hook #'clj-refactor-mode)
;; (setq org-babel-clojure-backend 'cider)

(setup (:pkg cider)
  (:option cider-use-overlays t
           cider-save-file-on-load nil
           cider-prompt-for-symbol nil
           cider-font-lock-dynamically nil
           cider-font-lock-reader-conditionals nil
           cider-repl-pop-to-buffer-on-connect nil
           cider-repl-display-in-current-window nil
           cider-eval-spinner-type 'half-circle
           cider-merge-sessions 'project

           ;; Automatically download all available .jars with Java sources and
           ;; javadocs, allowing you to navigate to Java sources and javadocs
           ;; in your Clojure projects.
           cider-enrich-classpath t

           ;; nrepl
           ;; nrepl-hide-special-buffers t
           nrepl-sync-request-timeout nil
           ;; nrepl-use-ssh-fallback-for-remote-hosts t

           ;; cider-mode-line-show-connection nil  ;abo-abo
           ;; cider-font-lock-dynamically nilkoa ;abo-abo
           cider-jack-in-default 'clojure-cli ;abo-abo
           ;; cider-clojure-cli-aliases nil      ;abo-abo
           ;; cider-repl-display-help-banner nil ;abo-abo

           ;; figwheel
           ;; cider-default-cljs-repl 'figwheel-main
           cider-figwheel-main-default-options ":dev"

           ;; shadow
           cider-default-cljs-repl 'shadow

           )
  (:hook eldoc-mode)
  (require 'ob-clojure)
  (:option org-babel-clojure-backend 'cider))

(provide 'dc-dev-clojure)
