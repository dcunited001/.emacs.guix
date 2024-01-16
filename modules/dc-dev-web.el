;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2021 David Wilson
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

;;* Web

;; To get HTML, CSS, JSON, and eslint LSP's install this
;; npm install -g vscode-langservers-extracted

;; https://www.npmjs.com/package/vscode-langservers-extracted

;;** JSON

;; uses json-beautify -> json-pretty-print to format
(setup (:pkg json-mode))

(with-eval-after-load 'json-mode
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode)))

(setup (:pkg jq-mode)
  (:file-match "\\.jq\\'"))

;;** Javascript

(setup (:pkg typescript-mode)
  (:file-match "\\.ts\\'")
  ;; (:hook eglot-ensure)
  (setq typescript-indent-level 2))

(defun dw/set-js-indentation ()
  (setq-default js-indent-level 2)
  ;; (setq-default evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(setup (:pkg js2-mode)
  (:file-match "\\.jsx?\\'")

  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))

;;** Markdown

(setup (:pkg markdown-mode)
  (setq markdown-command "marked")
  (:file-match "\\.md\\'")
  (:when-loaded
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face)))))

;;** Markup

;;*** NXML

(setq nxml-slash-auto-complete-flag t)

;;*** SGML

;;*** ESXML


;;** CSS

;; https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-css.el

(with-eval-after-load 'css-mode
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode)))

;;*** SASS

;;*** LESS

;;** HTML

;;*** Web Mode

;; TODO: remove web-mode (mhtml uses treesitter).

;; html files are still associating with mhtml-mode

(setup (:pkg web-mode)
  (:file-match "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'")
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;;*** Processes

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(setup (:pkg impatient-mode :straight t))
(setup (:pkg skewer-mode))

(with-eval-after-load 'simple-httpd
  (add-to-list 'httpd-mime-types '("wasm" . "application/wasm")))

(provide 'dc-dev-web)
