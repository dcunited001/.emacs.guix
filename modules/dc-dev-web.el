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
(use-package json-mode :straight t :defer t
  :init
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode)))

(use-package jq-mode :straight t :defer t
  :mode ((rx "." (| "jq") eos) . jq-mode))

;;** Javascript

(use-package typescript-mode :straight t :defer t
  :mode ((rx "." (| "ts") eos) . typescript-mode)
  ;; (:hook eglot-ensure)
  :custom
  (typescript-indent-level 2))

(defun dw/set-js-indentation ()
  (setq-default js-indent-level 2)
  ;; (setq-default evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode :straight t :defer t
  :mode ((rx "." (| "js" "jsx") eos) . js2-mode)

  ;; TODO set :magic js2-mode for Node scripts
  ;; (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  :custom
  (js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  :hook
  ((js2-mode-hook js2-mode-hook) . dw/set-js-indentation))

;;*** Eglot Biome

;; TODO: get biome's LSP to coordinate communication to LSP
;; biome can run with the vscode.scm startup manifest

;; (add-to-list
;;  'eglot-server-programs
;;  `((javascript-mode jsx-mode typescript-mode typescript-tsx-mode) .
;;    ,(a-get* eglot-server-programs 'javascript-mode)))

;;** Markdown

(use-package markdown-mode :straight t :defer t
  :custom
  (markdown-command "marked")

  :mode ((rx "." (| "md" "mdx") eos) . markdown-mode)
  :hook
  (markdown-mode-hook . visual-line-mode)
  (markdown-mode-hook . (lambda () (setq-local truncate-lines nil))))

;; (:when-loaded
;;   (dolist (face '((markdown-header-face-1 . 1.2)
;;                   (markdown-header-face-2 . 1.1)
;;                   (markdown-header-face-3 . 1.0)
;;                   (markdown-header-face-4 . 1.0)
;;                   (markdown-header-face-5 . 1.0)))
;;     (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

;;** Astro

;; requires treesitter grammars: astro-ts, css, html and typescript-tsx
;;
;; astro-ts isn't provided by guix and is specified in dc-dev

;; NOTE: eglot in astro-ts-mode definitely seems to require eglot 0.17 (0.12.x
;; did not worK). it also does not like it when there are two LSP servers
;; running ... but whatever VSCode runs doesn't seem to bother eglot's
;; connection to AstroLS
;;
;; (:pkg astro-ts-mode :straight t :type git :flavor melpa
;;              :host github :repo "Sorixelle/astro-ts-mode")

(defun dc/astro-fix-completion ()
  (setq-local completion-at-point-functions
              (delete 'html-mode--complete-at-point
                      completion-at-point-functions)))

(use-package astro-ts-mode :straight t :defer t
  :init
  (add-to-list 'major-mode-remap-alist '(astro-mode . astro-ts-mode))
  :hook
  (astro-ts-mode-hook . dc/astro-fix-completion))

;; TODO: move to dc-org?
(use-package astro-ts-mode :straight t :defer t
  :mode ((rx "." (| "astro") eos) . astro-ts-mode)
  :after org
  (add-to-list 'org-src-lang-modes '("astro" . astro-ts)))

;;** Markup

;; older package
;; (setup (:pkg tidy :type git :host github :repo "emacsmirror/tidy"))

;;*** NXML

(use-package nxml-mode :straight (:type built-in)
  :custom
  (nxml-slash-auto-complete-flag t))

;;*** SGML

;;*** ESXML


;;** CSS

;; https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-css.el

(use-package css-mode :straight (:type built-in) :defer t
  :init
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode)))

;;*** SASS

;;*** LESS

;;** HTML

;;*** html-ts-mode

(use-package html-ts-mode
  :straight (:type git :host github :repo "mickeynp/html-ts-mode"
                   :flavor melpa :files ("html-ts-mode.el"))
  :defer t
  :mode ((rx "." (| "htm" "html") eos) . html-ts-mode)

  :init
  ;; TODO: tidy refuses to format things without <!DOCTYPE> and i'm in liquid...
  (add-to-list 'major-mode-remap-alist '(mhtml-mode . html-ts-mode)))

;;*** Web Mode

;; html files are still associating with mhtml-mode

;; (pop apheleia-mode-alist)

(use-package web-mode :straight (:type built-in)
  :defer t
  ;; (:file-match "\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'")
  ;; :mode ((rx "." (| "ejs" "tsx" "jsx") eos) . web-mode)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-attribute-indent-offset 2))

;;*** Processes


;;**** Servers

;; impatient-mode allows emacs to function as a web-server

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
;; (setup (:pkg impatient-mode :straight t))

;; (setup (:pkg skewer-mode))

;; (with-eval-after-load 'simple-httpd
;;   (add-to-list 'httpd-mime-types '("wasm" . "application/wasm")))

(provide 'dc-dev-web)
