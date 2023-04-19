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

;;* Dev TCL

(provide 'dc-dev-tcl)

;;** TCL Mode

(setup (:pkg tcl-mode)     ; derived from prog-mode
  ;; defaults for defcustoms
  ;; (:option tcl-help-directory-list nil
  ;;          tcl-use-smart-word-finder t
  ;;          tcl-application "wish"
  ;;          tcl-command-switches nil
  ;;          tcl-command-prompt "^\\(% \\|\\)"
  ;;          inferior-tcl-source-command "source %s\n"
  ;;          tcl-escaped-newline)
  )

;;** TCL Repl
;; (inferior-tcl-mode)  ; derived from comint-mode

;;** TCL LSP

;; Java-based LSP server for TCL
;; https://github.com/Dufgui/lsp-jtcl
;; https://github.com/Dufgui/com.mds.lsp.tcl (perhaps missing)

;; Soar language (tcl derivative?)
;; https://github.com/soartech/soar-language-server
;; TCL integration script in: ./integrations/emacs/lsp-soar.el
