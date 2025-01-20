;; -*- lexical-binding: t; -*-
;;
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

;;* Dev CPP

;;** GDB

(setq-default gdb-many-windows t)

;;*** Gud

(use-package gud :straight (:type built-in)
  :defer t)

(use-package gdb-mi :straight (:type built-in)
  :defer t
  :after gud)

;;*** Realgud

(use-package realgud :straight t :defer t
  :after (:all gud gdb-mi))

;; ("realgud.el" "realgud/.nosearch" "realgud-recursive-autoloads.el"
;;  ("realgud/common"             "realgud/common/*.el")
;;  ("realgud/common/buffer"      "realgud/common/buffer/*.el")
;;  ("realgud/debugger/bashdb"    "realgud/debugger/bashdb/*.el")
;;  ("realgud/debugger/gdb"       "realgud/debugger/gdb/*.el")
;;  ("realgud/debugger/gub"       "realgud/debugger/gub/*.el")
;;  ("realgud/debugger/ipdb"      "realgud/debugger/ipdb/*.el")
;;  ("realgud/debugger/jdb"       "realgud/debugger/jdb/*.el")
;;  ("realgud/debugger/kshdb"     "realgud/debugger/kshdb/*.el")
;;  ("realgud/debugger/nodejs"    "realgud/debugger/nodejs/*.el")
;;  ("realgud/debugger/pdb"       "realgud/debugger/pdb/*.el")
;;  ("realgud/debugger/perldb"    "realgud/debugger/perldb/*.el")
;;  ("realgud/debugger/rdebug"    "realgud/debugger/rdebug/*.el")
;;  ("realgud/debugger/remake"    "realgud/debugger/remake/*.el")
;;  ("realgud/debugger/trepan"    "realgud/debugger/trepan/*.el")
;;  ("realgud/debugger/trepan.pl" "realgud/debugger/trepan.pl/*.el")
;;  ("realgud/debugger/trepan2"   "realgud/debugger/trepan2/*.el")
;;  ("realgud/debugger/trepan3k"  "realgud/debugger/trepan3k/*.el")
;;  ("realgud/debugger/trepanjs"  "realgud/debugger/trepanjs/*.el")
;;  ("realgud/debugger/zshdb"     "realgud/debugger/zshdb/*.el")
;;  ("realgud/lang" "realgud/lang/*.el"))

;;** Clang Projects

;;** CMake Projects

;;*** KDE

;; https://github.com/xhcoding/qml-ts-mode
(use-package qml-ts-mode
  :disabled
  :straight (:type git :flavor melpa
                   :host github :repo xhcoding/qml-ts-mode)
  :defer t
  :mode (rx ".qml" eos))

;; NOTE: until this merged into 29.1 pgtk from upstream, this is necessary for
;; qml-ts-mode
(defvar js--treesit-sentence-nodes
  '("import_statement"
    "debugger_statement"
    "expression_statement"
    "if_statement"
    "switch_statement"
    "for_statement"
    "for_in_statement"
    "while_statement"
    "do_statement"
    "try_statement"
    "with_statement"
    "break_statement"
    "continue_statement"
    "return_statement"
    "throw_statement"
    "empty_statement"
    "labeled_statement"
    "variable_declaration"
    "lexical_declaration"
    "jsx_element"
    "jsx_self_closing_element")
  "Nodes that designate sentences in JavaScript.
See `treesit-thing-settings' for more information.")

;;*** Project CMake

;; REMOVE: this shuold already happen
;; :mode ((rx bos "CMakeLists.txt" eos) . cmake-ts-mode)

(use-package cmake-ts-mode :straight (:type built-in) :defer t)

;; added to auto-make-alist in dc-shim.el

;; (cmake-project :type git :flavor melpa
;;  :host github :repo "alamaison/emacs-cmake-project")

;; https://github.com/juanjosegarciaripoll/project-cmake

;; NOTE project-cmake:
;;
;; - wraps eglot-ensure
;;\
;; - adds ((c++-mode c-mode)...) to eglot-server-programs
(use-package  project-cmake
  :straight (:type git :host github :repo "juanjosegarciaripoll/project-cmake")
  :after (:all cmake-ts-mode eglot)
  :defer t
  :config
  ;; to integrate with eglot/clang
  (project-cmake-scan-kits)
  (project-cmake-eglot-integration))

;;** Bazel Projects

;; The hedronvision projects simply allow for the extraction of
;; compile_commands.json, which can then be used for eglot via clangd

;; for cpp
;; https://github.com/hedronvision/bazel-compile-commands-extractor

;; for c
;; https://github.com/hedronvision/bazel-make-cc-https-easy

(provide 'dc-dev-cpp)
