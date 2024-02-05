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

;; Dev CPP

;;** Clang Projects

;;** CMake Projects

;;*** KDE

(setup (:pkg qml-ts-mode)
  (:file-match "\\.qml\\'"))

;;*** Project CMake

(require 'cmake-ts-mode)
(add-to-list 'auto-mode-alist '("^CMakeLists\\.txt$\\'" . cmake-ts-mode))

;; https://github.com/juanjosegarciaripoll/project-cmake

;; to integrate with eglot/clang
(setup (:pkg project-cmake :straight t :type git
             :host github :repo "juanjosegarciaripoll/project-cmake")
  (require 'project-cmake)
  (require 'eglot)
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
