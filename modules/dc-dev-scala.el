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

(use-package scala-mode :disabled
  :straight (:type git :flavor melpa
                   :host github :repo "hvesalai/emacs-scala-mode"))

;; sbt-mode is for comint buffers, .sbt files should still use scala
(use-package sbt-mode :disabled
  ;; the fork enables using tramp when running SBT in docker
  ;; :fork (:host github :protocol ssh :repo "dcunited001/emacs-sbt-mode")
  :straight (:type git :flavor melpa :host github :repo "hvesalai/emacs-sbt-mode")
  :custom
  (sbt:program-options '("-Djline.terminal=none"
                         "-Dsbt.supershell=false")))

;; TODO: also suggested is a fix for the minibuffer

(provide 'dc-dev-scala)
