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

;;* Dev Python

(use-package python-mode :straight (:type built-in) :demand t
  :custom
  (python-indent-offset 4)
  :init
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode)))

;;** formatting

;; (with-eval-after-load 'apheleia
;;   (add-to-list 'apheleia-mode-alist '(python-mode . yapf)))

;;** LSP

;;*** pylsp

;;** Fly

;;*** Flymake

;; python-flymake-command:
;; '("flake8" "-") or ("pylint" "--from-stdin" "stdin")

;;** Environment

;;*** pyenv

;; well... that simplified everything greatly

;;** Notebooks

;;*** jupyter

;;**** emacs-jupyter

;; both the :kernel and the :session must be set in the same #+HEADER_ARGS line
;; if they aren't (plist-set ...)

;; the master branch is way ahead of 0.8.3
;;
;; i may want to use a newer version:
;;
;; - new versions fixes for kernelspec JSON parsing (when debugpy is installed):
;;   - https://github.com/emacs-jupyter/jupyter/issues/446
;; - may fix https://github.com/emacs-jupyter/jupyter/issues/481 & 483

(use-package jupyter :straight t :defer t
  :custom
  (org-babel-default-header-args:jupyter-python
   ;; TODO set :results
   '((:results . "both")
	   (:session . "jupyter-python")
	   ;; (:kernel . "python3")
	   (:pandoc . "t")
	   ;; (:exports . "both")
	   (:cache .   "no")
	   (:noweb . "no")
	   (:hlines . "no")
	   (:tangle . "no")
	   (:eval . "never-export")))

  ;; TODO move to emacs-startup-hook or similar?
  :config
  (require 'ob-jupyter)

  ;; TODO: this finds the kernelspecs, but only sets one alias for each
  ;; language, since :kernel is expected to be set in the org header
  (org-babel-jupyter-aliases-from-kernelspecs)

  ;; (org-babel-jupyter-aliases-from-kernelspecs t jupyter--kernelspecs)

  (add-to-list 'dc/org-babel-load-languages '(jupyter . t) t))

(defun dc/jupyter-refresh-kernels ()
  (interactive)
  (jupyter-available-kernelspecs t))

(provide 'dc-dev-python)
