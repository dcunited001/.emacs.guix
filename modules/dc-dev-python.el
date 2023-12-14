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

(setq python-indent-offset 4)

(add-to-list 'major-mode-remap-alist
             '(python-mode . python-ts-mode))

;;** formatting

;; (with-eval-after-load 'apheleia
;;   (add-to-list 'apheleia-mode-alist '(python-mode . yapf)))

;;** LSP

;;*** pylsp

(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   '(python-mode . ("pylsp"))))

;;** Fly

;;*** Flymake

;; python-flymake-command:
;; '("flake8" "-") or ("pylint" "--from-stdin" "stdin")

;;** Environment

;;*** pyenv

(let* ((pyenv-python-version "3.12.1")
      (pyenv-python-root
       (format "/data/lang/.pyenv/versions/%s" pyenv-python-version))
      (guix-python-root
       (string-join (list (getenv "GUIX_EXTRA") "pythondev" "pythondev") "/")))
  (setq dc/python-root (cond (dw/is-guix-system guix-python-root)
                             (dw/is-guix-system pyenv-python-root))))

;; =============================================
;; TODO: fix pyenv code.... i never have time to get this working on arch AND
;; guix AND kde AND i3 AND wayland AND sway.... some parts are *incredibly*
;; divergent and don't exactly lend themselves to easy reuse or modularity.
;; those parts have *almost nothing to do with guix*.........

(defun dc/pyenv-version (pyenv-root &optional rel-path)
  ;; TODO: typically set rel-path to default-directory
  (if-let* ((pyversion-file (file-truename
                             (expand-file-name rel-path dc/pyenv-root)))
            ;; is file-truename + expand-file-name redundant?
            (pyversion-content (and (file-exists-p pyversion-file)
                                    (with-temp-buffer (insert-file-contents
                                                       pyversion-file)
                                                      (s-trim (buffer-string))))))
      pyversion-content
    dc/pyenv-default-version))

;; ("/data/lang/.pyenv/versions/3.10.11/share/jupyter/kernels/python3")
(defun dc/pyenv-ipykernel (pyenv-root &optional ver)
  ;; python3
  (let ((ver (or ver dc/pyenv-default-version))
        (ipy (string-join '("versions" ver) "/"))
        (relk (string-join '("share" "jupyter" "kernels" "python3") "/")))
    (thread-first
      pyenv-root
      (expand-file-name))
    )
  )


(setq dc/pyenv-root (or (getenv "PYENV_ROOT") "/data/lang/.pyenv")
      dc/pyenv-default-version "3.12.1"
      dc/pyenv-root-version (dc/pyenv-version dc/pyenv-root
                                              "../python/.pythonversion")
      dc/ipykernel-default-spec-path (expand-file-name (format "3.10.11")))

;;  i like literally want to use a python cli to generate palettes from imagres
;;  but maybe run it in jupyter... omfg 3 hours later. and 3 months later, same
;;  problem. i need jupyter to run on a fucking network.

;; =============================================

;;** Notebooks

;;*** EIN

;; (setup (:pkg ein :straight t :type git :flavor melpa
;;              :host github :repo "millejoh/emacs-ipython-notebook"))

;; (defun dc/ein-get-token (url)
;;   (gethash (ein:query-divine-authorization-tokens-key url) ein:query-authorization-tokens))
;; (maphash (lambda (k v) (list k v)) ein:query-authorization-tokens)
;; (ein:query-divine-authorization-tokens-key "http://127.0.0.1:8888")
;; (ein:$kernel-session-id "python3")

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

(setup (:pkg jupyter :straight t :type git :flavor melpa
             :host github :repo "emacs-jupyter/jupyter"
             :files (:defaults "Makefile" "widget.html" "js" "jupyter-pkg.el"))
  (:option org-babel-default-header-args:jupyter-python
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
	           (:eval . "never-export"))))

(with-eval-after-load 'jupyter-autoloads
  (require 'jupyter)
  (require 'ob-jupyter)

  ;; TODO: this generates the org-babel-FN:jupyter-LANG aliases
  ;;
  ;; systemd will start from $HOME on init, so it's probably all good. it needs
  ;; to be given a valid specs argument. if starting emacs from another
  ;; directory, this will get confusing (i 'll see the ob-jupyter kernel error)
  ;;
  ;; wherever it runs from the above (jupyter-available-kernelspecs t) needs to
  ;; complete if refresh is specified.
  ;; (let ((default-ipykernel-spec
  ;;        (make-jupyter-kernelspec
  ;;         :name "Python 3 (ipykernel)"
  ;;         :plist '(:argv ["python"
  ;;                         "-m"
  ;;                         "ipykernel_launcher"
  ;;                         "-f"
  ;;                         "{connection_file}"]
  ;;                        :env nil
  ;;                        :display_name "Python 3 (ipykernel)"
  ;;                        :language "python"
  ;;                        :interrupt_mode "signal"
  ;;                        :metadata (:debugger t))
  ;;         :resource-directory
  ;;         (expand-file-name
  ;;          "share/jupyter/kernels/python3" dc/python-root))))

  ;;   ;; (... &rest refresh specs)
  ;;   ;; (org-babel-jupyter-aliases-from-kernelspecs nil default-ipykernel-spec)
  ;;   default-ipykernel-spec)


  ;; (org-babel-jupyter-aliases-from-kernelspecs)
  ;; before (org-babel-do-load-languages ...) which is where ob-jupyter is req.
  ;;
  ;; NOTE: 12/13/2023 ... which will never be successful on arch (and guix?)
  ;; since the main jupyter kernel is installed under the pyenv 3.10.11
  ;;
  ;; a solution would be to remove the changes to default directory above and
  ;; set ~/.python-version and ... which has been confusing in the past
  (add-to-list 'dc/org-babel-load-languages '(jupyter . t) t))

(defun dc/jupyter-refresh-kernels ()
  (interactive)
  (jupyter-available-kernelspecs t))

(provide 'dc-dev-python)
