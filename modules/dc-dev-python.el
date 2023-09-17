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

;;** pyenv

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
      dc/pyenv-default-version "3.10.11"
      dc/pyenv-root-version (dc/pyenv-version dc/pyenv-root
                                              "../python/.pythonversion")
      dc/ipykernel-default-spec-path (expand-file-name (format "3.10.11")))


;;** EIN

;; (setup (:pkg ein :straight t :type git :flavor melpa
;;              :host github :repo "millejoh/emacs-ipython-notebook"))

;; (defun dc/ein-get-token (url)
;;   (gethash (ein:query-divine-authorization-tokens-key url) ein:query-authorization-tokens))
;; (maphash (lambda (k v) (list k v)) ein:query-authorization-tokens)
;; (ein:query-divine-authorization-tokens-key "http://127.0.0.1:8888")
;; (ein:$kernel-session-id "python3")

;;** pylsp

(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   '(python-mode . ("pylsp"))))

;;** formatting

(with-eval-after-load 'aphelia
  (add-to-list 'apheleia-mode-alist '(python-mode . yapf)))


;;** jupyter

;;*** emacs-jupyter

;; both the :kernel and the :session must be set in the same #+HEADER_ARGS line
;; if they aren't (plist-set ...)

(setup (:pkg emacs-jupyter)
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

(with-eval-after-load 'jupyter
  ;; kernels installed via poetry still aren't being found. but this allows me
  ;; to load kernels installed via pyenv
  ;;
  ;; this may not be fixed soon, since `default-directory' has 30 references.
  ;; the comments in jupyter-api-url-request indicate issues with URL handling
  ;; for tramp paths. it seems this handling of default-directory is to
  ;; establish consistent behavior.

  (defun jupyter-runtime-directory ()
    "Return the runtime directory used by Jupyter.
Create the directory if necessary.  If `default-directory' is a
remote directory, return the runtime directory on that remote.

As a side effect, the variable `jupyter-runtime-directory' is set
to the local runtime directory if it is nil."
    (unless jupyter-runtime-directory
      (setq jupyter-runtime-directory
            ;; (let ((default-directory (expand-file-name "~" user-emacs-directory)))
            ;;   (jupyter-command "--runtime-dir"))
            (jupyter-command "--runtime-dir")))
    (let ((dir (if (file-remote-p default-directory)
                   (jupyter-command "--runtime-dir")
                 jupyter-runtime-directory)))
      (unless dir
        (error "Can't obtain runtime directory from jupyter shell command"))
      (prog1 (setq dir (concat (file-remote-p default-directory) dir))
        (make-directory dir 'parents))))

  ;; TODO: this generates the org-babel-FN:jupyter-LANG aliases
  ;;
  ;; systemd will start from $HOME on init, so it's probably all good. it needs
  ;; to be given a valid specs argument. if starting emacs from another
  ;; directory, this will get confusing (i 'll see the ob-jupyter kernel error)
  ;;
  ;; wherever it runs from the above (jupyter-available-kernelspecs t) needs to
  ;; complete if refresh is specified.
  (let ((default-ipykernel-spec
         '(("python3"
            "/data/lang/.pyenv/versions/3.10.11/share/jupyter/kernels/python3"
            :argv ["python"
                   "-m"
                   "ipykernel_launcher"
                   "-f"
                   "{connection_file}"]
            :env nil
            :display_name "Python 3 (ipykernel)"
            :language "python"
            :interrupt_mode "signal"
            :metadata (:debugger t)))))

    ;; (... &rest refresh specs)
    (org-babel-jupyter-aliases-from-kernelspecs nil default-ipykernel-spec))

  ;; before (org-babel-do-load-languages ...) which is where ob-jupyter is req.
  (add-to-list 'dc/org-babel-load-languages '(jupyter . t) t))

(defun dc/jupyter-refresh-kernels ()
  (interactive)
  (jupyter-available-kernelspecs t))

(provide 'dc-dev-python)
