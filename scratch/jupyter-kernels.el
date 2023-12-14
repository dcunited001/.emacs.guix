;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.



;; =============================================
;; listing the kernelspecs (they're cl structs now)
(let (thekeys '())
  (maphash (lambda (k v) (push (list k v) thekeys)) jupyter--kernelspecs)
  thekeys)



;; =>
;;

'(("local" #s
   (hash-table size 5 test equal rehash-size 1.5 rehash-threshold 0.8125 data
               (nil
                (#s
                 (jupyter-kernelspec
                  "python3"
                  (:argv
                   ["/gnu/store/94ccg3my0xpsy3rna5bwf664g708ascy-python-toolchain-3.10.7/bin/python"
                    "-m" "ipykernel_launcher" "-f" "{connection_file}"]
                   :env nil
                   :display_name "Python 3 (ipykernel)"
                   :language "python"
                   :interrupt_mode "signal"
                   :metadata (:debugger t))
                  "/gnu/store/bmbfj937k0zjrjkn2a91xva6fm3m6vah-python-ipykernel-bootstrap-6.13.0/lib/python3.10/site-packages/ipykernel/resources"))))))

;; =============================================
;; other notes



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

;; =============================================
;;

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
