;; From https://cs.opensource.google/go/x/tools/+/refs/tags/gopls/v0.13.2:gopls/doc/emacs.md

(defun dc/project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

;; see overloading for (cl-defgeneric project-root (project))
;; (cl-defmethod project-root (project
;;                              &context (project--within-roots-fallback
;;                                        (eql nil))) ...)

;; TODO: finish configuring go-mode and gopls

(use-package go-mode :straight (:type built-in) :defer t
  :config
  (add-hook 'project-find-functions #'dc/project-find-go-module))

;; (add-hook 'go-mode-hook 'eglot-ensure)

;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

;; (add-hook 'go-mode-hook #'eglot-format-buffer-on-save)


;; TODO default project configuration & .dir-locals
;; (setq-default eglot-workspace-configuration
;;               '((:gopls .
;;                         ((staticcheck . t)
;;                          (matcher . "CaseSensitive")))))

;; .dir-locals.
;; ((nil (eglot-workspace-configuration
;;        . ((gopls . ((staticcheck . t)
;; 						        (matcher . "CaseSensitive")))))))
