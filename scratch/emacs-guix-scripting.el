
;; =============================================

;; TODO: figure out how to take a list of packages from `guix import'
;; (with or without version information) and iterate over it

;; guix-packages-by-name calls
;; (guix-package-get-display profile 'name name)

;; this transforms args into entries (for bui search)
;; (cl-list* (guix-package-profile guix-current-profile) 'name "pcscd")

;; bui-get-entries will (apply...) against a function passed to it)
;; (bui-get-entries (guix-package-list-type) 'list args)

(let* ((pkg-name (list "emacs-kind-icon"))
       (args (cl-list* (guix-package-profile guix-current-profile) 'name pkg-name)))
  (bui-get-entries (guix-package-list-type) 'list args))

;; (((id . "139152839767072:out")
;;   (output . "out")
;;   (installed)
;;   (package-id . 139152839767072)
;;   (name . "emacs-kind-icon")
;;   (version . "0.2.1")
;;   (synopsis . "Completion kind icons in Emacs")
;;   (hidden)
;;   (known-status . known)
;;   (superseded)))
