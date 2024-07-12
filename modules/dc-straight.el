;;; dc-straight.el
;;
;; Copyright © 2024 David Conner
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

;; -*- lexical-binding: t; -*-
;; TODO: determine whether this file needs lexical binding

;;* Straight

;; =============================================
;; initially in init.el, but should load early enough to defer this
;; ---------------------------------------------
;; (require 'subr-x)

;; subr-x: needed later (would love to native-comp the core libs)
;; xdg: to locate academic docs across systems
;; map: 'dw-settings
;; dash magit transient with-editor: magit requires these (straight lazy-loads)
;; iso-translate: can load later
;; use-package use-package-diminish: straight can just download anyways
;; a: also used early, but unnecessary

;; (defvar dc/straight-built-in-pseudo-packages '()
;;   "list of packages to append to `straight-built-in-pseudo-packages'")

;; (dolist (pkg '(subr-x xdg iso-transl))
;;   ;; compat
;;   (require pkg)
;;   (add-to-list dc/straight-built-in-pseudo-packages pkg))
;; =============================================

(setq straight-use-package-by-default t)

;;; -- Install Straight.el -----
(unless (featurep 'straight)
  (setq straight-repository-branch "develop")
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name
	  "straight/repos/straight.el/bootstrap.el"
	  (or (bound-and-true-p straight-base-dir)
	      user-emacs-directory)))
	(bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
	  (url-retrieve-synchronously
	   (format "https://raw.githubusercontent.com/radian-software/straight.el/%s/install.el" "develop")
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;;** Early Config

;; (unless (file-directory-p dc/emacs-eln-cache)
;;   (make-directory dc/emacs-eln-cache))

;;*** Pseudo Packages

(defvar dc/straight-built-in-pseudo-packages '()
  "list of packages to append to `straight-built-in-pseudo-packages'

This is really for pseudo-packages and not quasi-packages.

i.e. a dependency specified in Package-Requires header which is
 not a library.  So `M-x find-library emacs' says \"Can't find
 library: emacs\". That's a pseudo-package.

Adding things here will not (or may not) effect straight's deps
graph & clones.")

;; no recipes: subr/x, iso-translate
;; (mapc (apply-partially #'add-to-list 'straight-built-in-pseudo-packages)
;;       dc/straight-built-in-pseudo-packages)

;;*** Straight Functionality

(defun dc/straight-flatten-dependencies (&rest pkgnames)
  (-uniq (-sort #'string< (-flatten (mapcar #'straight-dependencies pkgnames)))))

(defun dc/straight-flatten-dependents (&rest pkgnames)
  (-uniq (-sort #'string< (-flatten (mapcar #'straight-dependents pkgnames)))))

;;*** Builds

;;** Use Package

;; pick one:

;; - always-demand, unless :defer t (default)
;; - always-defer, unless :demand t

;; :bind,:mode,:interpreter -> implies :defer t
;; :demand overrides

;; - :ensure,:pin are /only/ for package.el

;; (require 'use-package)
(straight-use-package '(use-package :type built-in))

(use-package magit :straight t
  :init ;; TODO: move ligher to delight?
  (setq magit-wip-mode-lighter "│§ WIP"
	magit-blame-mode-lighter "│§ BLAME")
  :config
  (setq magit-display-buffer-function
	#'magit-display-buffer-same-window-except-diff-v1)
  :demand t)

;;** Load Path

;; '("/home/dc/.emacs.straight2/straight/build/bind-key"
;;   "/home/dc/.emacs.straight2/straight/build/use-package"
;;   "/home/dc/.emacs.straight2/straight/build/straight"
;;   "/home/dc/.emacs.straight/modules"
;;   "/home/dc/.emacs.g/dw/dw"
;;   "/gnu/store/ia...nm-profile/share/emacs/site-lisp"
;;   "/home/dc/.guix-profile/share/emacs/site-lisp"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/vc"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/use-package"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/url"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/textmodes"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/progmodes"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/play"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/org"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/nxml"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/net"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/mh-e"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/mail"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/leim"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/language"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/international"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/image"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/gnus"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/eshell"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/erc"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/emulation"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/emacs-lisp"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/cedet"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/calendar"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/calc"
;;   "/gnu/store/fs...7v-emacs-pgtk-29.4/share/emacs/29.4/lisp/obsolete")


(provide 'dc-straight)


;; '(pdf-tools org which-key hydra compat ; magit eglot
;;             embark consult corfu cape vertigo marginalia
;;             xref project eldoc eglot jsonrpc flymake track-changes
;;             flycheck
;;             orderless kind-icon)


;; (-other-dir-eln (file-name-nondirectory (directory-file-name (car native-comp-eln-load-path))))
;; requires files.el: (not (file-in-directory-p (car native-comp-eln-load-path) user-emacs-directory))
;;  (-other-dir-eln (car native-comp-eln-load-path))
;; -init-dir-eln (file-name-nondirectory (directory-file-name user-emacs-directory))


;; (defvar dc/emacs-init-file user-init-file "")
;; (defvar dc/emacs-init-dir (directory-file-name user-emacs-directory) "")


;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
;; 
