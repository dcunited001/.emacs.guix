;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2023 David Conner
;; Copyright © 2021 David Wilson
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

;;* Project

;;** Project.el

;; :delight "│π →"
(use-package project :straight t :demand t
  :custom
  (project-buffers-viewer 'project-list-buffers-ibuffer)
  (project-vc-extra-root-markers '(".project.el" ".projectile" ".repo"))
  (project-kill-buffers-display-buffer-list t)
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (compile-multi-default-directory 'dc/project-local-root)
  (project-switch-commands
   ;; if the key is absent, corresponding command must exist in
   ;; project-prefix-map
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-find-dir "Find directory")
     (project-vc-dir "VC-Dir")
     (project-eshell "Eshell")
     (magit-status "Magit Status" ?g)
     (magit-project-status "Magit Project Status" ?G))))

;; from SystemCrafters Code Dive: Project.el
;;
;; if project.el is temporarily broken for a project/type
;; and I don't have time to fix it
;; or to grok through the common-lisp types
;;
;; looking forward to using the defgeneric/defmethod ... eventually
(defun dc/project-current-directory-override-set (arg)
  "Function to help remember how to set `project-current-directory-override'"
  (interactive "D")
  (add-dir-local-variable nil 'project-current-directory-override arg))

;;*** Interface Support

(defun dw/current-project-name ()
  (file-name-nondirectory
   (directory-file-name
    (project-root (project-current)))))

(defun dw/switch-project-action ()
  (interactive)
  (let* ((project-name (dw/current-project-name))
         (tab-bar-new-tab-choice #'magit-status)
         (tab-index (tab-bar--tab-index-by-name project-name)))
    (if tab-index
        (tab-bar-select-tab (1+ tab-index))
      (tab-bar-new-tab)
      (tab-bar-rename-tab project-name))))

(defun dw/close-project-tab ()
  (interactive)
  (let* ((project-name (dw/current-project-name))
         (tab-index (tab-bar--tab-index-by-name project-name)))
    (project-kill-buffers t)
    (when tab-index
      (tab-bar-close-tab (1+ tab-index)))))

;;*** Project.el Backends

;; These should useally be done in the `'dc-dev-lang' module

;; https://vannilla.org/write/1609258895/article.html

;;** Projectile

;; projectile-auto-discover is nil
;; trigger project auto-discovery with projectile-discover-projects-in-search-path

;; NOTE: I'm not currently using this
;; (setq projectile-project-search-path '(("/data/repo/" . 1)
;;                                        ("/data/ecto/" . 3))
;;       projectile-mode-line-prefix "│π →")

(provide 'dc-project)
