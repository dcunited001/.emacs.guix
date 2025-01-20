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

;;** TODO project.el

;; maybe configure:

;; + project-remember-projects-under
;; + project-forget-zombie-projects
;; + project-forget-projects-under


;;*** conditions:

;; project-kill-buffer-conditions, project-ignore-buffer-conditions

;; Each condition is either:
;; - regex:
;; - function: (lambda (buffer) ...) returns non-nil
;; - a cons: (car . cdr)
;;   * `major-mode': if buffer major-mode 'eq cdr major-mode
;;   * `derived-mode': if buffer's major-mode derived from cdr major-mode
;;   * `not': cdr is negation of a condition.
;;   * `and': cdr is recursively eval'd; all have to be met
;;   * `or': cdr is recursively eval'd; one has to be met


;; try:

;; - cl-defmethod dispatch on cl-struct

;; finish:

;; - dc/org-capture-todo-file

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

;;*** Backends

;; `project--value-in-dir' hack-dir-local-variables

;; (defun project--value-in-dir (var dir)
;;   (with-temp-buffer
;;     (setq default-directory dir)
;;     (let ((enable-local-variables :all))
;;       (hack-dir-local-variables-non-file-buffer))
;;     (symbol-value var)))

;;**** PyProject

;; from alexkehayias
;; https://github.com/alexkehayias/emacs.d/blob/master/init.el#L587-L636

(defun dc/project-in-pyproject? (dir)
  (when-let* ((found (locate-dominating-file dir "pyproject.toml")))
    (cons 'pyproject found)))

;; (add-hook 'project-find-functions 'dc/project-in-pyproject? nil nil)

;;**** Cargo

;; NOTE: needs
;; cargo metadata --no-deps --format-version 1 | jq .workspace_root
(defun dc/project-in-cargo? (dir)
  (when-let*
      ((output
        (let ((default-directory dir))
          (shell-command-to-string "cargo metadata --no-deps --format-version 1")))
       (js (ignore-errors (json-read-from-string output)))
       (found (cdr (assq 'workspace_root js))))
    (cons 'cargo found)))


;;*** cl-defmethod

;; After looking into something unrelated (pcase ...), an a-ha moment occured
;;
;; + The flow for these cl-defmethod lambdas should be designed in stratified
;;   layers (see diagrams for this common-lisp abstraction).
;;
;; + The layers should ordered from most-specific (first functionality) to
;;   most-specific at the bottom
;;
;; =============================================
;;
;; An example:
;;
;; + An ansible project that includes terraform content
;;
;; + has python environment, specifying most dependencies via nix/guix manifest
;;
;; + incorporates some kind of IoT build as a deployable artifact with source in
;;   a git submodule (maybe). The build is expected to include names/addresses
;;   of the innocent in its binary to be deployed onto ESP via serial port,
;;   potentially debug symbols injected for development (multiple builds)
;;
;; This project:
;;
;; + will need to handle YAML with special treatment for ansible depending on
;;   their path
;;
;; + should tweak file-find, lsp/eglot and compile functionality based on the
;;   current buffers' modes/paths.
;;
;; + should imagine "stovepipes" of project.el functionality that ascend the
;;   layers, but where the layers are stratified. For the top layers of
;;   stovepipes should, the cl-defmethod should match only once, then specifying
;;   a chain of functionality to execute at lower layers. Finally, the lowest layers
;;   should execute their functionality (once), imagined as left-to-right.
;;
;; |-------------+---------+-----------+---------------------+-----------------------+-----|
;; | python-mode | ansible |           | zig-mode            | c/c++                 |     |
;; |-------------+---------+-----------+---------------------+-----------------------+-----|
;; |             | YAML    | Terraform |                     | cmake (arduino/esp32) |     |
;; |-------------+---------+-----------+---------------------+-----------------------+-----|
;; | uv/poetry   |         |           | <- compile tasks -> |                       | tex |
;; |-------------+---------+-----------+---------------------+-----------------------+-----|
;; | Guix/Nix -> |         |           |                     |                       |     |
;; |-------------+---------+-----------+---------------------+-----------------------+-----|
;;
;; =============================================

;; interface:
;; (cl-defmethod project-root (project ()) ...)
;; (cl-defmethod project-root (project (head transient)) ...)

;; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el#L622-L631
;; (cl-defmethod project-root ((project (head vc))) ...)
;; (cl-defmethod project-root ((project (head vc)) &optional dirs) ...)

;; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el#L633-L652
;; (cl-defmethod project-files ((project (head vc)) &optional dirs) ...)

;; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el#L774-L806
;; (cl-defmethod project-ignores ((project (head vc)) dir) ...)

;; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el#L842C2-L861
;; (cl-defmethod project-buffers ((project (head vc))) ...)
;; (cl-defmethod project-name ((proejct (head vc))) ...)

;; These should useally be done in the `'dc-dev-lang' module

;; https://vannilla.org/write/1609258895/article.html

;; example: multiple backends for cmake/autotools
;; https://github.com/Ergus/project-multi-mode/blob/master/project-multi-mode.el


;;** Projectile

;; projectile-auto-discover is nil
;; trigger project auto-discovery with projectile-discover-projects-in-search-path

;; NOTE: I'm not currently using this
;; (setq projectile-project-search-path '(("/data/repo/" . 1)
;;                                        ("/data/ecto/" . 3))
;;       projectile-mode-line-prefix "│π →")

(provide 'dc-project)
