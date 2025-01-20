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

;;* Checking

;;** Flymake

;; C-x = to get position
;; run (flycheck-error-level-interesting-at-pos-p pos)
;; runs (flycheck-error-level-interesting-p (get-char-property pos 'flycheck-error))

(use-package flymake :straight (:type built-in)
 :custom
 (flymake-mode-line-lighter  "│♠µ"))

 ;; :config ; flymake-proc backend is deprecated
 ;; ;; also:
 ;; ;; - flymake-proc-ignored-file-name-regexps
 ;; ;; - flymake-proc-proc-xml-program
 ;; (add-to-list 'flymake-proc-allowed-file-name-masks
 ;;              '("\\.xml\\'" flymake-proc-xml-init))

(use-package flymake-shellcheck :straight t
  :commands flymake-shellcheck-load
  :init
  (dolist (m '(sh-base-mode))
    ;; still requires invoking M-x `flymake-mode'
    (add-hook m 'flymake-shellcheck-load)))

;;** Flycheck

;; This is a quick survey of flycheck and org-babel functionality
;; https://github.com/jkitchin/scimax/commit/9a039cfc5fcdf0114a72d23d34b78a8b3d4349c9

(use-package flycheck :straight t
  :custom
  ;; display
  (flycheck-highlighting-mode 'columns)
  (flycheck-navigation-minimum-level 'error)
  (flycheck-error-list-minimum-level 'warning)

  (flycheck-mode-line-prefix "│♠√")

  ;; code
  (flycheck-emacs-lisp-load-path 'inherit)

  ;; modes: specify these here ... but not using flycheck-global-mode
  (flycheck-global-modes '(;emacs-lisp-mode sh-mode bash-ts-mode
                           xml-mode nxml-mode
                           python-mode python-ts-mode))

  ;; (:also-load flycheck-guile)
  ;; (:also-load flycheck-package)
  :config
  (setq-default dc/flycheck-highlighting-style-default flycheck-highlighting-style
                dc/flycheck-highlighting-styles `(nil ,dc/flycheck-highlighting-style-default)
                dc/flycheck-warnings-have-underlines nil)

  (dolist (m flycheck-global-modes)
    (if-let* ((da-mode-name (derived-mode-hook-name m)))
	(add-hook da-mode-name #'flycheck-mode)
      ;; let me know about it
      (when DBUS_FOUND
	(alert (format "%s's hook is giving you lip" (symbol-name da-mode-name))
	       :title "Flycheck")))))

;; use M-x flycheck-error-list-set-filter to change (f, F in the list buffer)

(defun dc/toggle-flycheck-highlighting-style ()
  "Don't taze me bro."
  (interactive)
  (setq-default flycheck-highlighting-style
                (if flycheck-highlighting-style
                    nil
                  dc/flycheck-highlighting-style-default))
  (and flycheck-mode
       flycheck-highlighting-mode
       (flycheck-refresh-fringes-and-margins)))

;; see .emacs.doom/modules/checkers/javascript/config.el
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)
;; (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
;; (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode)
;; tide not completely compatible with LSP

;;*** Tweaks

;; fix marginalia annotations (from Doom Emacs)
;; NOTE 20240705: this command already has annotations
;; also ... No annotators found for category ‘builtin’
;;   (add-to-list 'marginalia-command-categories
;;                '(flycheck-error-list-set-filter . builtin))

;;*** Flycheck Misc Packages

;; Flycheck Color Mode Line
;; https://github.com/flycheck/flycheck-color-mode-line/blob/master/flycheck-color-mode-line.el

;;*** Consult Flycheck

;; (with-eval-after-load 'flycheck
(use-package consult-flycheck :straight t
  :after flycheck)

;;** Juggle flycheck faces

;; sometimes ... you don't actually want a warning per 2 sLOC.

;; still have to reload the theme to refresh the face
;; (dc/toggleable-boolean dc/flycheck-warnings-have-underlines)

;; (add-hook
;;  'ef-themes-post-load-hook
;;  (lambda ()
;;    (defface dc/flycheck-warning
;;      (get 'flycheck-warning 'theme-face)
;;      "Flycheck face for warnings, with underlines.")))

;; (defface dc/flycheck-warning-no-underline
;;   '((t :inherit warning))
;;   "Flycheck face for warnings.")

(provide 'dc-fly)
