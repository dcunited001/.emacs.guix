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

;;* YAML

(defun dc/setup-jq-for-yaml ()
  (setq-local jq-interactive-command "yq"
              jq-interactive-font-lock-mode #'yaml-mode
              jq-interactive-default-options "--yaml-roundtrip"))

(use-package yaml-mode :straight t :defer t
  ;; yaml-mode is already in auto-mode-alist
  ;; (:file-match "\\.ya?ml\\'")

  ;; (:unbind "C-M-i")
  :hook (yaml-mode-hook . dc/setup-jq-for-yaml))

(use-package yaml-ts-mode :straight (:type built-in) :defer t
  :init
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
  :config
  (unbind-key "C-M-i" 'yaml-ts-mode-map)
  :hook
  (yaml-ts-mode . dc/setup-jq-for-yaml))

;; TODO: change number completion candidites or orderless matching
;; - too many candidiates and server responses are too long

;; TODO: ensure flycheck is aware of ansible-language-server results

;;** Ansible

;; i'm not the only who thought of this
;; https://github.com/emacs-lsp/lsp-mode/issues/3687
(define-derived-mode ansible-mode yaml-mode "Ansible"
  "Major mode which is YAML-mode + ansible minor mode.")

(define-derived-mode ansible-ts-mode yaml-ts-mode "Ansible TS"
  "Major mode which is YAML-mode + ansible minor mode.")

;; either .dir-locals.el or k1LoW/emacs-ansible: required to distinguish ansible
;; buffers from yml buffers
(use-package ansible
  :straight (:type git :host github :repo "k1LoW/emacs-ansible"
                   :flavor melpa :files ("*.el" "snippets" "dict" "ansible-pkg.el"))
  :defer t
  :after yaml-ts-mode

  :hook
  ((ansible-mode-hook ansible-ts-mode-hook) . ansible)
  (ansible-ts-mode-hook . combobulate-mode))

;;** Kubernetes


;;** Helm


(provide 'dc-dev-yaml)
