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

;;* Social

;;** Elfeed

(setup (:pkg elfeed))
(setup (:pkg elfeed-org)
  (:option rmh-elfeed-org-files (list (concat dc/emacs-d "elfeed.org")))
  (require 'elfeed-org)
  (elfeed-org))

;;** GNUS
;; - stores messages in gnus-directory, ~/News
;; - fetches from NNTPSERVER or /etc/nntpserver
;; - caches state in ~/.newsrc
;; - gnus-home-directory
;;   - gnus-startup-file
;;   - gnus-init-file
;;   - gnus-directory (set to SAVEDIR if defined)
(require 'bug-reference)

(setup (:pkg gnus)
  (:option gnus-read-newsrc-file nil
           gnus-save-newsrc-file t)
  ;; god bless your soul embark actions ... holy shit lmao
  ;; gnus-newsrc-alist
  ;; gnus-newsrc-hashtb
  ;; gnus-newsrc-options
  ;; gnus-newsrc-options-n
  ;; gnus-save-newsrc-hook
  ;; gnus-newsrc-file-version
  ;; gnus-read-newsrc-el-hook
  ;; gnus-save-quick-newsrc-hook
  ;; gnus-newsrc-last-checked-date
  ;; gnus-save-standard-newsrc-hook
  ;; gnus-save-newsrc-file-last-timestamp
  (:with-hook gnus-summary-mode-hook
    (:hook bug-reference-mode)))

(setup gnus-art
  (:with-hook gnus-article-mode-hook
    (:hook bug-reference-mode)))

;; TODO reappropriate locking from desktop.el
;; - also: https://www.emacswiki.org/emacs/PreventingMultipleGnus
(defvar gnus-lock-filename)
;; gnus--load-locked-desktop-p
;; gnus-claim-lock
;; gnus-release-lock

;;** Debbugs
;; see debbugs.el manual for:
;; - integrating with GNUS to apply patches to a worktree
;;   https://elpa.gnu.org/packages/doc/debbugs-ug.html#Applying-Patches
;; - other version-control-based workflows are described
(setup (:pkg debbugs)
  (:option debbugs-gnu-default-packages '("guix-patches" "guix"))
  (add-hook 'bug-reference-mode-hook 'debbugs-browse-mode)
  (add-hook 'bug-reference-prog-mode-hook 'debbugs-browse-mode))

;; TODO: interactives: prefixed with debbugs-(gnu/org):
;; -package,-bugs,-tagged,-patches,-my-open-bugs,-search,-guix-search

;;** IRC
;; post files/images to 0x0, intended for sharing in ERC

;; retention = min_age + (-max_age + min_age) * pow((file_size / max_size - 1), 3)
(setup (:pkg 0x0 :host gitlab :repo "willvaughn/emacs-0x0" :straight t))

(provide 'dc-social)
