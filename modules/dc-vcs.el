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

;;* VCS

;;** Diff



;;*** Patches



;;*** Smerge

;; reopen hydra
(use-package hydra
  :config
  (defhydra dw/smerge-panel ()
    "smerge"
    ("k" (smerge-prev) "prev change" )
    ("j" (smerge-next) "next change")
    ("u" (smerge-keep-upper) "keep upper")
    ("l" (smerge-keep-lower) "keep lower")
    ("q" nil "quit" :exit t)))

;;*** Emerge


;;** Git

;; + inside a superproject, git-link breaks if there isn't an "origin"
;;
;; + set `git-link-use-commit' and use `C-u' `git-link'


(use-package git-link :straight t :demand t
  :custom
  (git-link-open-in-browser t)
  (git-link-use-commit t))

;; test ; (git-link--parse-remote remote-url)
(defun dc/git-link-remote-add-gitlab (host)
  ;; gitlab uses same homepage URL format as github
  (add-to-list 'git-link-remote-alist `(,host git-link-gitlab))
  (add-to-list 'git-link-commit-remote-alist `(,host git-link-commit-gitlab))
  (add-to-list 'git-link-homepage-remote-alist `(,host git-link-homepage-github)))

;;*** Git Timemachine
;; control-f8, like facebook's conference
(use-package git-timemachine :straight t :defer t)

;; TODO: DOOM: defadvice! +vc-support-git-timemachine-a (fn)
;; TODO: DOOM: defadvice! +vc-update-header-line-a (revision)
;; TODO: DOOM: keybindings
;; (map! :map git-timemachine-mode-map
;;       :n "C-p" #'git-timemachine-show-previous-revision
;;       :n "C-n" #'git-timemachine-show-next-revision
;;       :n "gb"  #'git-timemachine-blame
;;       :n "gtc" #'git-timemachine-show-commit)

;;** Magit

(use-package magit :straight t
  :init ;; TODO: move ligher to delight?
  (setq magit-wip-mode-lighter "│§ WIP"
	      magit-blame-mode-lighter "│§ BLAME")
  :config
  (setq magit-display-buffer-function
	#'magit-display-buffer-same-window-except-diff-v1)
  :demand t)

(use-package magit-todos :straight t :after magit :demand t
  :hook
  (emacs-startup-hook . magit-todos-mode))

;; interface to git-tbdiff, gives better control over git ranges
(use-package magit-tbdiff :straight t :after magit :demand t)

;; TODO: interactive: magit-tbdiff-ranges
;; TODO: interactive: magit-tbdiff-revs
;; TODO: interactive: magit-tbdiff-with-base
;; TODO: interactive: magit-tbdiff-save

;; (with-eval-after-load 'magit
;;  (require 'dc-vcs-magit))

;;*** Ghub

(defun dc/ensure-ghub-graphql ()
  ;; can't load this, but there are examples of ghub/graphql
  ;; (require 'graphql-examples)
  (require 'graphql)
  (require 'ghub-graphql)
  (require 'glab)
  (require 'gtea))

(use-package graphql :straight t :demand t)

;; graphql propagates ghub input, but only for 'graphql-examples (ehh?)

(use-package ghub :straight t :after graphql :demand t
  ;; TODO: check that graphql and ghub-graphql get loaded
  ;; :hook (emacs-startup-hook . dc/ensure-ghub-graphql)
  )

;;*** Forge
(use-package forge :straight t :demand t
  :after (:all magit graphql ghub)
  :custom
  (forge-pull-notifications t)

  :config
  (add-to-list 'forge-alist
	             '("invent.kde.org"
		             "invent.kde.org/api/v4"
		             "invent.kde.org"
		             forge-gitlab-repository))
  (add-to-list 'forge-alist
	             '("gitlab.freedestkop.org"
		             "gitlab.freedesktop.org/api/v4"
		             "gitlab.freedesktop.org"
		             forge-gitlab-repository)))

;; https://github.com/emacs-straight/repology/blob/master/repology.el
(use-package repology :straight t :defer t)

;;** Forges

;;*** Consult-GH

(setq dc/clone-club (expand-file-name "gh" dc/lang-path)
      consult-gh-default-clone-directory dc/clone-club)
(unless (file-exists-p dc/clone-club)
  (mkdir dc/clone-club))

;; https://github.com/armindarvish/consult-gh

(defun dc/load-consult-gh ()
  "Load code for consult-gh. Req. reloading f1-f2 keybinds"

  ;; req. cmdline gh tool :( but otherwise really cool. I've never used that bc
  ;; of the cookie storage. making an alias that uses something like <(gpg ...)>
  ;; may work, but it may be a bad idea. the API doesn't yet integrate with
  ;; 'auth-secrets or `pass`, though it probably could
  ;;
  ;; (consult-gh :type git :host github :repo "emacsmirror/consult-gh)"
  (use-package consult-gh :disabled
    :straight (:type git :host github
                     :repo "armindarvish/consult-gh" :branch "main")
    ;; :after forge embark
    :defer t
    ;; TODO customize consult-gh-default-orgs-list (for M-x consult-gh-default-repos)
    :custom
    (consult-gh-large-file-warning-threshold 2500000)
    (consult-gh-prioritize-local-folder 'suggest)

    (consult-gh-forge-timeout-seconds 12)
    ;; preview gh on demand (avoids large files)
    (consult-gh-show-preview t)
    (consult-gh-preview-key "M-o")
    ;; consult-gh-preview-buffer-mode 'org-mode ;preview in org
    ;; do things in emacs

    (consult-gh-issue-action #'consult-gh--issue-view-action)
    (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
    (consult-gh-file-action #'consult-gh--files-view-action)

    :config
    (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
    (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)

    :after forge :config (require 'consult-gh-forge)
    :after embark :config (require 'consult-gh-embark)))

;;*** Sr.ht
(use-package srht :straight t :defer t
  :custom (srht-username user-mail-address))

;; (srht :type git :host github :repo "emacs-straight/srht" :files ("*" (:exclude ".git")))

;;*** Repo
;; For Google Repo
(use-package repo :straight t :defer t)

;; TODO: repo interactives/customs: repo-status, repo-init...

(provide 'dc-vcs)
