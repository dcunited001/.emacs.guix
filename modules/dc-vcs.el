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

(with-eval-after-load 'hydra
  (defhydra dw/smerge-panel ()
    "smerge"
    ("k" (smerge-prev) "prev change" )
    ("j" (smerge-next) "next change")
    ("u" (smerge-keep-upper) "keep upper")
    ("l" (smerge-keep-lower) "keep lower")
    ("q" nil "quit" :exit t)))

;;*** Emerge


;;** Git

;; + (git-link) doesn't work for repositories inside a repo superproject
;;
;; + C-u `git-link' does work, but needs to have `git-link-use-commit' set

(setup (:pkg git-link)
  (:option git-link-open-in-browser t
           git-link-use-commit t))


;;*** Git Timemachine
;; control-f8, like facebook's conference
(setup (:pkg git-timemachine))

;; TODO: DOOM: defadvice! +vc-support-git-timemachine-a (fn)
;; TODO: DOOM: defadvice! +vc-update-header-line-a (revision)
;; TODO: DOOM: keybindings
;; (map! :map git-timemachine-mode-map
;;       :n "C-p" #'git-timemachine-show-previous-revision
;;       :n "C-n" #'git-timemachine-show-next-revision
;;       :n "gb"  #'git-timemachine-blame
;;       :n "gtc" #'git-timemachine-show-commit)

;;** Magit

(setup (:pkg magit :straight t)
  (:option magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1

           magit-wip-mode-lighter "│§ WIP"
           magit-blame-mode-lighter "│§ BLAME"

           ;; t show diffchars when point on hunk
           ;; 'all to always show diffchars
           magit-diff-refine-hunk t))

(setup (:pkg magit-todos :straight t)
  (:with-hook emacs-startup-hook
    (:hook magit-todos-mode)))

;; interface to git-tbdiff, gives better control over git ranges
(setup (:pkg magit-tbdiff :straight t))

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

(setup (:pkg ghub :straight t)
  (:with-hook emacs-startup-hook
    (:hook #'dc/ensure-ghub-graphql)))

;; graphql propagates ghub input, but only for 'graphql-examples (ehh?)

(setup (:pkg graphql :straight t))

;;*** Forge
(setup (:pkg forge :straight t)
  (:option forge-pull-notifications t))

(with-eval-after-load 'forge
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
(setup (:pkg repology))

;;** Forges

;;*** Consult-GH

(with-eval-after-load 'hydra
  (defhydra dw/smerge-panel ()
    "smerge"
    ("k" (smerge-prev) "prev change" )
    ("j" (smerge-next) "next change")
    ("u" (smerge-keep-upper) "keep upper")
    ("l" (smerge-keep-lower) "keep lower")
    ("q" nil "quit" :exit t)))

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
  (setup (:pkg consult-gh :straight t :type git :host github
               :repo "armindarvish/consult-gh" :branch "main")

    ;; TODO customize consult-gh-default-orgs-list (for M-x consult-gh-default-repos)

    (:option consult-gh-large-file-warning-threshold 2500000
             consult-gh-prioritize-local-folder 'suggest

             consult-gh-forge-timeout-seconds 12

             ;; preview gh on demand (avoids large files)
             consult-gh-show-preview t
             consult-gh-preview-key "M-o"
             ;; consult-gh-preview-buffer-mode 'org-mode ;preview in org

             ;; do things in emacs
             consult-gh-issue-action #'consult-gh--issue-view-action
             consult-gh-repo-action #'consult-gh--repo-browse-files-action
             consult-gh-file-action #'consult-gh--files-view-action)

    (with-eval-after-load 'forge
      (require 'consult-gh-forge))
    (with-eval-after-load 'embark
      (require 'consult-gh-embark)))

  (with-eval-after-load 'consult-gh
    (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
    (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)))

;;*** Sr.ht
(setup (:pkg srht)
  (:option srht-username user-mail-address))

;; (srht :type git :host github :repo "emacs-straight/srht" :files ("*" (:exclude ".git")))

;;*** Repo
;; For Google Repo
(setup (:pkg repo))

;; TODO: repo interactives/customs: repo-status, repo-init...

(provide 'dc-vcs)
