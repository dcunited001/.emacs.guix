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
;; - stores mail (archive/drafts) in gnus-..., ~/Mail
;; - fetches from NNTPSERVER or /etc/nntpserver
;; - caches state in ~/.newsrc
;; - gnus-home-directory
;;   - gnus-startup-file
;;   - gnus-init-file
;;   - gnus-directory (set to SAVEDIR if defined)
(require 'bug-reference)

;; see video for information https://www.youtube.com/watch?v=hbCXqDT1iNI
;;
;; if you have auth-sources set up for imap.gmail.com, /when gnus starts/
;;
;; then it should pull your gmail:

;;      301: INBOX
;;      302: [Gmail]/All Mail
;;      101: [Gmail]/Important
;;      317: [Gmail]/Spam
;;        0: [Gmail]/Starred

;; and pressing ^ will get you to the groups:

;; {nnfolder:archive} (opened)
;; {nndraft:} (opened)
;; {nntp:news.gmane.io} (opened)
;; {nnimap:imap.gmail.com} (opened)

;;*** Startup Files

;; don't read the newsrc file, but write to it
(setq gnus-read-newsrc-file nil
      gnus-save-newsrc-file t
      gnus-use-dribble-file t
      gnus-always-read-dribble-file t)

;;*** Group


;;*** Async
(setq gnus-asynchronous t
      gnus-use-article-prefetch 20)

;;*** Agent

;;*** Article
(setq gnus-article-over-scroll nil
      gnus-article-show-cursor t
      gnus-html-frame-width 80
      gnus-html-image-automatic-caching t
      ;; gnus-inhibit-images t
      gnus-max-image-proportion 0.7
      ;; gnus-treat-display-smileys nil
      ;; gnus-article-mode-line-format "%G %S %m"

      ;; all images in headers are outright annoying---disabled!
      gnus-article-x-face-too-ugly ".*"
      )

;; (setq gnus-visible-headers
;;       '("^From:" "^To:" "^Cc:" "^Subject:" "^Newsgroups:" "^Date:"
;;         "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
;;         "^X-Mailer:"))
;; (setq gnus-sorted-header-list gnus-visible-headers)

;;*** Group

;;*** Summary

(setup (:pkg gnus)
  ;; https://www.gnu.org/software/emacs/manual/html_mono/gnus.html#Changing-Servers
  ;; .newsrc breaks when your 'gnus-select-method changes
  ;; gnus-select-method '(nnnil)
  (:option gnus-select-method '(nnimap "imap.gmail.com")
           gnus-large-newsgroup 4000
           gnus-secondary-select-methods '(;; (nnimap "imap.gmail.com")
                                           (nntp "news.gmane.io"))
           gnus-message-archive-group "\"[Gmail]/Sent Mail\""
           ;; gnus-interactive-exit nil
           ;; gnus-novice-user nil
           ;; gnus-expert-user t
           )

  (:with-hook gnus-summary-mode-hook
    (:hook bug-reference-mode))

  (:with-hook gnus-after-getting-new-news-hook
    (:hook #'gnus-notifications)))

(setup gnus-art
  (:with-hook gnus-article-mode-hook
    (:hook bug-reference-mode)))

;;*** Notifications

;;*** Interface

;; TODO (all-the-icons-gnus :type git :flavor melpa
;;                          :host github :repo "nlamirault/all-the-icons-gnus")

;; TODO reappropriate locking from desktop.el
;; - also: https://www.emacswiki.org/emacs/PreventingMultipleGnus
(defvar gnus-lock-filename)
;; gnus--load-locked-desktop-p
;; gnus-claim-lock
;; gnus-release-lock

(with-eval-after-load 'gnus
  (dolist (mode '(gnus-group-mode-hook gnus-summary-mode-hook gnus-browse-mode-hook))
    (add-hook mode #'hl-line-mode)))

;;** Mail

(with-eval-after-load 'gnus
  (setq mail-user-agent 'message-user-agent ;default

        message-send-mail-function #'smtpmail-send-it
        message-mail-user-agent t

        mail-signature "David Conner\n"
        message-signature "David Conner\n"

        message-citation-line-function #'message-insert-formatted-citation-line
        message-citation-line-format (concat "> From: %f\n"
                                             "> Date: %a, %e %b %Y %T %z\n"
                                             ">")
        message-ignored-cited-headers ""
        ;; message-confirm-send nil
        ;; message-kill-buffer-on-exit t
        message-wide-reply-confirm-recipients t)

  (add-hook 'message-setup-hook #'message-sort-headers)

  ;; `gnus-dired' (does not require `gnus')
  (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)

  ;; send-mail-function is in the sendmail package
  (setq send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587))

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

;;** 0x0

(defun dc/0x0-set-retention-policy ()
  "Change retention for 0x0 servers"

  ;; `(,tag ,@(mapcar plist-put cfg :max-age 7))
  ;; ok you can chain the setf

  ;; TODO make this accept a plist and merge
  (cl-loop for server in 0x0-servers
           with cfg do
           (setf tag (car server)
                 cfg (cdr server)
                 cfg (plist-put cfg :max-age 7)
                 cfg (plist-put cfg :min-age 0))
           collect `(,tag ,@cfg)))

;; retention = min_age + (-max_age + min_age) * pow((file_size / max_size - 1), 3)
(setup (:pkg 0x0 :host gitlab :repo "willvaughn/emacs-0x0" :straight t))

(with-eval-after-load '0x0
  (setq 0x0-servers (dc/0x0-set-retention-policy)))

;;** Elpher

;; gopher/gemini client
(setup (:pkg elpher))

(provide 'dc-social)
