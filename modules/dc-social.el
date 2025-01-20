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

;;** Newsticker

;; what the fuck? not bad. lisp code ages well (the manual last updated in 2004)

;; newsticker.el.gz
;; newst-reader.el.gz
;; newst-ticker.el.gz
;; newst-backend.el.gz
;; newst-treeview.el.gz
;; newst-plainview.el.gz

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

;;*** Setup

(setup (:pkg gnus)
  ;; https://www.gnu.org/software/emacs/manual/html_mono/gnus.html#Changing-Servers
  ;; .newsrc breaks when your 'gnus-select-method changes
  ;; gnus-select-method '(nnnil)
  (:option gnus-select-method '(nnimap "imap.gmail.com")
           gnus-large-newsgroup 4000
           gnus-secondary-select-methods '(;; (nnimap "imap.gmail.com")
                                           (nntp "news.gmane.io"))
           gnus-message-archive-group "\"[Gmail]/Sent Mail\"")

  ;; gnus-interactive-exit nil
  ;; gnus-novice-user nil
  ;; gnus-expert-user t
  (:with-hook gnus-group-mode-hook
    (:hook gnus-topic-mode))
  (:with-hook gnus-summary-mode-hook
    (:hook bug-reference-mode))
  (:with-hook gnus-after-getting-new-news-hook
    (:hook #'gnus-notifications)))

(setup gnus-art
  (:with-hook gnus-article-mode-hook
    (:hook bug-reference-mode)))

;;*** Startup

;; don't read the newsrc file, but write to it
(setq gnus-read-newsrc-file nil
      gnus-save-newsrc-file t
      gnus-use-dribble-file t
      gnus-always-read-dribble-file t)

;;**** Lock Files

;; TODO reappropriate locking from desktop.el
;; - also: https://www.emacswiki.org/emacs/PreventingMultipleGnus
(defvar gnus-lock-filename)

;; gnus--load-locked-desktop-p
;; gnus-claim-lock
;; gnus-release-lock

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

;; open summary buffer ─► seen
;; open article ─► read
;; hit ! to tick ─► ticked
;;

;; | n-ticked | n~untouched | n-read |  ;;;  n-unseen
;; M: marked B: summary-open | S: subscr, %L sub-level
(setq gnus-group-line-format "%M%B%p│%-42,42c│%3T│%4I│%7y│%L%S %d\n"
      gnus-group-uncollapsed-levels 3
      gnus-level-subscribed 6
      gnus-level-default-subscribed 4)

;;**** Topics

;; "%i└─♦ %(%{%n%}%) %gg:%a (%G:%A) ]%v\n"
;; "%i╚═♦ %(%{%n%}%) »───« %a in %g »───« %G in %A »───♦  %v\n"

;; needs too many hacks, may not work
;;
;; dc/gnus-topic-indent-max (* 5 gnus-topic-indent-level)
;;
;; (concat "%i└─•─"
;;         "%4,4~(form (make-string dc/gnus-topic-indent-max (string-to-char \"─\")))@"
;;         "« %(%{%n%}%) »───« %G in %A »───♦  %v\n")
(setq gnus-topic-indent-level 2
      gnus-topic-line-format
      (concat "%i└─•────« %(%{%n%}%) »───« %G%v »───♦ %-56=%7A \n"))

;;*** Server

;; the %h formatter needs type conversion to set the length
(setq gnus-server-line-format "%-8,8s │ %-8,8a │ %-14,14c | %-16,16w %h\n")
;; (dc/gnus-format-alist-translate gnus-server-line-format-alist)

;;*** Summary

(setq gnus-summary-line-format "%U%R%z %d %B%-3L %[%f%] %s\n"
      gnus-sum-thread-tree-root            "☼──► "
      gnus-sum-thread-tree-false-root      "○┬─► "
      gnus-sum-thread-tree-vertical        "│"
      gnus-sum-thread-tree-leaf-with-other "├•─► "
      gnus-sum-thread-tree-single-leaf     "└•─► "
      gnus-sum-thread-tree-indent          " "
      gnus-sum-thread-tree-single-indent   "■    "
      gnus-summary-newsgroup-prefix        "⇒ "
      gnus-summary-to-prefix               "→ ")

;;*** Interface

;; gnus-buffer-configuration

;;**** Gnus Visual

;; variable: gnus-visual

;;**** Window Layout

;; definitely, definitely use the trees
(setq gnus-use-trees t
      gnus-generate-tree-function 'gnus-generate-horizontal-tree)


;; use gnus-add-configuration to modify gnus-buffer-configuration (must run
;; after gnus starts)

;;**** Gravatars

;; retro, identicon, robohash, wavatar, monsterid
;; set to "404" to disable
(setq gravatar-default-image "robohash")

(setup gnus-gravatar
  ;; gnus-gravatar-size 32
  ;; gnus-gravatar-too-ugly "toougly@fugly.com"
  (:option gnus-treat-from-gravatar 'head
           gnus-treat-mail-gravatar 'head))

;;**** Notifications


;; TODO (all-the-icons-gnus :type git :flavor melpa
;;                          :host github :repo "nlamirault/all-the-icons-gnus")

(with-eval-after-load 'gnus
  (dolist (mode '(gnus-group-mode-hook gnus-summary-mode-hook gnus-browse-mode-hook))
    (add-hook mode #'hl-line-mode)))

;; https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org#make-all-mails-visible-important

(with-eval-after-load 'gnus
  ;; @see https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
  ;; gnus-group-mode
  (eval-after-load 'gnus-group
    '(progn
       (defhydra hydra-gnus-group (:color blue)
         "
[_A_] Remote groups (A A) [_g_] Refresh
[_L_] Local groups        [_\\^_] List servers
[_c_] Mark all read       [_m_] Compose new mail
[_#_] Mark mail
"

         ("A" gnus-group-list-active)
         ("L" gnus-group-list-all-groups)
         ("c" gnus-topic-catchup-articles)
         ;; ("G" dianyou-group-make-nnir-group) ;; [_G_] Search mails (G G)   from gnus
         ("g" gnus-group-get-new-news)
         ("^" gnus-group-enter-server-mode)
         ("m" gnus-group-new-mail)
         ("#" gnus-topic-mark-topic)
         ("q" nil))
       ;; y is not used by default
       (define-key gnus-group-mode-map "y" 'hydra-gnus-group/body)))

  ;; gnus-summary-mode
  (eval-after-load 'gnus-sum
    '(progn
       (defhydra hydra-gnus-summary (:color blue)
         "
[_s_] Show thread   [_F_] Forward (C-c C-f)
[_h_] Hide thread   [_e_] Resend (S D e)
[_n_] Refresh (/ N) [_r_] Reply
[_!_] Mail -> disk  [_R_] Reply with original
[_d_] Disk -> mail  [_w_] Reply all (S w)
[_c_] Read all      [_W_] Reply all with original (S W)
[_#_] Mark
"
         ("s" gnus-summary-show-thread)
         ("h" gnus-summary-hide-thread)
         ("n" gnus-summary-insert-new-articles)
         ("F" gnus-summary-mail-forward)
         ("!" gnus-summary-tick-article-forward)
         ("d" gnus-summary-put-mark-as-read-next)
         ("c" gnus-summary-catchup-and-exit)
         ("e" gnus-summary-resend-message-edit)
         ("R" gnus-summary-reply-with-original)
         ("r" gnus-summary-reply)
         ("W" gnus-summary-wide-reply-with-original)
         ("w" gnus-summary-wide-reply)
         ("#" gnus-topic-mark-topic)
         ;; ("G" dianyou-group-make-nnir-group) [_G_] Search mails
         ("q" nil))
       ;; y is not used by default
       (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body)))

  ;; gnus-article-mode
  (eval-after-load 'gnus-art
    '(progn
       (defhydra hydra-gnus-article (:color blue)
         "
[_F_] Forward           [_o_] Save attachment
[_r_] Reply
[_R_] Reply with original
[_w_] Reply all (S w)
[_W_] Reply all with original (S W)

"
         ("F" gnus-summary-mail-forward)
         ("r" gnus-article-reply)
         ("R" gnus-article-reply-with-original)
         ("w" gnus-article-wide-reply)
         ("W" gnus-article-wide-reply-with-original)
         ("o" gnus-mime-save-part)
         ;; ("v" my-w3m-open-with-mplayer)
         ;; ("d" my-w3m-download-rss-stream)
         ;; ("b" my-w3m-open-link-or-image-or-url)
         ;; ("f" w3m-lnum-follow)
         ;; ("g" w3m-lnum-goto)
         ("q" nil))
       ;; y is not used by default
       (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body)))

  ;; message-mode
  (eval-after-load 'message
    '(progn
       (defhydra hydra-message (:color blue)
         "
[_c_] Complete mail address
[_a_] Attach file
[_s_] Send mail (C-c C-c)
"
         ;; ("c" counsel-bbdb-complete-mail)
         ("a" mml-attach-file)
         ("s" message-send-and-exit)
         ;; ("i" dianyou-insert-email-address-from-received-mails)
         ("q" nil))))

  (defun message-mode-hook-hydra-setup ()
    (local-set-key (kbd "C-c C-y") 'hydra-message/body))

  (add-hook 'message-mode-hook 'message-mode-hook-hydra-setup))

;;** Gnus Support

;; TODO update to translate the types (these are chars like \s \b and \n
(defun dc/gnus-format-alist-translate (alist)
  (mapcar (lambda (a)
            (let ((key (if (numberp (car a))
                           (char-to-string (car a))
                         (concat "&" (symbol-name (car a))))))
              (cons (concat "%" key) (cdr a)))) alist))


;;*** Demons

;;**** Support

(defun dc/gnus-demon-scan-mail-unless-closed (servers)
  (save-window-excursion
    (let (server
          (nnmail-fetched-sources (list t))
          (servers (seq-intersection
                    gnus-opened-servers servers
                    (lambda (a b) (eq (car a) (car b))))))
      (while (setq server (car (pop servers)))
        (and (gnus-check-backend-function 'request-scan (car server))
             (eq 'nnimap (car server))
             ;; exits early if the server is closed
             (not (eq 'closed (gnus-server-status server)))
             (message "Reading mail from %s" (pp-to-string server))
	           (or (gnus-server-opened server)
		             (gnus-open-server server))
	           (gnus-request-scan nil server))))))

;;**** Handlers

(defun dc/gnus-demon-setup ()

  ;; close connections after emacs is idle for 30 minutes
  (gnus-demon-add-handler
   'gnus-demon-close-connections nil 30)

  ;; assuming the imap server is specified by gnus-select-method at
  ;; gnus-startup.
  (let ((nnimap-methods (list (list gnus-select-method))))

    ;; this still may not work
    (gnus-demon-add-handler
     (apply-partially #'dc/gnus-demon-scan-mail-unless-closed
                      nnimap-methods) 10 nil))

  (gnus-demon-init))

;; this will reopen the server even after it's been disconnected.
;; (gnus-demon-add-handler
;;  'gnus-demon-scan-mail 10 nil)

(add-hook 'gnus-started-hook #'dc/gnus-demon-setup)


;;** Mail

;; potentially problematic sendmail options (configure later)
;; (setq mail-specify-envelope-from t
;;       message-sendmail-envelope-from 'header
;;       mail-envelope-from 'header)

;; req action to unfold threads (maybe disable this)
(setq gnus-thread-hide-subtree t)

(with-eval-after-load 'gnus
  ;; TODO: setq-default?
  (setq mail-user-agent 'message-user-agent ;default

        message-send-mail-function #'smtpmail-send-it
        message-mail-user-agent t

        mail-signature "David Conner\n"
        message-signature "David Conner\n"

        mm-discouraged-alternatives '("text/html" "text/richtext")

        ;; this needs to be set after loading gnus
        mm-automatic-display (remove "text/html" mm-automatic-display)

        message-citation-line-function #'message-insert-formatted-citation-line
        message-citation-line-format (concat "> From: %f\n"
                                             "> Date: %a, %e %b %Y %T %z\n"
                                             ">")
        message-ignored-cited-headers ""
        ;; message-confirm-send nil
        message-kill-buffer-on-exit t

        message-wide-reply-confirm-recipients t)

  (add-hook 'message-setup-hook #'message-sort-headers)
  (add-hook 'message-setup-hook (lambda () (auto-fill-mode -1)))

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

;; The emacs package(s) have a creative commons license and there are two
;; different versions. they have incompatible interfaces.

;; sr.ht/~pkal/nullpointer-emacs
;; sr.ht/~willvaughn/emacs-0x0

;; retention = min_age + (-max_age + min_age) * pow((file_size / max_size - 1), 3)
(setup (:pkg 0x0 :straight t :host sourcehut :repo "willvaughn/emacs-0x0"))

(with-eval-after-load '0x0
  (require 'dc-0x0)                       ; in ~/.emacs.d/lisp
  (setq 0x0-servers (dc/0x0-set-retention-policy)))

;;** Elpher

;; gopher/gemini client
(setup (:pkg elpher))

(provide 'dc-social)
