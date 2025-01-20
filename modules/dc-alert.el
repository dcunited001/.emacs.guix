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

;;* Alert

;;** Notifications

;; emacs built with d-bus and its socket are required

(use-package notifications :straight (:type built-in))

;;*** Straight.el

;; TODO: maybe use alerts instead
(defun dc/reminder-straight-fetch-on-action (id key)
  (when (and (equal key "update")
             (y-or-n-p "Update `straight.el'? "))
    (progn (straight-fetch-all)
           (notifications-notify
            :title "Emacs: straight.el"
            :body "Fetch complete.")))
  (remove-hook 'server-after-make-frame-hook #'dc/reminder-straight-fetch))

(defun dc/reminder-straight-fetch ()
  (notifications-notify
   :title "Emacs: straight.el"
   :body "Repositories last updated %s. Update now?"
   :actions '("update" "Update")
   :on-action #'dc/reminder-straight-fetch-on-action))

(when DBUS_FOUND
  ;; emacs-startup-hook: too early
  ;; server-after-make-frame-hook: works if server first loaded on desktop
  (add-hook 'server-after-make-frame-hook #'dc/reminder-straight-fetch))


;;*** Package

;; IDK why it's not loading

(defvar dc/reminder-packages-to-check '(casual-isearch sesman))

(defun dc/reminder-packages-not-loaded ()
  (when-let ((pkg-syms (bound-and-true-p dc/reminder-packages-to-check))
             (unloaded-syms (seq-filter (lambda (pkg) (not (featurep pkg))) pkg-syms))
             (unloaded (mapcar #'symbol-name unloaded-syms)))
    (message (pp-to-string unloaded))
    (notifications-notify
     :title (format "Emacs: Packages not loaded.")
     :body (format "These packages should load, but did not. %s\n\n"
                   (string-join unloaded " "))))
  (remove-hook 'server-after-make-frame-hook #'dc/reminder-packages-not-loaded))

;; with depth 25
(add-hook 'server-after-make-frame-hook #'dc/reminder-packages-not-loaded 25)

;;** Alert.el

;; main lib: https://github.com/jwiegley/alert

(use-package alert :straight t
  :custom
  ((alert-default-style 'libnotify)
   (alert-log-level 'normal)))

;; alert-styles
;; 'notifications: bundled with emacs to be OS/WM-agnostic compatible with all
;; 'libnotify: works when notify-send is on the system
;; 'gntp: IRC bots https://github.com/tekai/gntp.el
;; 'fringe: change fringe color
;; 'modeline: change background color
;; 'custom: use (alert-define-style ...)

;; NOTE: alert-add-rule adds to the alert-internal-configuration
;; whereas modifying alert-user-configuration is separate

;; the alert-style is dispatched on the plist values in the rules, so
;; configuring things like category or mode is helpful

(alert "Loading interface" :title "Emacs:")

(provide 'dc-alert)
