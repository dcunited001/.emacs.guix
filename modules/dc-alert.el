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

(setup notifications)

;;** Alert.el

;; main lib: https://github.com/jwiegley/alert

(setup (:pkg alert)
  (:option alert-default-style 'libnotify
           alert-log-level 'normal))

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

(provide 'dc-alert)
