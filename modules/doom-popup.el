;; -*- lexical-binding: t; -*-
;;; doom-popup.el --- Description
;;
;; Copyright © 2023 David Conner
;; Copyright © 2014-2022 Henrik Lissner.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;;; Commentary:
;;
;; Borrows heavily if not literally from Doom Emacs' +popup modules
;;
;; Not sure how to attribute copyrights for dotfiles/config.  It wouldn't
;; usually qualify as fair use.  When including software by package/module then
;; attributing is simple, I feel like if you solve the piecewise dotfiles/config
;; reference problem -- without being overwhelmed by mileau -- then your SBOM or
;; software supply chain solution could probably survive anything.
;;
;; These are dotfiles.  I'm configuring a personal system to behave according to
;; preferable logic.  Whenever I get to actually fucking programming, that's
;; different.
;;
;;; Code:

;;* Doom Popups

;;** Config

(defvar +popup--internal nil)
(defvar +popup--display-buffer-alist nil)

(defvar +popup-default-display-buffer-actions
  '(+popup-display-buffer-stacked-side-window-fn)
  "The functions to use to display the popup buffer.")

(defvar +popup-defaults
  (list :side 'bottom
        :height 0.16
        :width 40
        :quit t
        :select #'ignore
        :ttl 5))

;; (+popup-make-rule "\\^ fsdfdsa" (list :side 'bottom
;;                         :height 0.16
;;                         :width 40
;;                         :quit t
;;                         :select #'ignore
;;                         :ttl 5))


;; (or (cdr (assq 'actions alist))
;;                       +popup-default-display-buffer-actions)

(defun +popup-make-rule (predicate plist)
  (if (plist-get plist :ignore)
      (list predicate nil)
    (let* ((plist (append plist +popup-defaults))
           (alist
            `((actions . ,(plist-get plist :actions))
              (side . ,(plist-get plist :side))
              (size . ,(plist-get plist :size))
              (window-width . ,(plist-get plist :width))
              (window-height . ,(plist-get plist :height))
              (slot . ,(plist-get plist :slot))
              (vslot . ,(plist-get plist :vslot))))
           (params
            `((ttl . ,(plist-get plist :ttl))
              (quit . ,(plist-get plist :quit))
              (select . ,(plist-get plist :select))
              (modeline . ,(plist-get plist :modeline))
              (autosave . ,(plist-get plist :autosave))
              ,@(plist-get plist :parameters))))
      `(,predicate (+popup-buffer)
                   ,@alist
                   (window-parameters ,@params)))))

(defun +popup-parameter (parameter &optional window)
  "Fetch the window PARAMETER (symbol) of WINDOW"
  (window-paramter (or window (selected-window)) parameter))

(defun +popup-parameter-fn (parameter &optional window &rest args)
  "Fetch the window PARAMETER (symbol) of WINDOW. If it is a
function, run it with ARGS to get its return value."
  (let ((val (+popup-parameter parameter window)))
    (if (functionp val)
        (apply val args)
      val)))

;;*** Config Util

(defun +popup-cleanup-rules-h ()
  "Cleans up any duplicate popup rules."
  (interactive)
  (setq +popup--display-buffer-alist
        (cl-delete-duplicates +popup--display-buffer-alist
                              :key #'car :test #'equal :from-end t))
  (when +popup-mode
    (setq display-buffer-alist +popup--display-buffer-alist)))


;;** UI

(defun +popup-shink-to-fit (&optional window)
  "Shrinks WINDOW to fit the buffer contents, if the buffer isn't empty."
  (unless window (setq window (selected-window)))
  (unless (= (- (point-max) (point-min)) 0)
    (shrink-window-if-larger-than-buffer window)))

;;*** UI Util

(defun +popup-alist-from-window-state (state)
  "Convert window STATE (from `window-state-get') to a `display-buffer' alist."
  (let* ((params (alist-get 'parameters state)))
    `((side          . ,(alist-get 'window-side params))
      (window-width  . ,(alist-get 'total-width state))
      (window-height . ,(alist-get 'total-height state))
      (window-parameters ,@params))))


;;*** UI Actions

(defun +popup-display-buffer-stacked-side-window-fn (buffer alist)
  "A `display-buffer' action that serves as an alternative to
`display-buffer-in-side-window', but allows for stacking popups with the `vslot'
alist entry.

Accepts the same arguments as `display-buffer-in-side-window'. You must set
`window--sides-inhibit-check' to non-nil for this work properly."
  (let* ((side  (or (cdr (assq 'side alist)) 'bottom))
         (slot  (or (cdr (assq 'slot alist))  0))
         (vslot (or (cdr (assq 'vslot alist)) 0))
         (left-or-right (memq side '(left right)))
         (display-buffer-mark-dedicated (or display-buffer-mark-dedicated 'popup)))

    (cond ((not (memq side '(top bottom left right)))
           (error "Invalid side %s specified" side))
          ((not (numberp slot))
           (error "Invalid slot %s specified" slot))
          ((not (numberp vslot))
           (error "Invalid vslot %s specified" vslot)))

    (let* ((major (get-window-with-predicate
                   (lambda (window)
                     (and (eq (window-parameter window 'window-side) side)
                          (eq (window-parameter window 'window-vslot) vslot)))
                   nil))
           (reversed (window--sides-reverse-on-frame-p (selected-frame)))
           (windows
            (cond ((window-live-p major)
                   (list major))
                  ((window-valid-p major)
                   (let* ((first (window-child major))
                          (next (window-next-sibling first))
                          (windows (list next first)))
                     (setq reversed (> (window-parameter first 'window-slot)
                                       (window-parameter next 'window-slot)))
                     (while (setq next (window-next-sibling next))
                       (setq windows (cons next windows)))
                     (if reversed windows (nreverse windows))))))
           (slots (if major (max 1 (window-child-count major))))
           (max-slots
            (nth (plist-get '(left 0 top 1 right 2 bottom 3) side)
                 window-sides-slots))
           (window--sides-inhibit-check t)
           window this-window this-slot prev-window next-window
           best-window best-slot abs-slot)

      (cond ((and (numberp max-slots) (<= max-slots 0))
             nil)
            ((not windows)
             (cl-letf (((symbol-function 'window--make-major-side-window-next-to)
                        (lambda (_side) (frame-root-window (selected-frame)))))
               (when-let (window (window--make-major-side-window buffer side slot alist))
                 (set-window-parameter window 'window-vslot vslot)
                 (add-to-list 'window-persistent-parameters '(window-vslot . writable))
                 window)))
            (t
             ;; Scan windows on SIDE.
             (catch 'found
               (dolist (window windows)
                 (setq this-slot (window-parameter window 'window-slot))
                 (cond ((not (numberp this-slot)))
                       ((= this-slot slot) ; A window with a matching slot found
                        (setq this-window window)
                        (throw 'found t))
                       (t
                        ;; Check if this window has a better slot value wrt the
                        ;; slot of the window we want.
                        (setq abs-slot
                              (if (or (and (> this-slot 0) (> slot 0))
                                      (and (< this-slot 0) (< slot 0)))
                                  (abs (- slot this-slot))
                                (+ (abs slot) (abs this-slot))))
                        (unless (and best-slot (<= best-slot abs-slot))
                          (setq best-window window)
                          (setq best-slot abs-slot))
                        (if reversed
                            (cond
                             ((<= this-slot slot)
                              (setq next-window window))
                             ((not prev-window)
                              (setq prev-window window)))
                          (cond
                           ((<= this-slot slot)
                            (setq prev-window window))
                           ((not next-window)
                            (setq next-window window))))))))

             ;; `this-window' is the first window with the same SLOT.
             ;; `prev-window' is the window with the largest slot < SLOT. A new
             ;; window will be created after it.
             ;; `next-window' is the window with the smallest slot > SLOT. A new
             ;; window will be created before it.
             ;; `best-window' is the window with the smallest absolute
             ;; difference of its slot and SLOT.
             (or (and this-window
                      ;; Reuse `this-window'.
                      (with-current-buffer buffer
                        (setq window--sides-shown t))
                      (window--display-buffer
                       buffer this-window 'reuse alist))
                 (and (or (not max-slots) (< slots max-slots))
                      (or (and next-window
                               ;; Make new window before `next-window'.
                               (let ((next-side (if left-or-right 'above 'left))
                                     (+popup--internal t)
                                     (window-combination-resize 'side))
                                 (setq window
                                       (ignore-errors (split-window next-window nil next-side)))))
                          (and prev-window
                               ;; Make new window after `prev-window'.
                               (let ((prev-side (if left-or-right 'below 'right))
                                     (+popup--internal t)
                                     (window-combination-resize 'side))
                                 (setq window
                                       (ignore-errors (split-window prev-window nil prev-side))))))
                      (set-window-parameter window 'window-slot slot)
                      (with-current-buffer buffer
                        (setq window--sides-shown t))
                      (window--display-buffer
                       buffer window 'window alist))
                 (and best-window
                      ;; Reuse `best-window'.
                      (progn
                        ;; Give best-window the new slot value.
                        (set-window-parameter best-window 'window-slot slot)
                        (with-current-buffer buffer
                          (setq window--sides-shown t))
                        (window--display-buffer
                         buffer best-window 'reuse alist)))))))))

(provide 'doom-popup)
