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

;; * Desktop

;;** Desktop Mode

;; this module was originally intended for XDG desktop integration
(straight-use-package '(desktop :type built-in))

;; i'll probably need to revisit this to ensure it works as expected
;; https://emacs.stackexchange.com/questions/31621/handle-stale-desktop-lock-files-after-emacs-system-crash
(defun dc/desktop-read ()
  "Restore an emacs desktop session. Open "
  (interactive)
  (let* ((desktop-process (process-attributes (or (desktop-owner) 0)))
         (desktop-process-exec (alist-get 'comm desktop-process))
         (desktop-dirname (expand-file-name "desktop" no-littering-var-directory))
         (desktop-regexp (rx string-start (* (any ascii)) "emacs")))

    (unless (and desktop-process-exec
                 (string-match desktop-regexp desktop-process-exec))
      (message "Deleting desktop owned by pid %s" (desktop-owner))
      (delete-file (desktop-full-lock-name))))
  (desktop-read))

;;** Dashboard

;; ䷅ -> ䷃
;; 23 -> 4 (oh no!)
(setup (:pkg i-ching :straight t :type git :flavor melpa
             :host github :repo "zzkt/i-ching")
  (:option i-ching-randomness-source 'pseudo
           i-ching-divination-method 'yarrow-stalks))

;;** Opening Files

(setup (:pkg openwith)
  (require 'openwith)
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mpv"
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
               ;; causing feh to be opened...
               "feh"
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf"))
               "zathura"
               '(file)))))

(provide 'dc-desktop)

;; TODO: encountering a wierd issue with emacs-29 (pgtk, i think) where desktops
;; are being restored in an invalid state and causing crashes later .... or
;; something? see org-roam
