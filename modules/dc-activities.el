;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2024 David Conner
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


;; (activities-current) is a cl-struct
;; (cl-struct-p (activities-current))
;; (cl-struct-p (activities-activity-last (activities-current)))

(defun dc/pp-activity (activity)
  (let ((name (activities-activity-name activity))
        (last-saved (activities-activity-last activity))
        (default-saved (activities-activity-default activity)))
    (pp--insert-lisp (list name
                           last-saved
                           default-saved))))


(defmacro dc/make-activity (name frame &rest body)
  (let ((need-temp-frame (null frame))
        (frame (or frame (make-frame))))
    `(progn
       ;; returns the last form in body
       (with-selected-frame ,frame ,@body
                            (activities-new ,name :forcep t))
       (when need-temp-frame (delete-frame ,frame))
       (activities-named ,name))))

;; the easiest way to do this:
;; - always split right, only split 'below in the last (left-most) window
;; - otherwise, split left, before splitting below
;; - open buffers/files before and between splits
;;
;; and generally, no window configuration is superior to the simplicity of 2
;; windows (one left, one right)
;;
;; (current-window-definition) gets the object for the current frame

;; (dc/make-activity
;;  "REPO"
;;  nil
;;  (find-file "/data/repo/x.files")
;;  (split-window nil nil 'right)
;;  (find-file "/data/repo")
;;  (split-window nil nil 'left)
;;  (magit-status)
;;  (split-window nil nil 'below))

(provide dc-activities)


