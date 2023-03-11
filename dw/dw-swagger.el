;; -*- lexical-binding: t; -*-
;;
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

(require 'json)
(require 'subr-x)

(defun dw/swagger-get-actions (path)
  (string-join
   (mapcar (lambda (action)
             (upcase (format "%s" (car action))))
           path)
   ", "))

;;;###autoload
(defun dw/swagger-extract-paths ()
  "Extract URI paths and HTTP actions from current buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((paths (mapcar (lambda (path)
                           (format "- %s [%s]"
                                   (car path)
                                   (dw/swagger-get-actions (cdr path))))
                         (alist-get 'paths (json-read)))))
      (with-current-buffer (get-buffer-create "*swagger-extract*")
        (goto-char (point-min))
        (delete-region (point-min) (point-max))
        (insert (string-join paths "\n"))
        (switch-to-buffer (current-buffer))))))

;; (progn
;;   (with-current-buffer "metricsadvisor.json"
;;     (dw/swagger-extract-paths)))

(provide 'dw-swagger)
