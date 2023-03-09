;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2023 David Conner
;; Copyright © 2014-2022 Henrik Lissner.
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

;;* Doom Org Roam

(defun org-roam-link-follow-link-with-description-a (args)
  "Use a 'roam:X' link's description if X is empty."
  (when (or (string-empty-p (car args))
            (null (car args)))
    (setcar
     args (let ((link (org-element-context)))
            (and (org-element-property :contents-begin link)
                 (org-element-property :contents-end link)
                 (buffer-substring-no-properties
                  (org-element-property :contents-begin link)
                  (org-element-property :contents-end link))))))
  args)

(defun org-roam-link-replace-at-point-a (&optional link)
  "Replace \"roam:\" LINK at point with an \"id:\" link."
  (save-excursion
    (save-match-data
      (let* ((link (or link (org-element-context)))
             (type (org-element-property :type link))
             (path (org-element-property :path link))
             (desc (and (org-element-property :contents-begin link)
                        (org-element-property :contents-end link)
                        (buffer-substring-no-properties
                         (org-element-property :contents-begin link)
                         (org-element-property :contents-end link))))
             node)
        (goto-char (org-element-property :begin link))
        (when (and (org-in-regexp org-link-any-re 1)
                   (string-equal type "roam")
                   ;; Added `desc' ref here
                   (setq node (save-match-data
                                (org-roam-node-from-title-or-alias
                                 (if (string-empty-p path)
                                     desc
                                   path)))))
          (replace-match (org-link-make-string
                          (concat "id:" (org-roam-node-id node))
                          (or desc path))))))))

(provide 'doom-org-roam2)
