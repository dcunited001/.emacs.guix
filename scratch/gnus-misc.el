



;; ==================================================================

;;** Scan Imap Unless Disconnected


;;*** TODO impl with predicate

;; TODO implement predicate (this needs to be a solid function reference)
;; (defun dc/gnus-demon-scan-mail-unless (servers &optional pred))

;;*** second impl

;; this would attempt to pass a partial, but depending on the status of the
;; server (which can't be known in advance), then comparing the
;; `gnus-opened-servers' requires a custom comparator

;; here the order in which the args are called matters
;; it changes the outcome of ((car a) b)
(seq-intersection
 '((nnimap "imap.gmail.com"))
 gnus-opened-servers
 (lambda (a b)
   (message "%s\n%s\n\n"
            (pp-to-string a)
            (pp-to-string b))
   (equal (car a)
          b)))

;; that allowed the specification of '((nnimap "imap.gmail.com"))
;; but it's precarious/brittle

(seq-intersection
 '(((nnimap "imap.gmail.com") notastatus))
 gnus-opened-servers
 (lambda (a b)
   (message "%s\n%s\n\n"
            (pp-to-string a)
            (pp-to-string b))
   (equal (car a)
          (car b))))


;; add by using partial application, which makes it a bit unwiedly to
;; add/remove dynamically (maybe use defalias)


(defun dc/gnus-demon-scan-mail-unless-closed (servers)
  (save-window-excursion
    (let ((servers (seq-intersection gnus-opened-servers servers
                                     (lambda (a b) (eq (car a) (car b))))))
      server
      (nnmail-fetched-sources (list t))
      (while (setq server (car (pop servers)))
        (and (gnus-check-backend-function 'request-scan (car server))
             (eq 'nnimap (car server))
             ;; exits early if the server is closed
             (not (eq 'closed (gnus-server-status server)))
             (message "Reading mail from %s" (pp-to-string server))
	           (or (gnus-server-opened server)
		             (gnus-open-server server))
	           (gnus-request-scan nil server))))))


;; gnus-demon-init loops through handlers and hooks them on a timer,
;; calling run-with[-idle]-timer, which runs apply
;;   (apply #'run-at-time secs repeat function args)

;; unfortunately, you must supply a list at the end, which gets splatted
(apply (apply-partially #'+ 1 2 3) '())
(apply (apply-partially #'+ 1 2 3) 1 2 3 '(1 2 3)) ; calling with empty args req. list

(let ((nnimap-methods (list (list gnus-select-method))))

  ;; (gnus-demon-add-handler
  ;;  (apply-partially #'dc/gnus-demon-scan-mail-unless-closed
  ;;                   nnimap-methods) 10 nil)

  (funcall (apply-partially #'dc/gnus-demon-scan-mail-unless-closed
                            nnimap-methods)))



;;*** First impl.

;; read mail every ten minutes unless disconnected from server
;;  with some debugging output

(defun dc/gnus-demon-scan-mail-unless-closed ()
  (save-window-excursion
    (let ((servers gnus-opened-servers)
	        server
	        (nnmail-fetched-sources (list t)))
      (while (setq server (car (pop servers)))
        ;; checks that the backend has 'request-scan
	      (and (gnus-check-backend-function 'request-scan (car server))
             (eq 'nnimap (car server))
             ;; exits early if the server is closed
             (not (eq 'closed (gnus-server-status server)))
             (message "Reading mail from %s" (pp-to-string server))
	           (or (gnus-server-opened server)
		             (gnus-open-server server))
	           (gnus-request-scan nil server))))))

;; ==================================================================

(defun dc/mapcar-nnimap-process-buffers (f)
  (thread-last
    nnimap-connection-alist
    (mapcar #'cdr)
    (mapcar f)))

;; returns a list of nntp-server-buffers ... ? process buffers? no?
;; (dc/mapcar-nnimap-servers #'bufferp) ;nope
(dc/mapcar-nnimap-process-buffers #'nnimap-buffer)

(setq dc/testimap (nth 0 nnimap-connection-alist))

dc/testimap ;; (#<buffer  *nntpd*> #<buffer  *nnimap imap.gmail.com nil  *nntpd**>)

;; ==================================================================
