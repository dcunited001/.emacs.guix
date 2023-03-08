;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(require 'fsm)

;;* FSM

;; (setq foob (defun foo () 'bar))
;; (funcall (symbol-value 'foob))
;; (funcall foob)

;;** CL Macros
;; \(fn ((NAME ARGLIST BODY...) ...) FORM...)

(cl-macrolet ((zow (next timeout)
                `(progn (pp (list (cl-incf count) event))
                        ;; (hey (cl-incf count) event)
                        ;; (list ,next count ,timeout)
                        )))
  (let ((count 7)
        (event ":event"))
    (zow :pingg 0.1)))

;;** Define FSM

(cl-labels ((hey (n ev)
              (message "%d (%s)\tp%sn%s!" n ev
                       (if (zerop (% n 4)) "o" "i")
                       (make-string (max 1 (abs n)) ?g))))

  ;; zow becomes a macro that ...
  (cl-macrolet ((zow (next timeout)
                  `(progn
                     ;; ... calls hey with args:
                     (hey (cl-incf count) event)
                     ;; then returns '(next-state state-data and timeout)
                     (list ,next count ,timeout))))

   (define-fsm pingpong
               :start ((init) "Start a pingpong fsm."
                       (interactive "nInit (number, negative to auto-terminate): ")
                       (list :ping (ash (ash init -2) 2) ; 4 is death
                             (when (interactive-p) 0)))
               :state-data-name count
               :states
               ((:ping
                 (:event (zow :pingg 0.1)))
                (:pingg
                 (:event (zow :pinggg 0.1)))
                (:pinggg
                 (:event (zow :pong 1)))
                (:pong
                 (:event (zow :ping (if (= 0 count)
                                        (fsm-goodbye-cruel-world 'pingpong)
                                      3))))))))

;; (cl-lables (...) (cl-macrolet (...) (define-fsm fsm-name ...)))
;; - this returns a (closure ((...)))
;; - with define-fsm's output bound to fcell 'start-pingpont

;; (lambda (fsm count event callback)
;;   (progn
;;     (funcall
;;      --cl-hey--
;;      (setq count (1+ count))
;;      event)
;;     (list
;;      :ping count
;;      (if (= 0 count)
;;          (fsm-goodbye-cruel-world
;;           'pingpong)
;;        3))))

(functionp 'start-pingpong)             ;t
(symbol-function 'start-pingpong)

;; (lambda (init)
;;   "Start a pingpong fsm."
;;   (interactive "nInit (number, negative to auto-terminate): ")
;;   (fsm-debug-output
;;    "Starting %s"
;;    'pingpong)
;;   (let ((fsm (cl-gensym
;;               (concat "fsm-" "pingpong" "-"))))
;;     (let* ((--cl-rest-- (progn
;;                           (list
;;                            :ping (ash (ash init -2) 2)
;;                            (if (interactive-p) (progn 0)))))
;;            (state (if (cdr --cl-rest--)
;;                       (car-safe
;;                        (prog1
;;                            --cl-rest--
;;                          (setq --cl-rest--
;;                                (cdr --cl-rest--))))
;;                     (signal
;;                      'wrong-number-of-arguments
;;                      (list
;;                       '(state
;;                         state-data
;;                         &optional
;;                         timeout)
;;                       (length --cl-rest--)))))
;;            (state-data (car-safe
;;                         (prog1
;;                             --cl-rest--
;;                           (setq --cl-rest--
;;                                 (cdr --cl-rest--)))))
;;            (timeout (car-safe
;;                      (prog1
;;                          --cl-rest--
;;                        (setq --cl-rest--
;;                              (cdr --cl-rest--))))))
;;       (progn
;;         (if --cl-rest--
;;             (signal
;;              'wrong-number-of-arguments
;;              (list
;;               '(state
;;                 state-data
;;                 &optional
;;                 timeout)
;;               (+ 3 (length --cl-rest--)))))
;;         (put fsm :name 'pingpong)
;;         (put fsm :state nil)
;;         (put fsm :state-data nil)
;;         (put
;;          fsm
;;          :sleep #'(lambda (secs)
;;                     (accept-process-output
;;                      nil
;;                      secs)))
;;         (put fsm :deferred nil)
;;         (fsm-update
;;          fsm
;;          state
;;          state-data
;;          timeout)
;;         fsm))))

;;** Start FSM
(fsm-send (start-pingpong -16) t)

;; -15 (t)	pinggggggggggggggg!
;; -14 (:timeout)	pingggggggggggggg!
;; -13 (:timeout)	pinggggggggggggg!
;; -12 (:timeout)	pongggggggggggg!
;; nil [2 times]
;; -11 (:timeout)	pinggggggggggg!
;; -10 (:timeout)	pingggggggggg!
;; -9 (:timeout)	pinggggggggg!
;; -8 (:timeout)	pongggggggg!
;; Reverting buffer ‘modules’
;; -7 (:timeout)	pinggggggg!
;; -6 (:timeout)	pingggggg!
;; -5 (:timeout)	pinggggg!
;; -4 (:timeout)	pongggg!
;;
;; (get 'pingpong :fsm-event)
;; #s(hash-table size 11 ... )
;;
;; -3 (:timeout)	pinggg!
;; -2 (:timeout)	pingg!
;; -1 (:timeout)	ping!
;; 0 (:timeout)	pong!

;;** FSM State Hash-Table

(get 'pingpong :fsm-event)

;; #s(hash-table
;;    size
;;    11
;;    test
;;    eq rehash-size
;;    1.5
;;    rehash-threshold
;;    0.8125
;;    data
;;    (:ping (closure
;;               ((--cl-hey--
;;                 closure
;;                 (t)
;;                 (n ev)
;;                 (message
;;                  "%d (%s)	p%sn%s!"
;;                  n
;;                  ev
;;                  (if (= 0 (% n 4)) "o" "i")
;;                  (make-string
;;                   (max 1 (abs n))
;;                   103))))
;;               (fsm count event callback)
;;             (progn
;;               (funcall
;;                --cl-hey--
;;                (setq count (1+ count))
;;                event)
;;               (list :pingg count 0.1)))
;;           :pingg (closure
;;                      ((--cl-hey--
;;                        closure
;;                        (t)
;;                        (n ev)
;;                        (message
;;                         "%d (%s)	p%sn%s!"
;;                         n
;;                         ev
;;                         (if (= 0 (% n 4)) "o" "i")
;;                         (make-string
;;                          (max 1 (abs n))
;;                          103))))
;;                      (fsm count event callback)
;;                    (progn
;;                      (funcall
;;                       --cl-hey--
;;                       (setq count (1+ count))
;;                       event)
;;                      (list :pinggg count 0.1)))
;;           :pinggg (closure
;;                       ((--cl-hey--
;;                         closure
;;                         (t)
;;                         (n ev)
;;                         (message
;;                          "%d (%s)	p%sn%s!"
;;                          n
;;                          ev
;;                          (if (= 0 (% n 4)) "o" "i")
;;                          (make-string
;;                           (max 1 (abs n))
;;                           103))))
;;                       (fsm count event callback)
;;                     (progn
;;                       (funcall
;;                        --cl-hey--
;;                        (setq count (1+ count))
;;                        event)
;;                       (list :pong count 1)))
;;           :pong (closure
;;                     ((--cl-hey--
;;                       closure
;;                       (t)
;;                       (n ev)
;;                       (message
;;                        "%d (%s)	p%sn%s!"
;;                        n
;;                        ev
;;                        (if (= 0 (% n 4)) "o" "i")
;;                        (make-string
;;                         (max 1 (abs n))
;;                         103))))
;;                     (fsm count event callback)
;;                   (progn
;;                     (funcall
;;                      --cl-hey--
;;                      (setq count (1+ count))
;;                      event)
;;                     (list
;;                      :ping count
;;                      (if (= 0 count)
;;                          (fsm-goodbye-cruel-world
;;                           'pingpong)
;;                        3))))))
