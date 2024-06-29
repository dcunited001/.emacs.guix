;; https://github.com/positron-solutions/transient-showcase

(defun tsc-suffix-wave-msg ()
  "Wave at the user."
  (interactive)
  (message "Waves at the user at: %s." (current-time-string)))

(defun tsc-suffix-wave ()
  (interactive)
  (alert "👋" :title "wave"))

(defun tsc-suffix-wave2 ()
  (interactive)
  (alert "✌️" :title "wave"))

(transient-define-prefix tsc-layout-the-grid ()
  "Prefix with groups in a grid-like arrangement."

  [:description "The Grid\n"            ; must use slot or macro is confused
                ["Left Column"          ; note, no newline
                 ("ltt" "left top top" tsc-suffix-wave)
                 ("ltb" "left top bottom" tsc-suffix-wave)
                 ""
                 ("lbt" "left bottom top" tsc-suffix-wave)
                 ("lbb" "left bottom bottom" tsc-suffix-wave)] ; note, no newline

                ["Right Column\n"
                 ("rtt" "right top top" tsc-suffix-wave2)
                 ("rtb" "right top bottom" tsc-suffix-wave2)
                 ""
                 ("rbt" "right bottom top" tsc-suffix-wave2)
                 ("rbb" "right bottom bottom\n" tsc-suffix-wave2)]])

(defun transient-test-grid ()
  "test grid"
  (interactive)
  (tsc-layout-the-grid))

;; ---------------------------------------------
;; toggle unicode for casual transients

;; (dc/toggleable-boolean casual-lib-use-unicode)

;; deprecated
;; (dc/toggleable-boolean casual-info-use-unicode-symbols)

;; ---------------------------------------------
;; no need to rebuild the transient apparently (which makes sense)

(defun dc/toggle-casual-lib-unicode (library)
  ;; (buffer-file-name (find-file-noselect (find-library-name library)))
  (when-let ((libname (find-library-name library)))
    (load libname)))
