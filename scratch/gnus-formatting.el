;;** Gnus Visual


;;  article-highlight
;;  article-menu
;;  binary-menu
;;  browse-menu
;;  group-highlight
;;  group-menu
;;  highlight
;;  menu
;;  mouse-face
;;  page-marker
;;  pick-menu
;;  server-menu
;;  summary-highlight
;;  summary-menu
;;  tree-highlight
;;  tree-menu



;;** Gnus Summary Line

;; ▬►☺
;; 263A 	☻
;; 263B 	♥
;; 2665 	♦
;; 2666 	♣
;; 2663 	♠
;; 2660 	•
;; 2022 	◘
;; 25D8 	○
;; 25CB 	◙
;; 25D9 	♂
;; 2642 	♀
;; 2640 	♪
;; 266A 	♫[b]
;; 266B


(setq gnus-summary-line-format "%U%R%z %d %B%-3L %[%f%] %s\n"

      gnus-sum-thread-tree-root            "╦═► "
      gnus-sum-thread-tree-false-root      "○╦═► "
      gnus-sum-thread-tree-vertical        "│"
      gnus-sum-thread-tree-leaf-with-other "├╤═► "
      gnus-sum-thread-tree-single-leaf     "└╤═►"
      gnus-sum-thread-tree-indent          " "
      gnus-sum-thread-tree-single-indent   "■ "
      gnus-summary-newsgroup-prefix        "⇒ "
      gnus-summary-to-prefix               "→ ")

(setq gnus-summary-line-format "%U%R%z %d %B%-3L %[%f%] %s\n"

      gnus-sum-thread-tree-root            "■═► "
      gnus-sum-thread-tree-false-root      "○╥─► "
      gnus-sum-thread-tree-vertical        "│"
      gnus-sum-thread-tree-leaf-with-other "╠╤═► "
      gnus-sum-thread-tree-single-leaf     "╚╤═►"
      gnus-sum-thread-tree-indent          " "
      gnus-sum-thread-tree-single-indent   "■ "
      gnus-summary-newsgroup-prefix        "⇒ "
      gnus-summary-to-prefix               "→ ")

(setq gnus-summary-line-format "%U%R%z %d %B%-3L %[%f%] %s\n"

      gnus-sum-thread-tree-root            "○╦═► "
      gnus-sum-thread-tree-false-root      "╥─► "
      gnus-sum-thread-tree-vertical        "║"
      gnus-sum-thread-tree-leaf-with-other "╟──► "
      gnus-sum-thread-tree-single-leaf     "╙──►"
      gnus-sum-thread-tree-indent          " "
      gnus-sum-thread-tree-single-indent   "■ "
      gnus-summary-newsgroup-prefix        "⇒ "
      gnus-summary-to-prefix               "→ ")


(("%N" (mail-header-number gnus-tmp-header) 100)
 ("%S" (mail-header-subject gnus-tmp-header) 115)
 ("%s" gnus-tmp-subject-or-nil 115)
 ("%n" gnus-tmp-name 115)
 ("%A" (car (cdr (funcall gnus-extract-address-components gnus-tmp-from))) 115)
 ("%a" (or (car (funcall gnus-extract-address-components gnus-tmp-from)) gnus-tmp-from) 115)
 ("%F" gnus-tmp-from 115)
 ("%x" (mail-header-xref gnus-tmp-header) 115)
 ("%D" (mail-header-date gnus-tmp-header) 115)
 ("%d" (gnus-dd-mmm (mail-header-date gnus-tmp-header)) 115)
 ("%o" (gnus-date-iso8601 (mail-header-date gnus-tmp-header)) 115)
 ("%M" (mail-header-id gnus-tmp-header) 115)
 ("%r" (mail-header-references gnus-tmp-header) 115)
 ("%c" (or (mail-header-chars gnus-tmp-header) 0) 100)
 ("%k" (gnus-summary-line-message-size gnus-tmp-header) 115)
 ("%L" gnus-tmp-lines 115)
 ("%Z" (or (nnselect-article-rsv (mail-header-number gnus-tmp-header)) 0) 100)
 ("%G" (or (nnselect-article-group (mail-header-number gnus-tmp-header)) "") 115)
 ("%g" (or (gnus-group-short-name (nnselect-article-group (mail-header-number gnus-tmp-header))) "") 115)
 ("%O" gnus-tmp-downloaded 99)
 ("%I" gnus-tmp-indentation 115)
 ("%T" (if (= gnus-tmp-level 0) "" (make-string (frame-width) 32)) 115)
 ("%R" gnus-tmp-replied 99)
 ("%[" gnus-tmp-opening-bracket 115)
 ("%]" gnus-tmp-closing-bracket 115)
 ("%>" (make-string gnus-tmp-level 32) 115)
 ("%<" (make-string (max 0 (- 20 gnus-tmp-level)) 32) 115)
 ("%i" gnus-tmp-score 100)
 ("%z" gnus-tmp-score-char 99)
 ("%V" (gnus-thread-total-score (and (boundp 'gnus-tmp-thread) (car gnus-tmp-thread))) 100)
 ("%U" gnus-tmp-unread 99)
 ("%f" (gnus-summary-from-or-to-or-newsgroups gnus-tmp-header gnus-tmp-from) 115)
 ("%t" (gnus-summary-number-of-articles-in-thread (and (boundp 'gnus-tmp-thread) (car gnus-tmp-thread)) gnus-tmp-level) 100)
 ("%e" (gnus-summary-number-of-articles-in-thread (and (boundp 'gnus-tmp-thread) (car gnus-tmp-thread)) gnus-tmp-level t) 99)
 ("%u" gnus-tmp-user-defined 115)
 ("%P" (gnus-pick-line-number) 100)
 ("%B" gnus-tmp-thread-tree-header-string 115)
 ("%&user-date" (gnus-user-date (mail-header-date gnus-tmp-header)) 115))


;;** Gnus Summary


;; gnus-group-highlight

;; (((and mailp (= unread 0) (eq level 1)) . gnus-group-mail-1-empty)
;;  ((and mailp (eq level 1)) . gnus-group-mail-1)
;;  ((and mailp (= unread 0) (eq level 2)) . gnus-group-mail-2-empty)
;;  ((and mailp (eq level 2)) . gnus-group-mail-2)
;;  ((and mailp (= unread 0) (eq level 3)) . gnus-group-mail-3-empty)
;;  ((and mailp (eq level 3)) . gnus-group-mail-3)
;;  ((and mailp (= unread 0)) . gnus-group-mail-low-empty)
;;  ((and mailp) . gnus-group-mail-low)
;;  ((and (= unread 0) (eq level 1)) . gnus-group-news-1-empty)
;;  ((and (eq level 1)) . gnus-group-news-1)
;;  ((and (= unread 0) (eq level 2)) . gnus-group-news-2-empty)
;;  ((and (eq level 2)) . gnus-group-news-2)
;;  ((and (= unread 0) (eq level 3)) . gnus-group-news-3-empty)
;;  ((and (eq level 3)) . gnus-group-news-3)
;;  ((and (= unread 0) (eq level 4)) . gnus-group-news-4-empty)
;;  ((and (eq level 4)) . gnus-group-news-4)
;;  ((and (= unread 0) (eq level 5)) . gnus-group-news-5-empty)
;;  ((and (eq level 5)) . gnus-group-news-5)
;;  ((and (= unread 0) (eq level 6)) . gnus-group-news-6-empty)
;;  ((and (eq level 6)) . gnus-group-news-6)
;;  ((and (= unread 0)) . gnus-group-news-low-empty)
;;  (t . gnus-group-news-low))
