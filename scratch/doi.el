


;; https://doi.org/ra/$doi : get registrant information
;; RA: just Crossref


;; from org-ref

;; https://ieeexplore.ieee.org/document/8897162
(let ((modern-robotics-text "10.1109/MCS.2019.2937265"))

  (setq scr/doi-metadata
        (json-parse-string (doi-utils-get-json
                            modern-robotics-text))))

(setq-local
 keys (let ((keys))
        (maphash (lambda (k v)
                   (set 'keys (a-assoc keys k v))) scr/doi-metadata)
        keys))


;; a-list k -> hash-table
;; keys

;; (
;;  ("published"
;;   . #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data
;;                   ("date-parts" [[2019 12]])))
;;  ("container-title-short" . "IEEE Control Syst.")
;;  ("subject" .
;;   ["Electrical and Electronic Engineering" "Modeling and Simulation" "Control and Systems Engineering" "Electrical and Electronic Engineering" "Modeling and Simulation" "Control and Systems Engineering"])
;;  ("ISSN" . ["1066-033X" "1941-000X"])
;;  ("relation"
;;   . #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data ()))
;;  ("URL" . "http://dx.doi.org/10.1109/MCS.2019.2937265")
;;  ("journal-issue"
;;   . #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("issue" "6")))
;;  ("references-count" . 7)
;;  ("issued"
;;   . #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2019 12]])))
;;  ("short-title" . [])
;;  ("subtitle" . [])
;;  ("resource"
;;   . #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("primary" #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("URL" "https://ieeexplore.ieee.org/document/8897162/")))))
;;  ("score" . 1)
;;  ("deposited"
;;   . #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2022 7 16]] "date-time" "2022-07-16T17:10:06Z" "timestamp" 1657991406000)))
;;  ("link"
;;   . [#s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("URL" "http://xplorestaging.ieee.org/ielx7/5488303/8897097/08897162.pdf?arnumber=8897162" "content-type" "unspecified" "content-version" "vor" "intended-application" "similarity-checking"))])
;;  ("original-title" . [])
;;  ("container-title" . "IEEE Control Systems")
;;  ("reference"
;;   . [#s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("key" "ref4" "author" "chevallier" "year" "2017" "journal-title" "Multi-Body Kinematics and Dynamics with Lie Groups")) #s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("key" "ref3" "author" "angeles" "year" "2003" "journal-title" "Fundamentals of Robotic Mechanical Systems")) #s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("key" "ref6" "author" "murray" "year" "1994" "journal-title" "A Mathematical Introduction to Robotic Manipulation")) #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("key" "ref5" "doi-asserted-by" "publisher" "DOI" "10.1007/978-1-4899-7560-7")) #s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("key" "ref7" "author" "selig" "year" "2005" "journal-title" "Geometric Fundamentals of Robotics")) #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("key" "ref2" "doi-asserted-by" "publisher" "DOI" "10.1007/BFb0031048")) #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("key" "ref1" "doi-asserted-by" "publisher" "DOI" "10.1007/978-3-319-32552-1"))])
;;  ("member" . "263")
;;  ("author"
;;   . [#s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("given" "Andreas" "family" "Mueller" "sequence" "first" "affiliation" []))])
;;  ("volume" . "39")
;;  ("prefix" . "10.1109")
;;  ("title" . "Modern Robotics: Mechanics, Planning, and Control [Bookshelf]")
;;  ("is-referenced-by-count" . 10)
;;  ("source" . "Crossref")
;;  ("page" . "100-102")
;;  ("created"
;;   . #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data
;;                   ("date-parts" [[2022 2 1]]
;;                    "date-time" "2022-02-01T21:42:04Z"
;;                    "timestamp" 1643751724000)))
;;  ("type" . "journal-article")
;;  ("DOI" . "10.1109/mcs.2019.2937265")
;;  ("published-print"
;;   . #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2019 12]])))
;;  ("content-domain"
;;   . #s(hash-table size 2 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("domain" [] "crossmark-restriction" :false)))
;;  ("license"
;;   . [#s(
;;         hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data
;;         ("start" #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2019 12 1]] "date-time" "2019-12-01T00:00:00Z" "timestamp" 1575158400000)) "content-version" "vor" "delay-in-days" 0 "URL" "https://ieeexplore.ieee.org/Xplorehelp/downloads/license-information/IEEE.html"))
;;      #s(
;;         hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data
;;         ("start" #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2019 12 1]] "date-time" "2019-12-01T00:00:00Z" "timestamp" 1575158400000)) "content-version" "stm-asf" "delay-in-days" 0 "URL" "https://doi.org/10.15223/policy-009"))
;;      #s(
;;         hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("start" #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2019 12 1]] "date-time" "2019-12-01T00:00:00Z" "timestamp" 1575158400000)) "content-version" "stm-asf" "delay-in-days" 0 "URL" "https://doi.org/10.15223/policy-001"))]
;;   )
;;  ("issue" . "6")
;;  ("publisher" . "Institute of Electrical and Electronics Engineers (IEEE)")
;;  ("reference-count" . 7)
;;  ("indexed"
;;   . #s(
;;        hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data
;;        ("date-parts" [[2023 6 15]] "date-time" "2023-06-15T08:10:15Z" "timestamp" 1686816615429))))

;; orig hash-table
;; #s(hash-table size 36 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("indexed" #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2023 6 15]] "date-time" "2023-06-15T08:10:15Z" "timestamp" 1686816615429)) "reference-count" 7 "publisher" "Institute of Electrical and Electronics Engineers (IEEE)" "issue" "6" "license" [#s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("start" #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2019 12 1]] "date-time" "2019-12-01T00:00:00Z" "timestamp" 1575158400000)) "content-version" "vor" "delay-in-days" 0 "URL" "https://ieeexplore.ieee.org/Xplorehelp/downloads/license-information/IEEE.html")) #s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("start" #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2019 12 1]] "date-time" "2019-12-01T00:00:00Z" "timestamp" 1575158400000)) "content-version" "stm-asf" "delay-in-days" 0 "URL" "https://doi.org/10.15223/policy-009")) #s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("start" #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2019 12 1]] "date-time" "2019-12-01T00:00:00Z" "timestamp" 1575158400000)) "content-version" "stm-asf" "delay-in-days" 0 "URL" "https://doi.org/10.15223/policy-001"))] "content-domain" #s(hash-table size 2 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("domain" [] "crossmark-restriction" :false)) "published-print" #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2019 12]])) "DOI" "10.1109/mcs.2019.2937265" "type" "journal-article" "created" #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2022 2 1]] "date-time" "2022-02-01T21:42:04Z" "timestamp" 1643751724000)) "page" "100-102" "source" "Crossref" "is-referenced-by-count" 10 "title" "Modern Robotics: Mechanics, Planning, and Control [Bookshelf]" "prefix" "10.1109" "volume" "39" "author" [#s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("given" "Andreas" "family" "Mueller" "sequence" "first" "affiliation" []))] "member" "263" "reference" [#s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("key" "ref4" "author" "chevallier" "year" "2017" "journal-title" "Multi-Body Kinematics and Dynamics with Lie Groups")) #s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("key" "ref3" "author" "angeles" "year" "2003" "journal-title" "Fundamentals of Robotic Mechanical Systems")) #s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("key" "ref6" "author" "murray" "year" "1994" "journal-title" "A Mathematical Introduction to Robotic Manipulation")) #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("key" "ref5" "doi-asserted-by" "publisher" "DOI" "10.1007/978-1-4899-7560-7")) #s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("key" "ref7" "author" "selig" "year" "2005" "journal-title" "Geometric Fundamentals of Robotics")) #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("key" "ref2" "doi-asserted-by" "publisher" "DOI" "10.1007/BFb0031048")) #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("key" "ref1" "doi-asserted-by" "publisher" "DOI" "10.1007/978-3-319-32552-1"))] "container-title" "IEEE Control Systems" "original-title" [] "link" [#s(hash-table size 4 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("URL" "http://xplorestaging.ieee.org/ielx7/5488303/8897097/08897162.pdf?arnumber=8897162" "content-type" "unspecified" "content-version" "vor" "intended-application" "similarity-checking"))] "deposited" #s(hash-table size 3 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2022 7 16]] "date-time" "2022-07-16T17:10:06Z" "timestamp" 1657991406000)) "score" 1 "resource" #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("primary" #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("URL" "https://ieeexplore.ieee.org/document/8897162/")))) "subtitle" [] "short-title" [] "issued" #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2019 12]])) "references-count" 7 "journal-issue" #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("issue" "6")) "URL" "http://dx.doi.org/10.1109/MCS.2019.2937265" "relation" #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data ()) "ISSN" ["1066-033X" "1941-000X"] "subject" ["Electrical and Electronic Engineering" "Modeling and Simulation" "Control and Systems Engineering" "Electrical and Electronic Engineering" "Modeling and Simulation" "Control and Systems Engineering"] "container-title-short" "IEEE Control Syst." "published" #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("date-parts" [[2019 12]]))))
