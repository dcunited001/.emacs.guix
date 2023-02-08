;; -*- lexical-binding: t; -*-

;;* Social

;;** Elfeed

(setup (:pkg elfeed))

(setup (:pkg elfeed-org)
  (:load-after elfeed
    (:option rmh-elfeed-org-files (list (concat dc/emacs-d "elfeed.org")))
    (elfeed-org)))

;;** IRC

;; post files/images to 0x0, intended for sharing in ERC
;; (setup (:pkg 0x0 :host gitlab :repo "willvaughn/emacs-0x0" :straight t))

(provide 'dc-social)
