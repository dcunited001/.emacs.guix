
;; general takeaways:

;; it's definitely :KEYMAPS and not :KEYMAP

;; :keymaps/:prefix are mutually exclusive with :prefix-map/:prefix-command
;; w.r.t. the `keymaps' variable only. this also means, that when the latter are
;; set, there is only one keymap

(when (and (or prefix-map prefix-command)
           (not (or prefix keymaps-specified-p)))
  (setq keymaps (list (or prefix-map prefix-command))))

;; however, in the second call to (foo-def ..) below, the keys are added to


;; ===================================================================

;; second set of tests

;; -------------------------------------------------------------------

(general-create-definer foo-def
  :global-prefix "C-c")

(foo-def
  :keymaps 'global
  ;; add `:wk' or `which-keys' won't be generated when using `:global-prefix'
  "n" '(:prefix-command dc/org-agenda-global-map :wk "AGENDA")
  "nr" '(:prefix-command dc/org-roam-global-map :wk "ROAM")
  "nrf" #'org-roam-node-find
  )

;; dc/org-agenda-global-map
;; r f							org-roam-node-find

;; dc/org-roam-global-map
;; f							org-roam-node-find


;; requires running an org-roam funciton to load the code
(foo-def
  :keymaps 'global
  :prefix "nr"

  "i" #'org-roam-node-insert)

;; dc/org-agenda-global-map
;; r f							org-roam-node-find
;; r i							org-roam-node-insert

;; dc/org-roam-global-map
;; f							org-roam-node-find
;; i							org-roam-node-insert

;; global-map ... so instead ... ?
;; C-c i						org-roam-node-insert

(foo-def
  :keymaps 'global
  :infix "nr"

  "n" #'org-roam-capture)

;; correct

(foo-def
  :keymaps 'org-mode-map
  :infix "l"

  "a" #'org-roam-alias-add)

;; org-mode-map (it applies the prefix twice, no more L's for you)
;; l a	    						org-roam-alias-add
;; C-c l a							org-roam-alias-add

(foo-def
  :keymaps 'org-mode-map
  :prefix-map 'dc/org-C-c-prefix-map
  :prefix-command 'dc/org-C-c-prefix-map

  "l" '(:ignore t :wk "LOCAL")
  "lA" #'org-roam-alias-remove)

;; org-mode-map
;; l A								org-roam-alias-remove
;; C-c l A						org-roam-alias-remove

;; dc/org-C-c-prefix-map (empty)

(foo-def
  :prefix-map 'dc/org-C-c-prefix-map
  :prefix-command 'dc/org-C-c-prefix-map

  "l" '(:ignore t :wk "LOCAL")
  "lr" #'org-roam-ref-add
  "lR" #'org-roam-ref-remove)

;; dc/org-C-c-prefix-map
;; l R							org-roam-ref-remove
;; l r							org-roam-ref-add
;; C-c l R					org-roam-ref-remove
;; C-c l r					org-roam-ref-add

;; org-mode-map (nothing added)

(foo-def
  :keymaps 'org-mode-map
  :infix "l"

  "t" #'org-roam-tag-add
  "T" #'org-roam-tag-remove)

;; org-mode-map (the lt and lT have no effect)
;; l t							org-roam-tag-add
;; l T							org-roam-tag-remove
;; C-c l t					org-roam-tag-add
;; C-c l T					org-roam-tag-remove

;; ===================================================================

;; first run-through

;; now looking at the tests for general.el ... it doesn't support the same DSL
;; as doom emacs.

;; https://github.com/noctuid/general.el/blob/master/tests/test-general.el

;; -------------------------------------------------------------------

;; can general definers bind globally and to a context-defined prefix-map? no

(general-create-definer foo-def
  :prefix "C-c")


;; requires running an org-roam funciton to load the code
(foo-def
 :keymaps 'global
 :prefix "C-c"

 "nrf" #'org-roam-node-find

 :keymaps 'org-mode-map
 :prefix "C-c l"
 "a" #'org-roam-alias-add)

;; this doesn't work as i would expect it to.

;; -------------------------------------------------------------------

;; a bit of a pain to test, since it only defines the 'org-roam-etc-map once.
;; it can add to it, but it can't clear it out. you need to manually set the
;; value

(general-create-definer bar-def
  :prefix "C-c")

(bar-def
 :keymaps 'org-mode-map
 :prefix "l"
 :prefix-map 'org-roam-etc-map
 "a" #'org-roam-alias-add)

;; org-roam-etc-map is a keymap variable.

;; Key             Binding
;; a								org-roam-alias-add

;; -------------------------------------------------------------------




;; ===================================================================

;; general-lispy-define-key

;; -------------------------------------------------------------------

