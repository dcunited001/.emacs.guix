(require 'tree-sitter-core)

;; https://emacs-tree-sitter.github.io/api/

;; language: A language object that defines how to parse a language.
;; parser: A stateful object that consumes source code and produces a parse tree.
;; tree: A parse tree that contains syntax node’s, which can be inspected.
;; cursor: A stateful object used to traverse a parse tree.
;; query: A compiled list of structural patterns to search for in a parse tree.
;; query-cursor A stateful object used to execute a query.
;; point: A pair of (line-number . byte-column).
;;     line-number is the absolute line number returned by line-number-at-pos, counting from 1.
;;     byte-column counts from 0, like current-column. However, unlike that function, it counts bytes, instead of displayed glyphs.
;; range: A vector in the form of [start-bytepos end-bytepos start-point end-point].


;; extract snippets from parsed tree-sitter AST's
;; -  specify query (limiting depth or number of siblings or block passing)
;; -  join into trie (or reverse trie)
;; -  attach count/locations to each instance of structure
;; -  iterate over structure and extract frequent ast branch motifs into snippet

(provide 'tree-snipper)
