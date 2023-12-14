;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(let (thekeys '())
  (maphash (lambda (k v) (push (list k v) thekeys)) jupyter--kernelspecs)
  thekeys)
;; =>
;;

(("local" #s
  (hash-table size 5 test equal rehash-size 1.5 rehash-threshold 0.8125 data
              (nil
               (#s
                (jupyter-kernelspec
                 "python3"
                 (:argv
                  ["/gnu/store/94ccg3my0xpsy3rna5bwf664g708ascy-python-toolchain-3.10.7/bin/python"
                   "-m" "ipykernel_launcher" "-f" "{connection_file}"]
                  :env nil
                  :display_name "Python 3 (ipykernel)"
                  :language "python"
                  :interrupt_mode "signal"
                  :metadata (:debugger t))
                 "/gnu/store/bmbfj937k0zjrjkn2a91xva6fm3m6vah-python-ipykernel-bootstrap-6.13.0/lib/python3.10/site-packages/ipykernel/resources"))))))
