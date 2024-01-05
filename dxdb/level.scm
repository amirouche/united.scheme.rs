;; TODO: XXX: This is draft.
(library (letloop dxdb level)

  (export #;make-level
          ;; level-maybe-contains?
          ;; level-ref
          ;; level-query
          ;; level-start
          ;; level-end
          ;; okvs->level
          #;~check-level-000-bytetrie-debug
          #;~check-level-001-bytetrie-finalize)

  (import (scheme base)
          (scheme comparator)
          (only (chezscheme) equal-hash display assert)
          (letloop byter)
          (letloop r999)
          (scheme hash-table))

  (define-record-type* <letloop-level-bytetrie>
    (make-bytetrie-base hash-table key value)
    bytetrie?
    (hash-table bytetrie-hash-table)
    (key bytetrie-key)
    (value bytetrie-value bytetrie-value!))

  (define make-bytetrie
    (lambda (key)
      (make-bytetrie-base (make-hash-table (make-comparator number? = #f equal-hash))
                          key
                          #f)))

  (define (bytetrie-debug bytetrie)
    ;; TODO: FIXME.

    (define root bytetrie)
    (define todo (make-hash-table (make-eq-comparator)))

    (hash-table-set! todo root #t)

    (display "digraph g {
 graph [fontname = \"Noto Sans\"];
 node [fontname = \"Noto Sans\"];
 edge [fontname = \"Noto Sans\"];
")

    (let loop ()
      ;; TODO: replace hash table with list.
      (unless (hash-table-empty? todo)
        (call-with-values (lambda () (hash-table-pop! todo))
          (lambda (trie _)
            (call-with-values (lambda () (hash-table-entries (bytetrie-hash-table trie)))
              (lambda (keys values)
                (let loop0 ((entries (map cons keys values)))
                  (unless (null? entries)
                    (let* ((byte (caar entries))
                           (child (cdar entries)))
                      (display "\t") (display (bytetrie-key trie)) (display " -> ") (display byte)(newline)
                      (hash-table-set! todo child #t)
                      (loop0 (cdr entries)))))))
            ;; render success
            (when (bytetrie-value trie)
              (display "\t") (display (bytetrie-key trie)) (display " -> \"") (display (bytetrie-value trie)) (display "\" [color=\"green\"]") (newline))))
        (loop)))

    (display "\n}")
    (newline))

  (define bytetrie-add!
    (lambda (bytetrie bytevector value)
      (define i (assert (not (= (bytevector-length bytevector) 0))))
      (define byte (bytevector-u8-ref bytevector 0))
      (define child (hash-table-ref/default (bytetrie-hash-table bytetrie)
                                            byte
                                            (make-bytetrie byte)))
      (hash-table-set! (bytetrie-hash-table bytetrie)
                       byte
                       child)
      (if (= (bytevector-length bytevector) 1)
          (bytetrie-value! child value)
          (bytetrie-add! child (byter-slice bytevector 1 (bytevector-length bytevector)) value))))

   ;; TODO
  (define bytetrie-search)

  (define ~check-level-000-bytetrie-debug
    (lambda ()
      (define x (make-bytetrie 'check-level-000))
      (bytetrie-add! x (bytevector 1 2 101) '(1 2 101))
      (bytetrie-add! x (bytevector 1 2 42) '(1 2 42))
      (bytetrie-debug x)))

  (define bytetrie-finalize
    (lambda (bytetrie)
      (let loop ((bytetrie bytetrie)
                 (bytes (list (bytetrie-key bytetrie))))
        (pk bytes)
        (if (= 1 (hash-table-size (bytetrie-hash-table bytetrie)))
            (call-with-values (lambda () (hash-table-pop! (bytetrie-hash-table bytetrie)))
              (lambda (x y)
                (loop y
                      (cons x
                            bytes))))
            (let ((new (make-bytetrie (reverse bytes))))
              (pk 'fooof)
              (call-with-values (lambda ()
                                  (hash-table-entries (bytetrie-hash-table bytetrie)))
                (lambda (keys values)
                  (for-each (lambda (k v)
                              (hash-table-set! (bytetrie-hash-table new) k (bytetrie-finalize v)))
                            keys values)))
              new)))))

  (define ~check-level-001-bytetrie-finalize
    (lambda ()
      (define x (make-bytetrie 'check-level-001))
      (bytetrie-add! x (bytevector 1 2 101) '(1 2 101))
      (bytetrie-add! x (bytevector 1 2 42) '(1 2 42))
      (bytetrie-debug (bytetrie-finalize x))))

  )
