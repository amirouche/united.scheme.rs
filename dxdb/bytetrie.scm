(library (letloop dxdb bytetrie)
  (export bytetrie
          bytetrie-add!
          bytetrie-finalize
          ;; ~check-bytetrie-000
          ;; ~check-bytetrie-001
          ;; ~check-bytetrie-002
          )

  (import (chezscheme)
          (letloop r999)
          (letloop flow)
          (letloop dxdb notebook)
          (letloop byter))

  (define bytetrie-null (- (expt 2 64) 1))

  (define integer->bytevector
    (lambda (integer)
      (let ((bytevector (make-bytevector 8)))
        (bytevector-u64-set! bytevector 0 integer 'big)
        bytevector)))

  (define-record-type* <bytetrie>
    (make-bytetrie hashtable value)
    bytetrie?
    (hashtable bytetrie-hashtable)
    (value bytetrie-value bytetrie-value!))

  (define bytetrie
    (lambda ()
      (make-bytetrie (make-hashtable equal-hash equal?)
                     (integer->bytevector bytetrie-null))))

  (define bytetrie-add!
    (lambda (b k* v)
      (define k (bytevector->u8-list k*))
      (let loop ((b b)
                 (k k)
                 (v v))
        (define i k)
        (define h (bytetrie-hashtable b))
        (define c (hashtable-ref h
                                 (car k)
                                 (bytetrie)))
        (hashtable-set! h (car k) c)
        (if (null? (cdr k))
            (bytetrie-value! c v)
            (loop c (cdr k) v)))))

  (define bytevector-random
    (lambda (n)
      (u8-list->bytevector (map (lambda _ (random 256))
                                (iota n)))))

  (define hashtable->alist
    (lambda (h)
      (call-with-values (lambda () (hashtable-entries h))
        (lambda (keys values)
          (map cons (vector->list keys) (vector->list values))))))

  (define bytetrie-finalize
    (lambda (b)
      (let loop ((b b)
                 (e '()))
        (let ((h (hashtable->alist (bytetrie-hashtable b)))
              (v (bytetrie-value b)))
          (if (null? h)
              (list (u8-list->bytevector (reverse e)) v #f)
              (if (and (null? (cdr h)) (not (bytetrie-value b)))
                  (loop (cdar h) (cons (caar h) e))
                  (list (u8-list->bytevector (reverse e))
                        (bytetrie-value b)
                        (map (lambda (x)
                               (loop (cdr x)
                                     (list (car x))))
                             h))))))))

  (define pk
    (lambda args
      (display ";; ")
      (write args)
      (newline)
      (car (reverse args))))

  (define shuffle
    (lambda (objects)

      (define random-choice
        (lambda (objects)
          (list-ref objects (random (length objects)))))

      (let loop ((objects objects)
                 (out '()))
        (if (null? (cdr objects))
            (cons (car objects) out)
            (let* ((object (random-choice objects))
                   (objects (remove object objects)))
              (loop objects (cons object out)))))))

  (define ~check-bytetrie-000
    (lambda ()

      ;;

      (define b (bytetrie))
      (define query (list 5 3 3 7 7 7))

      (define bvs
        (shuffle
         (list
          (cons (list 5 1 42) (integer->bytevector 1))
          (cons (list 5 1 43) (integer->bytevector 2))
          (cons (list 5 2 1) (integer->bytevector 3))
          (cons (list 5 2 101) (integer->bytevector 4))
          (cons (list 5 3 3) (integer->bytevector 5))
          (cons (list 5 3 3 7 7 7) (integer->bytevector 6))
          (cons (list 0 0 1 1 2 2) (integer->bytevector 7))
          (cons (list 0 0 2 2 3 3) (integer->bytevector 8)))))

      (for-each (lambda (x)
                  (bytetrie-add! b
                                 (u8-list->bytevector (car x))
                                 (cdr x)))
                bvs)

      ;; TODO: bytetrie-finalize will convert a bytetrie made of
      ;; records, a composition of lists and bytevector. Maybe that
      ;; should be a single procedure called `bytetrie-write`. And
      ;; maybe it will be possible to make it faster, and more
      ;; performant that way.
      (for-each pretty-print (bytetrie-finalize b))

      #t))

  (define bytetrie-write
    (lambda (b notebook)
      (define xxx (make-notebook-chapter-x notebook))
      (bytetrie-write-accumulator b xxx)))

  (define ~check-bytetrie-002
    (lambda ()
      (define b (bytetrie))

      (define xxx
        (let ((out '()))
          (lambda (o)
            (if (eof-object? o)
                (byter-concatenate (reverse out))
                (set! out (cons o out))))))

      (define query (list 5 3 3 7 7 7))

      (define bvs
        (shuffle
         (list
          ;; (cons (list 5 1 42) (integer->bytevector 1))
          ;; (cons (list 5 1 43) (integer->bytevector 2))
          ;; (cons (list 5 2 1) (integer->bytevector 3))
          ;; (cons (list 5 2 101) (integer->bytevector 4))
          ;; (cons (list 5 3 3) (integer->bytevector 5))
          (cons (list 5 3 3 7 7 7) (integer->bytevector 777))
          (cons (list 0 101) (integer->bytevector 102))
          (cons (list 0 42) (integer->bytevector 43)))))

      (for-each (lambda (x)
                  (bytetrie-add! b
                                 (u8-list->bytevector (car x))
                                 (cdr x)))
                bvs)

      (bytetrie-write-accumulator b xxx)

      (pk (xxx (eof-object)))))

  (define bytetrie-write-accumulator
    (lambda (b xxx)
      (define iii (pretty-print (bytetrie-finalize b)))
      (define trie-bytes car)
      (define trie-value cadr)
      (define trie-children (lambda (x) (if (caddr x) (caddr x) '())))

      (let loop ((offset 0)
                 (a (bytetrie-finalize b)))
        (pk 'loop 'in offset)
        ;; Write the bytetrie recursively, each node on disk schema:
        ;;
        ;;  [parent node length: 8 bytes] [bytes length: 8 bytes] [bytes ...] [value: 8 bytes]
        ;;  [child nodes length: 8 bytes] [child node ...] [child node ...]
        ;;
        ;; Where 'child node' is a the offset of the beginning of the
        ;; child node's length.
        ;;
        (if (null? (trie-children a))
            (let ((length (+ 8 8 (bytevector-length (trie-bytes a)) 8 8)))
              (pk 'loop 'null?)
              (xxx (integer->bytevector length))
              (xxx (integer->bytevector (bytevector-length (trie-bytes a))))
              (xxx (trie-bytes a))
              (xxx (trie-value a))
              ;; TODO: remove that length, and do the sum of the first length
              ;; with the various fields.
              (xxx (integer->bytevector 0))
              (cons offset length))
            (call-with-values (lambda ()
                                (let loopx ((children (trie-children a))
                                            (offset* offset)
                                            (out '()))
                                  (if (null? children)
                                      (values (+ (caar out) (cdar out))
                                              (reverse out))
                                      (let* ((out* (loop offset* (car children))))
                                        (loopx (cdr children)
                                               (+ (car out*) (cdr out*))
                                               (cons out* out))))))
              (lambda (offset offset+length)
                (pk 'offset offset)
                (pk 'offset+length offset+length)
                (let ((length*
                       (+ 8 8 (bytevector-length (trie-bytes a))
                          8 8 (* 8 (length offset+length)))))
                  (pk 'loop-else offset+length)
                  (xxx (integer->bytevector length*))
                  (xxx (integer->bytevector (bytevector-length (trie-bytes a))))
                  (xxx (trie-bytes a))
                  (xxx (trie-value a))
                  (xxx (integer->bytevector (* 8 (length offset+length))))
                  (for-each (lambda (x) (xxx (pk 'child (integer->bytevector x))))
                            (map car offset+length))
                  (pk 'loop 'out (cons offset length*)))))))))

  (define call-with-temporary-filepath
    (lambda (prefix proc)
      (define stdlib (load-shared-object #f))
      (define mkstemp
        (foreign-procedure "mkstemp" (string) int))

      (define close
        (foreign-procedure "close" (int) int))

      (define (make-temporary-filepath prefix)
        (let ((input (string-append prefix "-XXXXXX")))
          (close (mkstemp input))
          input))

      (define filepath (make-temporary-filepath prefix))

      (call-with-values (lambda () (proc filepath))
        (lambda args
          (delete-file filepath)
          (apply values args)))))

  (define bytetrie-ref
    (lambda (notebook x key)
      (define bv (notebook-chapter-x-ref notebook x))
      #f))

  (define ~check-bytetrie-001
    (lambda ()
      (define b (bytetrie))

      (define query (list 5 3 3 7 7 7))

      (define bvs
        (shuffle
         (list
          (cons (list 5 1 42) (integer->bytevector 1))
          (cons (list 5 1 43) (integer->bytevector 2))
          (cons (list 5 2 1) (integer->bytevector 3))
          (cons (list 5 2 101) (integer->bytevector 4))
          (cons (list 5 3 3) (integer->bytevector 5))
          (cons (list 5 3 3 7 7 7) (integer->bytevector 6))
          (cons (list 0 0 1 1 2 2) (integer->bytevector 7))
          (cons (list 0 0 2 2 3 3) (integer->bytevector 8)))))

      (for-each (lambda (x)
                  (bytetrie-add! b
                                 (u8-list->bytevector (car x))
                                 (cdr x)))
                bvs)

      (call-with-temporary-filepath "notebook-check"
        (lambda (filepath)
          (with-flow
           (let* ((notebook (make-notebook filepath 1024))
                  (x (bytetrie-write b notebook)))
             (pk (make-notebook-chapter-x notebook x))))))))

  )
