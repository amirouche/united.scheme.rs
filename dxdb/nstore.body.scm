(define any
  (lambda (p? objects)
    (let loop ((objects objects))
      (if (null? objects)
          #f
          (if (p? (car objects))
              #t
              (loop (cdr objects)))))))

(define every
  (lambda (p? objects)
    (let loop ((objects objects))
      (if (null? objects)
          #t
          (if (p? (car objects))
              (loop (cdr objects))
              #f)))))

;; combinatorics helpers

(define (permutations s)
  ;; http://rosettacode.org/wiki/Permutations#Scheme
  (cond
   ((null? s) '(()))
   ((null? (cdr s)) (list s))
   (else ;; extract each item in list in turn and permutations the rest
    (let splice ((l '()) (m (car s)) (r (cdr s)))
      (append
       (map (lambda (x) (cons m x)) (permutations (append l r)))
       (if (null? r) '()
           (splice (cons m l) (car r) (cdr r))))))))

(define (combination k lst)
  (cond
   ((= k 0) '(()))
   ((null? lst) '())
   (else
    (let ((head (car lst))
          (tail (cdr lst)))
      (append (map (lambda (y) (cons head y)) (combination (- k 1) tail))
              (combination k tail))))))

(define (combinations lst)
  (if (null? lst) '(())
      (let* ((head (car lst))
             (tail (cdr lst))
             (s (combinations tail))
             (v (map (lambda (x) (cons head x)) s)))
        (append s v))))

;; make-indices will compute smallest set of
;; indices/tables/subspaces required to bind any pattern in one
;; hop. The math behind this computation is explained at:
;;
;;   https://math.stackexchange.com/q/3146568/23663
;;
;; make-indices will return the smallest set of permutations in
;; lexicographic order of the base index ie. the output of (iota
;; n) where n is the length of ITEMS ie. the n in nstore.

(define (prefix? lst other)
  "Return #t if LST is prefix of OTHER"
  (let loop ((lst lst)
             (other other))
    (if (null? lst)
        #t
        (if (= (car lst) (car other))
            (loop (cdr lst) (cdr other))
            #f))))

(define (permutation-prefix? c o)
  (any (lambda (p) (prefix? p o)) (permutations c)))

(define (ok? combinations candidate)
  (every (lambda (c) (any (lambda (p) (permutation-prefix? c p)) candidate)) combinations))

(define (findij L)
  (let loop3 ((x L)
              (y '()))
    (if (or (null? x) (null? (cdr x)))
        (values #f (append (reverse y) x) #f #f)
        (if (and (not (cdr (list-ref x 0))) (cdr (list-ref x 1)))
            (values #t
                    (append (cddr x) (reverse y))
                    (car (list-ref x 0))
                    (car (list-ref x 1)))
            (loop3 (cdr x) (cons (car x) y))))))

(define (lex< a b)
  (let loop ((a a)
             (b b))
    (if (null? a)
        #t
        (if (not (= (car a) (car b)))
            (< (car a) (car b))
            (loop (cdr a) (cdr b))))))

(define (make-indices n)
  ;; This is based on:
  ;;
  ;;   https://math.stackexchange.com/a/3146793/23663
  ;;
  (let* ((tab (iota n))
         (cx (combination (floor (/ n 2)) tab)))
    (let loop1 ((cx cx)
                (out '()))
      (if (null? cx)
          (begin (unless (ok? (combinations tab) out)
                   (error 'okvs "impossible..."))
                 (list-sort lex< out))
          (let loop2 ((L (map (lambda (i) (cons i (not (not (memv i (car cx)))))) tab))
                      (a '())
                      (b '()))
            (call-with-values (lambda () (findij L))
              (lambda (continue? L i j)
                (if continue?
                    (loop2 L (cons j a) (cons i b))
                    (loop1 (cdr cx)
                           (cons (append (reverse a) (map car L) (reverse b))
                                 out))))))))))

(define-record-type* <nstore>
  (make-nstore% prefix indices n)
  nstore?
  (prefix nstore-prefix)
  (indices nstore-indices)
  (n nstore-n))

(define (make-nstore prefix n)
  (make-nstore% prefix
               (make-indices n)
               n))

(define (make-tuple list permutation)
  ;; Construct a permutation of LIST based on PERMUTATION
  (let ((tuple (make-vector (length permutation))))
    (for-each (lambda (index value) (vector-set! tuple index value)) permutation list)
    (vector->list tuple)))

(define (permute items index)
  ;; inverse of `make-tuple`
  (let ((items (list->vector items)))
    (let loop ((index index)
               (out '()))
      (if (null? index)
          (reverse out)
          (loop (cdr index)
                (cons (vector-ref items (car index)) out))))))

(define nstore-add!
  (lambda (transaction nstore items value)
    (define prefix (nstore-prefix nstore))
    ;; add ITEMS into the okvs and prefix each of the permutation
    ;; of ITEMS with the nstore-prefix and the index of the
    ;; permutation inside the list INDICES called SUBSPACE.
    (let loop ((indices (nstore-indices nstore))
               (subspace 0))
      (unless (null? indices)
        (let ((key (byter-write
                    (list->vector (append (list prefix
                                                subspace)
                                          (permute items (car indices)))))))
          (okvs-set! transaction key value)
          (loop (cdr indices) (+ subspace 1)))))))

(define nstore-clear!
  (lambda (transaction nstore items)
    (define prefix (nstore-prefix nstore))
    ;; Similar to the above but remove ITEMS
    (let loop ((indices (nstore-indices nstore))
               (subspace 0))
      (unless (null? indices)
        (let ((key (byter-write (list->vector
                                (append (list prefix
                                              subspace)
                                        (permute items (car indices)))))))
          (okvs-clear! transaction key)
          (loop (cdr indices) (+ subspace 1)))))))

(define-record-type* <nstore-var>
  (nstore-var name)
  nstore-var?
  (name nstore-var-name))

(define (bind* pattern tuple seed)
  ;; Associate variables of PATTERN to value of TUPLE with SEED.
  (let loop ((tuple tuple)
             (pattern pattern)
             (out seed))
    (if (null? tuple)
        out
        (if (nstore-var? (car pattern)) ;; only bind variables
            (loop (cdr tuple)
                  (cdr pattern)
                  (cons (cons (nstore-var-name (car pattern))
                              (car tuple))
                        out))
            (loop (cdr tuple) (cdr pattern) out)))))

(define (pattern->combination pattern)
  (let loop ((pattern pattern)
             (index 0)
             (out '()))
    (if (null? pattern)
        (reverse out)
        (loop (cdr pattern)
              (+ 1 index)
              (if (nstore-var? (car pattern))
                  out
                  (cons index out))))))

(define (pattern->index pattern indices)
  ;; Retrieve the index and subspace that will allow to bind
  ;; PATTERN in one hop. This is done by getting all non-variable
  ;; items of PATTERN and looking up the first index that is
  ;; permutation-prefix...
  (let ((combination (pattern->combination pattern)))
    (let loop ((indices indices)
               (subspace 0))
      (if (null? indices)
          (error 'nstore "Impossible, there is always a matching index" pattern)
          (if (permutation-prefix? combination (car indices))
              (values (car indices) subspace)
              (loop (cdr indices) (+ subspace 1)))))))

(define (pattern->prefix pattern index)
  ;; Return the list that correspond to INDEX, that is the items
  ;; of PATTERN that are not variables. This is used as the prefix
  ;; for the range query done later.
  (let loop ((index index)
             (out '()))
    (let ((v (list-ref pattern (car index))))
      (if (nstore-var? v)
          (reverse out)
          (loop (cdr index) (cons v out))))))

(define gmap
  (lambda (proc g)
    (lambda ()
      (let ((object (g)))
        (if (eof-object? object)
            (eof-object)
            (proc object))))))

(define (nstore-from transaction nstore pattern seed)
  (call-with-values (lambda () (pattern->index pattern (nstore-indices nstore)))
    (lambda (index subspace)
      (define pattern-prefix (pattern->prefix pattern index))
      (define prefix (let ((out (byter-write
                                 (list->vector
                                  (append (list (nstore-prefix nstore)
                                                subspace)
                                          pattern-prefix)))))
                       (byter-slice out 0 (- (bytevector-length out) 1))))

      (gmap (lambda (pair)
              (bind* pattern
                     (make-tuple (cddr (vector->list (byter-read (car pair)))) index)
                     seed))
            (okvs-query transaction
                        prefix
                        (byter-next-prefix prefix))))))

(define (pattern-bind pattern seed)
  ;; Return a pattern where variables that have a binding in SEED
  ;; are replaced with the associated value. In practice, most of
  ;; the time, it is the same pattern with less variables.
  (map (lambda (item)
         (or (and (nstore-var? item)
                  (and (assq (nstore-var-name item) seed)
                       (cdr (assq (nstore-var-name item) seed))))
             item))
       pattern))

(define (gconcatenate generator)
  ;; Return a generator that yields the elements of the generators
  ;; produced by the given GENERATOR. Similar to gflatten but
  ;; GENERATOR contains other generators instead of lists.
  (let ((state eof-object))
    (lambda ()
      (let ((value (state)))
        (if (eof-object? value)
            (let loop ((new (generator)))
              (if (eof-object? new)
                  new
                  (let ((value (new)))
                    (if (eof-object? value)
                        (loop (generator))
                        (begin (set! state new)
                               value)))))
            value)))))

(define nstore-where
  (lambda (transaction nstore pattern from)
    (gconcatenate
     (gmap (lambda (bindings) (nstore-from transaction
                                           nstore
                                           (pattern-bind pattern bindings)
                                           bindings))
           from))))

(define nstore-ref
  (lambda (transaction nstore items)
    ;; indices are sorted in lexicographic order, that is the
    ;; first index is always (iota n) (also known as the base
    ;; index). So that there is no need to permute ITEMS.  zero in
    ;; the following `list` is the index of the base subspace in
    ;; nstore-indices

    (let* ((key (byter-write (list->vector (append (list (nstore-prefix nstore) 0) items)))))
       (okvs-query transaction key))))

(define nstore-query
  (lambda (transaction nstore patterns)
    (if (null? patterns)
        (eof-object)
        (let loop ((g (nstore-from transaction nstore (car patterns) '()))
                   (patterns (cdr patterns)))
          (if (null? patterns)
              g
              (loop (nstore-where transaction nstore (car patterns) g)
                    (cdr patterns)))))))
