;; Copyright © 2023 Amirouche A. BOUBEKKI <amirouche at hyper dev>

(define-record-type* <lbst>
  (make-lbst-base key key-bytes value value-bytes length left right parent)
  lbst?
  ;; TODO: why is bytes per keys, and per values?
  (key lbst-key)
  (key-bytes lbst-key-bytes) ;; recursive sum, except where value == #f
  (value lbst-value)
  (value-bytes lbst-value-bytes) ;; recursive sum, mind value == #f
  (length lbst-length-base) ;; recursive length
  (left lbst-left)
  (right lbst-right)
  (parent lbst-parent lbst-parent!))

(define make-lbst
  (lambda ()
    (make-lbst-base #f 0 #f 0 0 #f #f #f)))

(define lbst-empty?
  (lambda (lbst)
    (or (not lbst) (not (lbst-key lbst)))))

(define lbst-bytes
  (case-lambda
    ((lbst) (if (lbst-empty? lbst) 0
                (+ (lbst-key-bytes lbst)
                   (lbst-value-bytes lbst))))
    ((lbst key other)

     ;; TODO: check KEY, and OTHER and replace with lbst-start, and
     ;; lbst-end when possible.

     ;; (define start (lbst-start lbst))
     ;; (define key* (case (byter-compare (lbst-key key) start)
     ;;                (smaller start)
     ;;                (equal start)
     ;;                (bigger key)))
     ;; (define end (lbst-end lbst))
     ;; (define other* (case (byter-compare (lbst-key key) end)
     ;;                  (smaller key)
     ;;                  (equal end)
     ;;                  (bigger end)))
     ;; (pk 'lbst-bytes 'key (and lbst (lbst-key lbst)))

     (if (not lbst) 0
         (match (cons (byter-compare (lbst-key lbst) (lbst-key key))
                      (byter-compare (lbst-key lbst) (lbst-key other)))
       ((bigger . bigger) (lbst-bytes (lbst-left lbst) key other))
       ((bigger . equal) (lbst-bytes (lbst-left lbst) key other))
       ((bigger . smaller)
        (+ (bytevector-length (lbst-key lbst))
           (bytevector-length (lbst-value lbst))
           (lbst-bytes (lbst-left lbst)
                       key
                       other)
           (lbst-bytes (lbst-right lbst)
                       key
                       other)))
       ((equal . bigger) (error 'lbst "Invalid bytes query" 1))
       ((equal . equal) (error 'lbst "Invalid bytes query" 2))
       ((equal . smaller) (+ (bytevector-length (lbst-key lbst))
                             (bytevector-length (lbst-value lbst))
                             (lbst-bytes (lbst-right lbst)
                                         key
                                         other)))
       ((smaller . bigger) (error 'lbst "Invalid bytes query" 3))
       ((smaller . equal) (error 'lbst "Invalid bytes query" 4))
       ((smaller . smaller) (lbst-bytes (lbst-right lbst) key other)))))))


(define fixnum-bit-length fxlength)

(define lbst<?
  (lambda (a b)
    (fx<? (fixnum-bit-length a) (fixnum-bit-length b))))

(define lbst-debug
  (lambda (lbst)
    (pk 'lbst-debug (lbst-key lbst) 'index (lbst-index lbst) (when (lbst-parent lbst) (lbst-index (lbst-parent lbst))))
    (pk 'lbst-debug (lbst-key lbst)
        (cons 'parent (and (lbst-parent lbst)
                           (lbst-key (lbst-parent lbst)))))
    (pk 'lbst-debug (lbst-key lbst)
        (cons 'left
              (if (lbst-left lbst)
                  (lbst-key (lbst-left lbst))
                  #f)))
    (pk 'lbst-debug (lbst-key lbst)
        (cons 'right
              (if (lbst-right lbst)
                  (lbst-key (lbst-right lbst))
                  #f)))
    (when (lbst-left lbst)
      (lbst-debug (lbst-left lbst)))
    (when (lbst-right lbst)
      (lbst-debug (lbst-right lbst)))))

(define lbst-length
  (lambda (x)
    (or (and x (lbst-length-base x)) 0)))

(define lbst-too-big?
  (lambda (a b)

    (define too-big?
      (lambda (a b)
        (lbst<? a (fxarithmetic-shift-right b 1))))

    (and a b
         (too-big? (lbst-length a) (lbst-length b)))))

(define lbst-join
  (lambda (key value left right)
    (define parent
      (make-lbst-base key
                      (fx+ (bytevector-length key)
                           (or (and left (lbst-key-bytes left)) 0)
                           (or (and right (lbst-key-bytes right)) 0))
                      value
                      ;; When VALUE is not a bytevector, record one byte.
                      (fx+ (or (and (bytevector? value) (bytevector-length value)) 1)
                           (or (and left (lbst-value-bytes left)) 0)
                           (or (and right (lbst-value-bytes right)) 0))
                      ;; When VALUE is not a bytevector, take it into account too.
                      (fx+ (lbst-length left)
                           (lbst-length right)
                           1)
                      left
                      right
                      #f))
    (and left (lbst-parent! left parent))
    (and right (lbst-parent! right parent))
    parent))

(define lbst-single-left-rotation
  (lambda (key value left right)
    (define new-left (lbst-join key value left (lbst-left right)))
    (define parent (lbst-join (lbst-key right)
                              (lbst-value right)
                              new-left
                              (lbst-right right)))
    parent))

(define lbst-double-left-rotation
  (lambda (key value left right)
    (define new-left (lbst-join key
                                value
                                left
                                (lbst-left (lbst-left right))))
    (define new-right (lbst-join (lbst-key right)
                                 (lbst-value right)
                                 (lbst-right (lbst-left right))
                                 (lbst-right right)))
    (define parent (lbst-join (lbst-key (lbst-left right))
                              (lbst-value (lbst-left right))
                              new-left
                              new-right))
    parent))

(define lbst-single-right-rotation
  (lambda (key value left right)
    (define new-right (lbst-join key
                                 value
                                 (lbst-right left)
                                 right))
    (define parent (lbst-join (lbst-key left)
                              (lbst-value left)
                              (lbst-left left)
                              new-right))
    parent))

(define lbst-double-right-rotation
  (lambda (key value left right)
    (define new-left (lbst-join (lbst-key left)
                                (lbst-value left)
                                (lbst-left left)
                                (lbst-left (lbst-right left))))
    (define new-right (lbst-join key
                                 value
                                 (lbst-right (lbst-right left))
                                 right))
    (define parent (lbst-join (lbst-key (lbst-right left))
                              (lbst-value (lbst-right left))
                              new-left
                              new-right))
    parent))

(define lbst-rebalance
  (lambda (key value left right)
    (if (lbst-too-big? left right)
        (if (not (lbst-too-big? (lbst-right right)
                                (lbst-left right)))
            (lbst-single-left-rotation key value left right)
            (lbst-double-left-rotation key value left right))
        (if (lbst-too-big? right left)
            (if (not (lbst-too-big? (lbst-left left)
                                    (lbst-right left)))
                (lbst-single-right-rotation key value left right)
                (lbst-double-right-rotation key value left right))
            ;; otherwise join both trees with a top level node
            (lbst-join key value left right)))))

(define lbst-set
  (lambda (lbst key value)
    (if (lbst-empty? lbst)
        (make-lbst-base key
                        (bytevector-length key)
                        value
                        (or (and (bytevector? value) (bytevector-length value)) 1)
                        1
                        #f
                        #f
                        #f)
        (case (byter-compare key (lbst-key lbst))
          (smaller (lbst-rebalance (lbst-key lbst)
                                   (lbst-value lbst)
                                   (lbst-set (lbst-left lbst) key value)
                                   (lbst-right lbst)))
          (equal (lbst-join key value (lbst-left lbst) (lbst-right lbst)))
          (bigger (lbst-rebalance (lbst-key lbst)
                                  (lbst-value lbst)
                                  (lbst-left lbst)
                                  (lbst-set (lbst-right lbst) key value)))
          (else (error 'lbst "Unexpected byter-compare return value"))))))

(define lbst-balanced?
  (lambda (lbst)
    ;; Of course the empty tree is balanced, there is nothing!
    (if (lbst-empty? lbst)
        #t
        ;; Check left, and right subtrees are balanced, and recurse.
        ;; There is two calls to lbst-too-big? because it is not
        ;; associative ie. (x? a b) may not be equal to (x? b a).
        (and (not (lbst-too-big? (lbst-left lbst) (lbst-right lbst)))
             (not (lbst-too-big? (lbst-right lbst) (lbst-left lbst)))
             (lbst-balanced? (lbst-left lbst))
             (lbst-balanced? (lbst-right lbst))))))

(define lbst-start
  (lambda (lbst)
    (if (lbst-empty? (lbst-left lbst))
        lbst
        (lbst-start (lbst-left lbst)))))

(define lbst-end
  (lambda (lbst)
    (if (lbst-empty? (lbst-right lbst))
        lbst
        (lbst-end (lbst-right lbst)))))

(define lbst-next
  (lambda (lbst)
    (if (lbst-empty? (lbst-right lbst))
        (let loop ((lbst lbst)
                   (parent (lbst-parent lbst)))
          (if (not parent)
              #f
              (if (eq? lbst (lbst-left parent))
                  parent
                  (loop parent (lbst-parent lbst)))))
        (lbst-start (lbst-right lbst)))))

(define lbst-previous
  (lambda (lbst)
    (if (lbst-empty? (lbst-left lbst))
        (let loop ((lbst lbst)
                   (parent (lbst-parent lbst)))
          (if (not parent)
              #f
              (if (eq? lbst (lbst-right parent))
                  parent
                  (loop parent (lbst-parent lbst)))))
        (lbst-end (lbst-left lbst)))))

(define lbst->alist
  (lambda (lbst)
    ;; traverse from end to start, hence avoid a reverse.
    (let loop ((lbst (lbst-end lbst))
               (out '()))
      (if (not lbst)
          out
          (loop (lbst-previous lbst)
                (cons (cons (lbst-key lbst) (lbst-value lbst)) out))))))

(define alist->lbst
  (lambda (alist)
    (let loop ((alist alist)
               (lbst (make-lbst)))
      (if (null? alist)
          lbst
          (loop (cdr alist)
                (lbst-set lbst (caar alist) (cdar alist)))))))

(define call-with-lbst
  (lambda (lbst key proc)

    ;; TODO: Check that the search strategy is 'exact, or 'before
    ;; otherwise 'after.

    (define lbst-search
      (lambda (lbst key)
        (if (lbst-empty? lbst)
            (values #f #f)
            (case (byter-compare key (lbst-key lbst))
              ((smaller)
               (if (lbst-empty? (lbst-left lbst))
                   (values lbst 'after)
                   (lbst-search (lbst-left lbst) key)))
              ((bigger)
               (if (lbst-empty? (lbst-right lbst))
                   (values lbst 'before)
                   (lbst-search (lbst-right lbst) key)))
              (else (values lbst 'exact))))))

    (call-with-values (lambda () (lbst-search lbst key)) proc)))

(define lbst-ref
  (lambda (lbst key)
    (call-with-lbst lbst key
      (lambda (lbst position)
        (if (eq? position 'exact)
            lbst
            #f)))))

(define (lbst-dot lbst start end)

  (define within?
    (lambda (a b c)
      (case (byter-compare b a)
        (smaller #f)
        (else (case (byter-compare b c)
                (bigger #f)
                ((smaller equal) #t))))))

  (define lbst-key-name
    (lambda (lbst)
      (format #f "\"~a\"" (lbst-key lbst))))

  (display "digraph g {")

  (let loop ((todo (list lbst)))
    (unless (null? todo)
      (let ((lbst (car todo)))
        (if (not lbst)
            (loop (cdr todo))
            (begin
              (when (within? start (lbst-key lbst) end)
                (display "\t")
                (display (lbst-key-name lbst))
                (if (or (equal? start (lbst-key lbst))
                        (equal? end (lbst-key lbst)))
                    (display " [color=\"red\"]")
                    (display " [color=\"green\"]"))
                (display ";")
                (newline))

              (when (lbst-left lbst)
                (display "\t")
                (display (lbst-key-name lbst))
                (display " -> ")
                (display (lbst-key-name (lbst-left lbst)))
                (display " [label=left]")
                (display ";")
                (newline))

              (when (lbst-right lbst)
                (display "\t")
                (display (lbst-key-name lbst))
                (display " -> ")
                (display (lbst-key-name (lbst-right lbst)))
                (display " [label=right]")
                (display ";")
                (newline))

              (loop (cons* (lbst-left lbst)
                           (lbst-right lbst)
                           (cdr todo))))))))

  (display "\n}")
  (flush-output-port)
  (newline))

(define lbst-delete
  (lambda (lbst key)

    (define lbst-delete-min
      (lambda (lbst)
        (if (lbst-empty? (lbst-left lbst))
            (lbst-right lbst)
            (lbst-rebalance (lbst-key lbst)
                            (lbst-value lbst)
                            (lbst-delete-min (lbst-left lbst))
                            (lbst-right lbst)))))

    (define lbst-concat3
      (lambda (key value left right)
        (if (lbst-empty? left)
            (unparent! (lbst-set right key value))
            (if (lbst-empty? right)
                (unparent! (lbst-set left key value))
                (cond
                 ((lbst-too-big? left right)
                  (lbst-rebalance (lbst-key right)
                                  (lbst-value right)
                                  (lbst-concat3 key
                                                value
                                                left
                                                (lbst-left right))
                                  (lbst-right right)))
                 ((lbst-too-big? right left)
                  (lbst-rebalance (lbst-key left)
                                  (lbst-value left)
                                  (lbst-left left)
                                  (lbst-concat3 key
                                                value
                                                (lbst-right left)
                                                right)))
                 (else (lbst-join key value left right)))))))

    (define unparent!
      (lambda (x)
        (and x (lbst-parent! x #f))
        x))

    (define lbst-concat2
      (lambda (lbst other)
        (if (lbst-empty? lbst)
            (unparent! other)
            (if (lbst-empty? other)
                (unparent! lbst)
                (let ((min (lbst-start other)))
                  (lbst-concat3 (lbst-key min)
                                (lbst-value min)
                                lbst
                                (lbst-delete-min other)))))))

    (if (lbst-empty? lbst)
        lbst
        (case (byter-compare key (lbst-key lbst))
          (bigger (lbst-rebalance (lbst-key lbst)
                                  (lbst-value lbst)
                                  (lbst-left lbst)
                                  (lbst-delete (lbst-right lbst) key)))
          (smaller (lbst-rebalance (lbst-key lbst)
                                   (lbst-value lbst)
                                   (lbst-delete (lbst-left lbst) key)
                                   (lbst-right lbst)))
          (else (lbst-concat2 (lbst-left lbst) (lbst-right lbst)))))))

(define compose
  (lambda args
    (lambda (x)
      (fold (lambda (x y) (x y)) x args))))

(define integer->bytevector
  (lambda (n)
    (define bv (make-bytevector 8))
    (bytevector-u64-set! bv 0 n 'big)
    bv))

(define lbst->inode
  (lambda (lbst index)

    (define fields
      (list
       (cons 'index (compose (lambda (x) index)
                             integer->bytevector))
       (cons 'key-length (compose lbst-key
                                  bytevector-length
                                  integer->bytevector))
       (cons 'key lbst-key)
       (cons 'tomb (compose lbst-value
                            not
                            (lambda (x) (if x (- (expt 2 64) 1) 0))
                            integer->bytevector))
       (cons 'bytes (compose lbst-bytes
                             integer->bytevector))
       (cons 'length (compose lbst-length
                              integer->bytevector))))

    (byter-concatenate (map (lambda (i p) ((cdr p) lbst))
                            (iota (length fields))
                            fields))))

(define make-lbst-inode-ref
  (lambda (field bytevector offset)

    (define inode-ref
      (lambda (field)
        (lambda (bytevector offset)
          ((cdr (assq field fields)) bytevector offset))))

    (define fields
      (list
       (cons 'index
             (lambda (bv offset)
               (bytevector-u64-ref bv offset 'big)))

       (cons 'key-length
             (lambda (bv offset)
               (bytevector-u64-ref bv (+ offset 8) 'big)))

       (cons 'key
             (lambda (bv offset)
               (byter-slice bv
                            (+ offset 8 8)
                            (+ offset 8 8
                               ((inode-ref 'key-length) bv offset)))))

       (cons 'tomb
             (lambda (bv offset)
               (bytevector-u64-ref bv
                                   (+ offset 8 8
                                      ((inode-ref 'key-length) bv offset))
                                   'big)))

       (cons 'bytes
             (lambda (bv offset)
               (bytevector-u64-ref bv
                                   (+ offset 8 8
                                      ((inode-ref 'key-length) bv offset)
                                      8)
                                   'big)))

       (cons 'length
             (lambda (bv offset)
               (bytevector-u64-ref bv
                                   (+ offset 8 8
                                      ((inode-ref 'key-length) bv offset)
                                      8 8)
                                   'big)))

       (cons 'parent
             (lambda (bv offset)
               (bytevector-u64-ref bv
                                   (+ offset 8 8
                                      ((inode-ref 'key-length) bv offset)
                                      8 8 8)
                                   'big)))

       ;; left child
       (cons 'left
             (lambda (bv offset)
               (bytevector-u64-ref bv
                                   (+ offset 8 8
                                      ((inode-ref 'key-length) bv offset)
                                      8 8 8 8)
                                   'big)))

       ;; right child
       (cons 'right
             (lambda (bv offset)
               (bytevector-u64-ref bv
                                   (+ offset 8 8
                                      ((inode-ref 'key-length) bv offset)
                                      8 8 8 8 8)
                                   'big)))))

    ((inode-ref field) bytevector offset)))

(define lbst-inode-index-ref
  (lambda (bytevector offset)
    (make-lbst-inode-ref 'index bytevector offset)))

(define lbst-inode-key-ref
  (lambda (bytevector offset)
    (make-lbst-inode-ref 'key bytevector offset)))

(define lbst-inode-tomb-ref
  (lambda (bytevector offset)
    (not (= 0 (make-lbst-inode-ref 'tomb bytevector offset)))))

(define lbst-inode-bytes-ref
  (lambda (bytevector offset)
    (make-lbst-inode-ref 'bytes bytevector offset)))

(define lbst-inode-length-ref
  (lambda (bytevector offset)
    (make-lbst-inode-ref 'length bytevector offset)))

(define lbst-inode-parent-ref
  (lambda (bytevector offset)
    (make-lbst-inode-ref 'length bytevector offset)))

(define lbst-inode->alist
  (lambda (bytevector offset)
    (map (lambda (x) (cons x (make-lbst-inode-ref x bytevector offset)))
         '(index key tomb length bytes parent))))

#;(define ~check-lbst-020
  (lambda ()
    (define lbst (make-lbst-base (bytevector 42) 1
                                 (bytevector 101) 1
                                 1 #f #f #f))
    (equal? (bytevector 0 0 0 0 0 0 0 20
                        0 0 0 0 0 0 0 1
                        42
                        0 0 0 0 0 0 0 0
                        0 0 0 0 0 0 0 2
                        0 0 0 0 0 0 0 1)
            (lbst->inode lbst 20))))

(define lbst-index
  (lambda (lbst)
    (if (not (lbst-parent lbst))
        1
        (let ((index (lbst-index (lbst-parent lbst))))
          (if (eq? lbst (lbst-left (lbst-parent lbst)))
              (* 2 index)
              (+ (* 2 index) 1))))))

(define lbst->lnode
  (lambda (lbst index)

    (define fields
      (list
       (cons 'index (compose (lambda _ index)
                             integer->bytevector))

       (cons 'key-length (compose lbst-key
                                  bytevector-length
                                  integer->bytevector))

       (cons 'key lbst-key)

       (cons 'tomb (compose lbst-value
                            not
                            (lambda (x) (if x (- (expt 2 64) 1) 0))
                            integer->bytevector))

       (cons 'value-length (compose lbst-value
                                    (lambda (x)
                                      (or (and x (bytevector-length x))
                                          x))
                                    integer->bytevector))

       (cons 'value (compose lbst-value
                             (lambda (bv) (or bv (bytevector)))))))

    (byter-concatenate (map (lambda (i p) ((cdr p) lbst))
                            (iota (length fields))
                            fields))))

#;(define ~check-lbst-021
  (lambda ()
    (define lbst (make-lbst-base (bytevector 13 37) 1
                                 (bytevector 4 21) 1
                                 1 #f #f #f))
    (equal? (bytevector 0 0 0 0 0 0 0 41
                        0 0 0 0 0 0 0 2
                        13 37
                        0 0 0 0 0 0 0 0
                        0 0 0 0 0 0 0 2
                        4 21)
            (lbst->lnode lbst 41))))

(define lbst-fold
  (lambda (lbst seed* proc)

    (define f
      (lambda (lbst seed proc)
        (if (not lbst)
            seed
            (f (lbst-next lbst) (proc lbst seed) proc))))

    (f (lbst-start lbst) seed* proc)))

(define lbst->notebook
  (lambda (lbst notebook)

    #;(define x (pk 'chapter-x (make-dbx-chapter-x dbx)))

    #;(define write
      (let ((payload (bytevector)))
        (lambda (bytevector)
          (if (eof-object? bytevector)
              (dbx-chapter-x-append! dbx
                                     (byter-append payload
                                                   (make-bytevector
                                                    (- (dbx-page-bytes dbx)
                                                       (bytevector-length payload))
                                                    0)))
              (let ((payload*
                     (byter-append payload bytevector)))
                (if (<= (dbx-page-bytes dbx)
                        (bytevector-length payload*))
                    (begin
                      (dbx-chapter-x-append!
                       dbx (byter-slice payload* 0
                                        (dbx-page-bytes dbx)))
                      (set! payload
                            (byter-slice payload*
                                         (dbx-page-bytes dbx)
                                         (bytevector-length payload*))))
                    (set! payload payload*)))))))

    (define inode-indexes (make-hashtable equal-hash equal?))
    (define lnode-indexes (make-hashtable equal-hash equal?))

    (define inodes
      (byter-concatenate
       (reverse
        (car
         (lbst-fold lbst
                    (list '() 0)
                    (lambda (lbst o)
                      (define-values (inodes roffset) (apply values o))
                      (define index (lbst-index lbst))
                      (define inode (lbst->inode lbst index))
                      (define roffset* (+ roffset (bytevector-length inode)))
                      (hashtable-set! inode-indexes index roffset)
                      (list (cons inode inodes) roffset*)))))))

    (define lnodes
      (byter-concatenate
       (reverse (car
        (lbst-fold lbst
                   (list '() 0)
                   (lambda (lbst o)
                     (define-values (lnodes roffset) (apply values o))
                     (define index (lbst-index lbst))
                     (define lnode (lbst->lnode lbst (lbst-index lbst)))
                     (define roffset* (+ roffset
                                         (bytevector-length lnode)))
                     (hashtable-set! lnode-indexes index roffset)
                     (list (cons lnode lnodes) roffset*)))))))

    (define inode-map
      (let* ((length (apply max
                            (vector->list
                             (hashtable-keys inode-indexes))))
             (out (make-vector length
                               (integer->bytevector (- (expt 2 64) 1)))))
        (let loop ((kvs (map cons
                             (vector->list (hashtable-keys inode-indexes))
                             (vector->list (hashtable-values inode-indexes)))))
          (if (null? kvs)
              (byter-concatenate (vector->list out))
              (let ((index (caar kvs))
                    (offset (cdar kvs)))
                (vector-set! out
                             (fx- index 1)
                             (integer->bytevector offset))
                (loop (cdr kvs)))))))

    (define lnode-map
      (let* ((length (apply max
                            (vector->list
                             (hashtable-keys lnode-indexes))))
             (out (make-vector length
                               (integer->bytevector (- (expt 2 64) 1)))))
        (let loop ((kvs (map cons
                             (vector->list (hashtable-keys lnode-indexes))
                             (vector->list (hashtable-values lnode-indexes)))))
          (if (null? kvs)
              (byter-concatenate (vector->list out))
              (let ((index (caar kvs))
                    (offset (cdar kvs)))
                (vector-set! out
                             (fx- index 1)
                             (integer->bytevector offset))
                (loop (cdr kvs)))))))

    ;; (write map*)
    ;; (write inodes)
    ;; (write lnodes)

    (byter-concatenate
     (list (integer->bytevector (lbst-length lbst))
           inode-map
           (integer->bytevector (bytevector-length inodes))
           inodes
           lnode-map
           lnodes))))

;; (define lbst-bytevector-start
;;   (lambda (bv)
;;     (call-with-values
;;         (lambda ()
;;           (lbst-bytevector-ref bv
;;                                (pk 'length (bytevector-u64-ref bv 0 'big))))
;;       (lambda (k v o) k))))

;; (define lbst-bytevector-end
;;   (lambda (bv)
;;     (call-with-values (lambda ()
;;                         (lbst-bytevector-ref bv
;;                                              (bytevector-u64-ref bv 8 'big)))
;;       (lambda (k v o) k))))

(define lbst-inode-ref
  (lambda (bytevector index)
    (list (cons 'index index)
          (cons 'key (lbst-field-ref bytevector 'inode-key index))
          (cons 'tomb (lbst-field-ref bytevector 'inode-tomb index))
          (cons 'bytes (lbst-field-ref bytevector 'inode-bytes index))
          (cons 'length (lbst-field-ref bytevector 'inode-length index)))))

(define lbst-bytevector->alist
  (lambda (bytevector)
    ;; TODO: implement after lnode->alist
    bytevector))

(define lbst-field-ref
  (lambda (bytevector name . args)
    (pk 'lbst-field-ref 'in name)
    (apply (cdr (assq name lbst-bytevector-fields)) bytevector args)))

(define lbst-bytevector-fields
  (list

   (cons 'map-length
         (lambda (bytevector)
           (* 8 (bytevector-u64-ref bytevector 0 'big))))

   (cons 'lnodes-map-offset
         (lambda (bytevector)

           (define map-length
             (lbst-field-ref bytevector 'map-length))

           (define inodes-length
             (bytevector-u64-ref bytevector (pk 'offset (+ 8 map-length)) 'big))

           (+ 8 map-length 8 inodes-length)))

   (cons 'lnodes-start
         (lambda (bytevector)

           (define map-length
             (lbst-field-ref bytevector 'map-length))

           (define lnodes-map-offset
             (lbst-field-ref bytevector 'lnodes-map-offset))

           (+ lnodes-map-offset map-length)))

   (cons 'lnodes-index
         (lambda (bytevector index)

           (define map-length
             (lbst-field-ref bytevector 'map-length))

           (define inodes-length
             (bytevector-u64-ref bytevector (+ 8 map-length) 'big))

           (+ 8 map-length 8 inodes-length)))

   (cons 'map
         (lambda (bytevector)
           (define length
             (lbst-field-ref bytevector 'map-length))

           (map (lambda (i)
                  (lbst-field-ref bytevector
                                  'inode-offset
                                  i))
                (cdr (iota (+ (/ length 8) 1))))))

   (cons 'inode-offset
         (lambda (bytevector index)

           (define map-length
             (lbst-field-ref bytevector
                             'map-length))

           (define roffset
             (bytevector-u64-ref bytevector (* 8 index) 'big))

           (if (= (- (expt 2 64) 1) roffset)
               #f
               (+ 8 ;; map block length
                  map-length
                  8
                  roffset))))

   (cons 'inode-index
         (lambda (bytevector index)
           (bytevector-u64-ref bytevector
                               (lbst-field-ref bytevector
                                               'inode-offset
                                               index)
                               'big)))

   (cons 'inode-key-length
         (lambda (bytevector index)

           (define offset
             (lbst-field-ref bytevector
                             'inode-offset
                             index))

           (bytevector-u64-ref bytevector
                               (+ offset 8)
                               'big)))

   (cons 'inode-key
         (lambda (bytevector index)
           (define offset
             (lbst-field-ref bytevector
                             'inode-offset
                             index))

           (define key-length
             (lbst-field-ref bytevector
                             'inode-key-length
                             index))

           (byter-slice bytevector
                        (+ offset 8 8)
                        (+ offset 8 8 key-length))))

   (cons 'inode-tomb
         (lambda (bytevector index)
           (define offset
             (lbst-field-ref bytevector
                             'inode-offset
                             index))

           (define key-length
             (lbst-field-ref bytevector
                             'inode-key-length
                             index))

           (bytevector-u64-ref bytevector
                               (+ offset 8 8 key-length) 'big)))

   (cons 'inode-bytes
         (lambda (bytevector index)
           (define offset
             (lbst-field-ref bytevector
                             'inode-offset
                             index))

           (define key-length
             (lbst-field-ref bytevector
                             'inode-key-length
                             index))

           (bytevector-u64-ref bytevector
                               (+ offset 8 8 key-length 8) 'big)))


   (cons 'inode-length
         (lambda (bytevector index)
           (define offset
             (lbst-field-ref bytevector
                             'inode-offset
                             index))

           (define key-length
             (lbst-field-ref bytevector
                             'inode-key-length
                             index))

           (bytevector-u64-ref bytevector
                               (+ offset 8 8 key-length 8 8) 'big)))

   (cons 'lnode-offset
         (lambda (bytevector index)
           (define start
             (lbst-field-ref bytevector 'lnodes-start))

           (bytevector-u64-ref bytevector
                               (+ start (* 8 index))
                               'big)))

   (cons 'lnode-indexes
         (lambda (bytevector)

           (define list-ref
             (lambda (objects i)
               (if (null? objects)
                   #f
                   (if (zero? i)
                       (car objects)
                       (list-ref (cdr objects) (- i 1))))))

           (define length (lbst-field-ref bytevector
                                          'map-length))

           (define i*
             (map (lambda (i)
                    (if (lbst-field-ref bytevector
                                        'lnode-offset
                                        i)
                        (cons i #t)
                        (cons i #f)))
                  (cdr (iota (+ (/ length 8) 1)))))

           (define t*
             (let loop ((i 1))
               (let ((o (list-ref i* (- i 1))))
                 (if (or (not o) (not (cdr o)))
                     #f
                     (list i (loop (* 2 i)) (loop (+ (* 2 i) 1)))))))

           (let loop ((t* t*))
             (if (or (not t*) (null? t*))
                 '()
                 (append (loop (cadr t*)) (list (car t*)) (loop (caddr t*)))))))

   (cons 'inode-indexes
         (lambda (bytevector)

           (define list-ref
             (lambda (objects i)
               (if (null? objects)
                   #f
                   (if (zero? i)
                       (car objects)
                       (list-ref (cdr objects) (- i 1))))))

           (define lengthx
             (lbst-field-ref bytevector 'map-length))

           (define i*
             (map (lambda (i)
                    (if (lbst-field-ref bytevector
                                        'inode-offset
                                        i)
                        (cons i #t)
                        (cons i #f)))
                  (cdr (iota (+ (/ lengthx 8) 1)))))

           (define t*
             (let loop ((i 1))
               (let ((o (list-ref i* (- i 1))))
                 (if (or (not o) (not (cdr o)))
                     #f
                     (list i (loop (* 2 i)) (loop (+ (* 2 i) 1)))))))

           (let loop ((t* t*))
             (if (or (not t*) (null? t*))
                 '()
                 (append (loop (cadr t*)) (list (car t*)) (loop (caddr t*)))))))))

(define make-lbst-write-check
  (lambda (seed)
    (define r (random-seed (pk 'LETLOOP_SEED= seed)))
    (define lbst (make-random-lbst (+ 3 #;(random 100))
                                   (+ 3 #;(random 10))))
    (define dbx #f #;(make-dbx (format #f "check-~a.dbx" seed)
      2048))

    ;; (define alist (pk (lbst->alist lbst)))
    (define bv (lbst->notebook lbst dbx))

    ;; (pk 'map (lbst-field-ref bv 'map))

    ;;
    ;; (pk 'inodes
    ;;     (map (lambda (x i)
    ;;            (pk x i)
    ;;         (if x
    ;;             (lbst-inode-ref bv i)
    ;;             #f))
    ;;       (lbst-field-ref bv 'map)
    ;;       (iota (/ (lbst-field-ref bv 'map-length) 8))))
    ;; (pk 'oops (lbst->alist lbst))
    ;;

    ;; (pk 'LNODES-MAP-OFFSET
    ;;     (lbst-field-ref bv 'lnodes-map-offset))

    ;; (pk 'LNODES-START
    ;;     (lbst-field-ref bv 'lnodes-start))


    ;; (pk 'LNODE-START-INDEX
    ;;     (bytevector-u64-ref bv (lbst-field-ref bv 'lnode-start) 'big))

    (assert (pk 'assert (equal? (pk 'given (lbst-inode-ref bv 1))
                                (pk 'expected (list
                                               (cons 'index 1)
                                               (cons 'key (lbst-key lbst))
                                               (cons 'tomb 0)
                                               (cons 'bytes (lbst-bytes lbst))
                                               (cons 'length (lbst-length lbst)))))))

    ))

#;(define ~check-lbst-022
  (lambda ()
    (import (letloop entangle))
    (random-seed (string->number (or (getenv "LETLOOP_SEED") "1")))
    (with-entangle
     (pk 'ooo (make-lbst-write-check (random (- (expt 2 32) 1))))
     (pk 'fuuu))))

#;(define ~check-lbst-022/random
  (lambda ()
    (random-seed (string->number (or (getenv "LETLOOP_SEED") "1")))
    (for-each (lambda _
                (make-lbst-write-check (random (- (expt 2 32) 1))))
              (iota (or (and (getenv "LETLOOP_SEED") 1) 1000)))))

#;(define ~check-lbst-023
  (lambda ()
    (import (letloop dxdb dbx))
    (import (letloop entangle))
    (define n 100)
    (define lbst (make-random-lbst n n))
    #t))
