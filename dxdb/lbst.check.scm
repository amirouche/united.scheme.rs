;; Copyright © 2023 Amirouche A. BOUBEKKI <amirouche at hyper dev>

(define ~check-lbst-000
  (lambda ()
    (check (lbst-empty? (make-lbst)))))

(define random-alist
  (lambda (magic)
    (map (lambda _ (cons (random-bytevector magic)
                         (random-bytevector magic)))
         (iota magic))))

(define ~check-lbst-000-set-false
  (lambda ()
    (let* ((init (alist->lbst (random-alist 42)))
           (out (lbst-set init (bytevector 42) #f)))
      (assert (and
               (if (lbst-ref init (bytevector 42))
                   (= (lbst-length out) 42)
                   (= (lbst-length out) 43))
               (not (lbst-value (lbst-ref out (bytevector 42)))))))))

(define ~check-lbst-000-set-not-bytevector
  (lambda ()
    (define ruse (lambda () ruse))

    (let* ((init (alist->lbst (random-alist 42)))
           (out (lbst-set init (bytevector 42) ruse)))
      (assert (and
               (if (lbst-ref init (bytevector 42))
                   (= (lbst-length out) 42)
                   (= (lbst-length out) 43))
               (eq? ruse (lbst-value (lbst-ref out (bytevector 42)))))))))

(define ~check-lbst-001
  (lambda ()
    (define alist (random-alist 8))
    (define expected (sort (lambda (x y) (bytevector<? (car x) (car y)))
                           alist))
    (define given (lbst->alist (alist->lbst alist)))
    (assert (equal? expected given))))

(define ~check-lbst-002
  (lambda ()
    (let* ((lbst (make-lbst))
           (lbst (lbst-set lbst (bytevector 42) (bytevector 42)))
           (lbst (lbst-set lbst (bytevector 13) (bytevector 13)))
           (lbst (lbst-set lbst (bytevector 14) (bytevector 14)))
           (lbst (lbst-set lbst (bytevector 101) (bytevector 101)))
           )
      (check (equal? (lbst->alist lbst)
                      `((,(bytevector 13) . ,(bytevector 13))
                        (,(bytevector 14) . ,(bytevector 14))
                        (,(bytevector 42) . ,(bytevector 42))
                        (,(bytevector 101) . ,(bytevector 101))))))))

(define ~check-lbst-003
  (lambda ()
    (let* ((lbst (make-lbst))
           (lbst (lbst-set lbst (bytevector 42) (bytevector 42)))
           (lbst (lbst-set lbst (bytevector 13) (bytevector 13)))
           (lbst (lbst-set lbst (bytevector 14) (bytevector 14)))
           (lbst (lbst-set lbst (bytevector 101) (bytevector 101))))
      (check (fx<? (bytevector-u8-ref (lbst-value lbst) 0) (bytevector-u8-ref (lbst-value (lbst-next lbst)) 0))))))

(define ~check-lbst-004
  (lambda ()
    (let* ((lbst (make-lbst))
           (lbst (lbst-set lbst (bytevector 42) (bytevector 42)))
           (lbst (lbst-set lbst (bytevector 13) (bytevector 13)))
           (lbst (lbst-set lbst (bytevector 14) (bytevector 14)))
           (lbst (lbst-set lbst (bytevector 101) (bytevector 101))))
      (check (fx<? (bytevector-u8-ref (lbst-value lbst) 0) (bytevector-u8-ref (lbst-value (lbst-next lbst)) 0))))))

(define ~check-lbst-005
  (lambda ()
    (let* ((lbst (make-lbst))
           (lbst (lbst-set lbst (bytevector 42) (bytevector 42)))
           (lbst (lbst-set lbst (bytevector 13) (bytevector 13)))
           (lbst (lbst-set lbst (bytevector 14) (bytevector 14)))
           (lbst (lbst-set lbst (bytevector 101) (bytevector 101))))
      (let* ((a (lbst-value (lbst-previous lbst)))
             (b (lbst-value (lbst-next (lbst-previous lbst))))
             (c (lbst-value (lbst-previous (lbst-next lbst))))
             (d (lbst-value (lbst-next lbst))))
        (check (apply fx<=? (map (lambda (x) (bytevector-u8-ref x 0)) (list a b c d))))))))

(define ~check-lbst-006
  (lambda ()
    (let* ((lbst (make-lbst))
           (lbst (lbst-set lbst (bytevector 42) (bytevector 42)))
           (lbst (lbst-set lbst (bytevector 13) (bytevector 13)))
           (lbst (lbst-set lbst (bytevector 14) (bytevector 14)))
           (lbst (lbst-set lbst (bytevector 101) (bytevector 101))))
      (check (bytevector=? (lbst-value (lbst-start lbst)) (bytevector 13))))))

(define ~check-lbst-007
  (lambda ()
    (let* ((lbst (make-lbst))
           (lbst (lbst-set lbst (bytevector 42) (bytevector 42)))
           (lbst (lbst-set lbst (bytevector 13) (bytevector 13)))
           (lbst (lbst-set lbst (bytevector 14) (bytevector 14)))
           (lbst (lbst-set lbst (bytevector 101) (bytevector 101))))
      (check (bytevector=? (lbst-value (lbst-end lbst)) (bytevector 101))))))

;; same as lbst->alist but we start from the first key until the
;; end, and accumulate key-value pairs in a list, and return that
;; without the need to call reverse.
(define lbst->alist/reversed
  (lambda (lbst)
    (let loop ((lbst (lbst-start lbst))
               (out '()))
      (if (not lbst)
          out
          (loop (lbst-next lbst) (cons (cons (lbst-key lbst) (lbst-value lbst)) out))))))

(define ~check-lbst-008
  (lambda ()
    (let* ((lbst (make-lbst))
           (lbst (lbst-set lbst (bytevector 42) (bytevector 42)))
           (lbst (lbst-set lbst (bytevector 13) (bytevector 13)))
           (lbst (lbst-set lbst (bytevector 14) (bytevector 14)))
           (lbst (lbst-set lbst (bytevector 101) (bytevector 101))))

      (check (equal? (reverse (lbst->alist/reversed lbst)) (lbst->alist lbst))))))

(define ~check-lbst-009
  (lambda ()

    (let* ((lbst (make-lbst))
           (lbst (lbst-set lbst (bytevector 42) (bytevector 42)))
           (lbst (lbst-set lbst (bytevector 13) (bytevector 13)))
           (lbst (lbst-set lbst (bytevector 14) (bytevector 14)))
           (lbst (lbst-set lbst (bytevector 101) (bytevector 101))))

      (let loop ((vs (list 13 14 42 101)))
        (if (null? vs)
            #t
            (call-with-lbst lbst (bytevector (car vs))
              (lambda (lbst position)
                (check (eq? position 'equal))
                (check (= (bytevector-u8-ref (lbst-value lbst) 0) (car vs)))
                (loop (cdr vs)))))))))

(define position->comparison
  (lambda (x)
    (case x
      ((exact) 'equal)
      ((before) 'bigger)
      ((after) 'smaller))))

(define ~check-lbst-009-bis
  (lambda ()

    (let* ((lbst (make-lbst))
           (lbst (lbst-set lbst (bytevector 13 36) (bytevector 1)))
           (lbst (lbst-set lbst (bytevector 13 38) (bytevector 2)))
           (key (bytevector 13 37)))

      (call-with-lbst lbst key
        (lambda (lbst position)
          (check (eq? (position->comparison position)
                      (byter-compare key (lbst-key lbst)))))))))

(define ~check-lbst-009-ter
  (lambda ()

    (let* ((lbst (make-lbst))
           (lbst (lbst-set lbst (bytevector 13 38) (bytevector 2)))
           (lbst (lbst-set lbst (bytevector 13 36) (bytevector 1)))
           (key (bytevector 13 37)))

      (call-with-lbst lbst key
        (lambda (lbst position)
          (check (eq? (position->comparison position)
                      (byter-compare key (lbst-key lbst)))))))))

(define ~check-lbst-010
  (lambda ()
    (let* ((lbst (make-lbst))
           (lbst (lbst-set lbst (bytevector 42) (bytevector 42)))
           (lbst (lbst-set lbst (bytevector 13) (bytevector 13)))
           (lbst (lbst-set lbst (bytevector 14) (bytevector 14)))
           (lbst (lbst-set lbst (bytevector 101) (bytevector 101))))

      (let loop ((vs (list (list 7 'smaller 13)
                           (list 15 'bigger 14)
                           (list 208 'bigger 101)
                           (list 15 'bigger 14)
                           (list 13 'equal 13))))
        (if (null? vs)
            #t
            (call-with-lbst lbst (bytevector (list-ref (car vs) 0))
              (lambda (lbst position)

                (assert (eq? (list-ref (car vs) 1)
                             (byter-compare (bytevector
                                             (list-ref (car vs) 0))
                                            (lbst-key lbst))))
                (assert (= (bytevector-u8-ref (lbst-value lbst) 0)
                           (list-ref (car vs) 2)))
                (loop (cdr vs)))))))))

(define ~check-lbst-011
  (lambda ()
    (let* ((lbst (make-lbst))
           (lbst (lbst-set lbst (bytevector 42) (bytevector 42)))
           (lbst (lbst-set lbst (bytevector 13) (bytevector 13)))
           (lbst (lbst-set lbst (bytevector 14) (bytevector 14)))
           (lbst (lbst-set lbst (bytevector 101) (bytevector 101))))

      (let loop ((vs (list 42 13 14 101)))
        (if (null? vs)
            #t
            (begin (check (= (car vs) (bytevector-u8-ref (lbst-value (lbst-ref lbst (bytevector (car vs)))) 0)))
                   (loop (cdr vs))))))))

(define ~check-lbst-012
  (lambda ()
    (let* ((lbst (make-lbst))
           (lbst (lbst-set lbst (bytevector 42) (bytevector 42)))
           (lbst (lbst-set lbst (bytevector 13) (bytevector 13)))
           (lbst (lbst-set lbst (bytevector 14) (bytevector 14)))
           (lbst (lbst-set lbst (bytevector 101) (bytevector 101))))

      (let loop ((vs (list 41 0 7 9 255)))
        (if (null? vs)
            #t
            (begin
              (check (not (lbst-ref lbst (bytevector (car vs)))))
              (loop (cdr vs))))))))

(define ~check-lbst-013
  (lambda ()
    (let* ((lbst (make-lbst))
           (lbst (lbst-set lbst (bytevector 42) (bytevector 42)))
           (lbst (lbst-set lbst (bytevector 13) (bytevector 13)))
           (lbst (lbst-set lbst (bytevector 14) (bytevector 14)))
           (lbst (lbst-set lbst (bytevector 101) (bytevector 101))))

      (let ((new (lbst-delete lbst (bytevector 42))))
        (check (equal? (lbst->alist new)
                        `((,(bytevector 13) . ,(bytevector 13)) (,(bytevector 14) . ,(bytevector 14)) (,(bytevector 101) . ,(bytevector 101)))))))))


(define make-seed
  (lambda ()
    (let* ((now (current-time))
           (seed (* (time-second now) (time-nanosecond now))))
      (+ (modulo seed (expt 2 32)) 1))))

(define ~check-lbst-013/random
  (lambda ()
    (random-seed (make-seed))
    (for-each (lambda (n)
                (define alist (random-alist 18))
                (define expected (lbst->alist
                                  (alist->lbst
                                   (filter (lambda (x) (not (bytevector=? (car x) (caar alist))))
                                           (cdr alist)))))
                (define tmp (lbst-delete (alist->lbst alist)
                                         (caar alist)))
                (define given (lbst->alist tmp))
                (define i0 (assert (equal? expected given)))
                (define maybe-empty (fold-left (lambda (lbst x) (lbst-delete lbst x)) tmp (map car given)))
                (assert (= 0 (lbst-length maybe-empty))))
              (cddr (iota 4096)))))

(define bytevector<?
  (lambda (a b)
    (case (byter-compare a b)
      (smaller #t)
      (else #f))))

(define ~check-lbst-014/random
  (lambda ()
    (let loop ((lbst (make-lbst))
               (out '())
               (count 16))
      (if (fxzero? count)
          (begin
            (check (equal? (lbst->alist lbst) (sort (lambda (a b) (bytevector<? (car a) (car b))) out))))
          (let ((key (make-bytevector 8))
                (value (random (expt 2 64))))
            (bytevector-u64-set! key 0 value 'big)
            (loop (lbst-set lbst key key) (cons (cons key key) out) (fx- count 1)))))))

(define ~check-lbst-015/random
  (lambda ()
    (let loop ((lbst (make-lbst))
               (out '())
               (count 16))
      (if (fxzero? count)
          (begin
            (check (equal? (lbst->alist/reversed lbst) (reverse (sort (lambda (a b) (bytevector<? (car a) (car b))) out)))))
          (let ((key (make-bytevector 8))
                (value (random (expt 2 64))))
            (bytevector-u64-set! key 0 value 'big)
            (loop (lbst-set lbst key key) (cons (cons key key) out) (fx- count 1)))))))

(define random-bytevector
  (lambda (length)
    (define bytevector (make-bytevector (fx+ (random length) 1)))
    (let loop ((length (bytevector-length bytevector)))
      (unless (fxzero? length)
        (bytevector-u8-set! bytevector (fx- length 1) (random 256))
        (loop (fx- length 1))))
    bytevector))

(define ~check-lbst-016/random
  (lambda ()
    (define magic 2048)
    (let loop ((lbst (make-lbst))
               (count 2048)
               (bytes 0))
      (if (fxzero? count)
          (begin
            (check (= (lbst-length lbst) 2048))
            (check (= (lbst-bytes lbst) bytes)))
          (let ((key (random-bytevector 2048))
                (value (random-bytevector 2048)))
            (loop (lbst-set lbst key value)
                  (fx- count 1)
                  (fx+ (bytevector-length key)
                       (bytevector-length value)
                       bytes)))))))

(define ~check-lbst-017/random
  (lambda ()
    (define magic 2048)
    (define length (random magic))
    (let loop ((lbst (make-lbst))
               (count length))
      (if (fxzero? count)
          (check (= (lbst-length lbst) length))
          (let ((key (random-bytevector magic))
                (value (random-bytevector magic)))
            (loop (lbst-set lbst key value)
                  (fx- count 1)))))))

(define make-random-lbst
  (lambda (n m)
    (let loop ((lbst (make-lbst))
               (n n))
      (if (fxzero? n)
          lbst
          (let ((key (random-bytevector (fx+ 1 (random m))))
                (value (random-bytevector 1)))
            (loop (lbst-set lbst key value)
                  (fx- n 1)))))))

(define random-choice
  (lambda (xs)
    (list-ref xs (random (length xs)))))

(define make-check-lbst-bytes
  (lambda (seed n m)
    (define i (random-seed seed))
    (define x (make-random-lbst n m))
    (define a (lbst->alist x))
    (define-values (k o)
      (let loop ()
        (let ((k (car (random-choice a)))
              (o (car (random-choice a))))
          (case (byter-compare k o)
            (equal (loop))
            (smaller (values k o))
            (bigger (values o k))))))

    (define r (take-while
               (lambda (x) (case (byter-compare (car x) o)
                             (smaller #t)
                             (else #f)))
               (drop-while (lambda (x) (case (byter-compare (car x) k)
                                         (smaller #t)
                                         (else #f)))
                           a)))
    (define expected (apply + (map (lambda (x)
                                     (+ (bytevector-length (car x))
                                        (bytevector-length (cdr x))))
                                   r)))

    ;; (pk (lbst->alist x))
    ;; (lbst-debug x)

    (let ((given (lbst-bytes x
                             (lbst-ref x k)
                             (lbst-ref x o))))
      #;(pk 'check k o '=? expected given)
      (guard (ex (else (lbst-dot x k o) (raise ex)))
        (assert (= expected given))))))

(define ~check-lbst-019/random
  (lambda ()
    (for-each (lambda _
                (define n 5)
                (define m 5)
                (define s (pk 'LETLOOP_SEED=
                              (string->number
                               (or (getenv "LETLOOP_SEED")
                                   (number->string
                                    (+ (- (random (expt 2 32)) 1) 1))))))
                (make-check-lbst-bytes s n m))
              (if (getenv "LETLOOP_SEED")
                  (iota 1)
                  (iota 1000)))))
