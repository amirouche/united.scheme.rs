#!chezscheme
(library (letloop dxdb notebook)
  (export make-notebook
          notebook-page-bytes
          notebook-ref
          notebook-bytes
          notebook-set!
          notebook-sync!

          ;; free page management, aka recycler;
          notebook-push!
          notebook-pop!

          ;; chapter-x will layout a bytevector over multiple notebook
          ;; page; as of yet, it is not possible to reclaim space, hence
          ;; it is only useful append only.
          ;;
          ;; TODO: implement notebook-chapter-x-free
          
          make-notebook-chapter-x
          notebook-chapter-x-ref
          notebook-chapter-x-append!
          notebook-chapter-x-query
          
          ;; ~check-notebook-000
          ;; ~check-notebook-001
          ;; ~check-notebook-002
          ;; ~check-notebook-003
          ;; ~check-notebook-004
          )
  (import (chezscheme)
          (letloop r999)
          (letloop byter)
          (letloop flow))

  ;;
  ;; XXX: DO NOT CACHE PAGES: READ, WRITE, AND SYNC ON NEED BY NEED
  ;; BASIS.  IN OTHER WORDS, DO NOT PASS AROUND BYTEVECTORS.
  ;;
  ;; WHY? Because it will be easier to discover patterns if all the
  ;; code use the same simple and slow code.
  ;;
  ;; XXX: DO NOT CACHE.
  ;;

  ;; TODO: ADD CHECKSUM.

  (define pk*
    (lambda args
      (write args)
      (newline)
      (flush-output-port)
      (car (reverse args))))

  (define-syntax pk
    (syntax-rules ()
      ((pk args ...)
       (pk* 'args ... args ...))))

  (define-record-type* <notebook>
    (make-notebook-base fd page-bytes)
    notebook?
    (fd notebook-fd)
    (page-bytes notebook-page-bytes))

  (define recycler-previous-pid
    (lambda (notebook pid)
      (bytevector-u64-ref (notebook-ref notebook pid) 0 'big)))

  (define recycler-previous-pid!
    (lambda (notebook pid previous)
      (define out (notebook-ref notebook pid))
      (bytevector-u64-set! out 0 previous 'big)
      (notebook-set! notebook pid out)
      (notebook-sync! notebook pid)))

  (define recycler-head
    (lambda (notebook pid)
      (bytevector-u64-ref (notebook-ref notebook pid) 8 'big)))

  (define recycler-head!
    (lambda (notebook pid roffset)
      (define out (notebook-ref notebook pid))
      (bytevector-u64-set! out 8 roffset 'big)
      (notebook-set! notebook pid out)
      (notebook-sync! notebook pid)))

  (define recycler-empty?
    (lambda (notebook)
      ;; the current recycler page is the zeroth itself
      (and (= (recycler-previous-pid notebook 0) 0)
           ;; the head of the zeroth page is at the end
           ;; of the page, that means it is empty.
           (= (recycler-head notebook 0) (notebook-page-bytes notebook)))))

  (define notebook-bytes
    (lambda (notebook)
      (flow-bytes (notebook-fd notebook))))

  (define make-notebook
    (lambda (filepath page-bytes)
      (define fd (flow-open filepath
                                (list 'flow-file-create
                                      'flow-file-read-write)))
      (define notebook (make-notebook-base fd page-bytes))

      (when (= 0 (notebook-bytes notebook))
        ;; new db, initialize the recycler.
        (let ((zeroth (make-bytevector (notebook-page-bytes notebook))))
          ;; spurious notebook-set! to be able to use procedures of the
          ;; recycler.
          (notebook-set! notebook 0 zeroth)
          ;; the previous page of the zeroth page is itself at the
          ;; beginning.
          (recycler-previous-pid! notebook 0 0)
          ;; At roffset 8 is stored the roffset of the head, when the page
          ;; is full it is equal to 16. Items in a recycler page start at
          ;; the end. So the page schema is:
          ;;
          ;;   { [previous page pid] [roffset of the head] ... [head] ... }
          ;;
          ;; The first uint64 is page id (pid) of the previous page,
          ;; except in the case of the zeroth page, where the first uint64
          ;; store the pid of the last page.
          (recycler-head! notebook 0 (notebook-page-bytes notebook))))
      notebook))

  (define notebook-set!
    (lambda (notebook pid bytevector)
      (flow-pwrite (notebook-fd notebook)
                       (* pid (notebook-page-bytes notebook))
                       bytevector)))

  (define notebook-ref
    (lambda (notebook pid)
      (define out (flow-pread (notebook-fd notebook)
                                  (* pid (notebook-page-bytes notebook))
                                  (notebook-page-bytes notebook)))

      (unless (= (notebook-page-bytes notebook) (bytevector-length out))
        (pk 'warning "page is smaller than notebook-page" (notebook-page-bytes notebook)))
      out))

  (define notebook-sync!
    (lambda (notebook pid)
      (flow-sync (notebook-fd notebook)
                     (* pid (notebook-page-bytes notebook))
                     (notebook-page-bytes notebook))))

  (define recycler-ref
    (lambda (notebook pid roffset)
      (bytevector-u64-ref (notebook-ref notebook pid) roffset 'big)))

  (define recycler-set!
    (lambda (notebook pid roffset v)
      (define x (notebook-ref notebook pid))
      (bytevector-u64-set! x roffset v 'big)
      (notebook-set! notebook pid x)
      (notebook-sync! notebook pid)))

  (define recycler-pop!
    (lambda (notebook)
      (let* ((pid (recycler-previous-pid notebook 0))
             (roffset (recycler-head notebook pid)))
        (if (= roffset (notebook-page-bytes notebook))
            (if (= pid 0)
                ;; the recycler is empty, return the pid of the page
                ;; at the very edge end of the file
                (let ((pid (/ (notebook-bytes notebook) (notebook-page-bytes notebook))))
                  (notebook-set! notebook pid (make-bytevector (notebook-page-bytes notebook) 0))
                  pid)
                (begin
                  ;; The current empty, and is not the zeroth page.
                  ;; Return pid as a free page, but before, fix the
                  ;; recycler: set the previous pid of the zeroth page
                  ;; to the page before pid.
                  (recycler-previous-pid! notebook 0 (recycler-previous-pid notebook pid))
                  pid))
            (let ((fid (recycler-ref notebook pid roffset)))
              (recycler-head! notebook pid (+ roffset 8))
              fid)))))

  (define recycler-push!
    (lambda (notebook pid)
      (let* ((rid (recycler-previous-pid notebook 0))
             (roffset (recycler-head notebook rid)))
        (if (not (= roffset 16))
            (let ((roffset* (- roffset 8)))
              (recycler-set! notebook rid roffset* pid)
              (recycler-head! notebook rid roffset*))
            ;; the recycler rid is full
            (let ((new (recycler-pop! notebook)))
              (recycler-previous-pid! notebook new rid)
              (recycler-previous-pid! notebook 0 new)
              (recycler-set! notebook new (- (notebook-page-bytes notebook) 8) rid)
              (recycler-head! notebook new (- (notebook-page-bytes notebook) 8)))))))

  (define notebook-pop! recycler-pop!)
  (define notebook-push! recycler-push!)

  ;; Checks

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

  (define ~check-notebook-000
    (lambda ()
      ;;
      ;; Check that the flow necessary flow interface works:
      ;;
      ;; - with-flow
      ;; - flow-open
      ;; - flow-pwrite
      ;; - flow-sync
      ;; - flow-pread
      ;;
      (define filepath #f)
      (call-with-temporary-filepath "notebook-check"
        (lambda (filepath*)
          ;;
          ;; For unknown reasons, sometime, the following check fails:
          ;;
          ;;   (assert (not (file-exists? filepath*)))
          ;;
          (set! filepath filepath*)
          (with-flow
           (let ((fd (flow-open filepath
                                    (list 'flow-file-create
                                          'flow-file-read-write))))
             (flow-pwrite fd 0 (bytevector 101 13 37))
             (flow-sync fd 0 3)
             (assert (equal? (bytevector 101 13 37) (flow-pread fd 0 3)))))))
      (not (file-exists? filepath))))

  (define ~check-notebook-001
    (lambda ()
      ;;
      ;; Check that writing a bytevector of the correct page bytes
      ;; write, then read works. The first page is written pid=0 that
      ;; will bypass, and overwrite the recycler chapter free page
      ;; management;
      ;;
      (call-with-temporary-filepath "notebook-check"
        (lambda (filepath)
          (with-flow
           (let* ((notebook (make-notebook filepath 128))
                  (expected (make-bytevector (notebook-page-bytes notebook))))
             (notebook-set! notebook 0 expected)
             (assert (equal? expected (notebook-ref notebook 0)))))))))

  (define bytevector-random
    (lambda (length)
      (u8-list->bytevector
       (map (lambda _ (random 256)) (iota length)))))
      
  (define ~check-notebook-002
    (lambda ()
      ;;
      ;; Request a free page, then write random bytes to it, and read
      ;; it back; eventually push that page, and pop it back
      ;; immediatly;
      ;;
      (call-with-temporary-filepath "notebook-check"
        (lambda (filepath)
          (with-flow
           (let* ((notebook (make-notebook filepath 1024))
                  (expected (bytevector-random (notebook-page-bytes notebook)))
                  (pid (notebook-pop! notebook)))
             (assert (= pid 1))
             (notebook-set! notebook
                       pid
                       expected)
             (assert (equal? expected
                             (notebook-ref notebook pid)))
             (notebook-push! notebook pid)
             (assert (= pid (notebook-pop! notebook)))))))))

  (define make-notebook-chapter-x
    (lambda (notebook)
     ;; Create a new chapter-x, returns the pid handle of the chapter;
      (define pid (notebook-pop! notebook))
      (define bv (notebook-ref notebook pid))
      ;; Wallou sentinel...
      (bytevector-u64-set! bv 0 1 'big)
      ;; Wallou next page...
      (bytevector-u64-set! bv (- (notebook-page-bytes notebook) 8) 0 'big)
      (notebook-set! notebook pid bv)
      (notebook-sync! notebook pid)
      pid))

  (define notebook-chapter-x-ref
    (lambda (notebook pid offset)
      (bytevector-u64-ref (notebook-chapter-x-query notebook pid offset 8)
                          0 'big)))

  (define notebook-chapter-x-append!
    (lambda (notebook xid bv)
      (define ignore (assert (= (notebook-page-bytes notebook) (bytevector-length bv))))
      (define pid (notebook-pop! notebook))
      (notebook-set! notebook pid bv)
      (notebook-sync! notebook pid)
      (notebook-chapter-x-index-set! notebook xid (notebook-chapter-x-index-head notebook xid) pid)
      (notebook-chapter-x-index-head! notebook xid (+ (notebook-chapter-x-index-head notebook xid) 1))))

  (define bytevector-concatenate
    (lambda (bvs)
      (let* ((total (apply fx+ (map bytevector-length bvs)))
             (out (make-bytevector total)))
        (let loop ((bvs bvs)
                   (index 0))
          (unless (null? bvs)
            (bytevector-copy! (car bvs) 0 out index (bytevector-length (car bvs)))
            (loop (cdr bvs) (fx+ index (bytevector-length (car bvs))))))
        out)))

  (define notebook-chapter-x-index-head
    (lambda (notebook xid)
      (notebook-chapter-x-index-ref notebook xid 0)))

  (define notebook-chapter-x-index-head!
    (lambda (notebook xid v)
      (notebook-chapter-x-index-set! notebook xid 0 v)))

  (define notebook-chapter-x-index-next
    (lambda (notebook pid)
      (bytevector-u64-ref (notebook-ref notebook pid) (- (notebook-page-bytes notebook) 8) 'big)))

  (define notebook-chapter-x-index-next!
    (lambda (notebook pid v)
      (bytevector-u64-set! (notebook-ref notebook pid) (- (notebook-page-bytes notebook) 8) v 'big)))

  (define notebook-chapter-x-index-ref
    (lambda (notebook pid i)
      (bytevector-u64-ref (notebook-ref notebook pid)
                          (* 8 i)
                          'big)))

  (define notebook-chapter-x-index-set!
    (lambda (notebook pid i v)
      (define bv (notebook-ref notebook pid))
      (bytevector-u64-set! bv (* 8 i) v 'big)
      (notebook-set! notebook pid bv)
      (notebook-sync! notebook pid)))

  (define notebook-chapter-x-query
    (lambda (notebook pid offset length)
      ;; A chapter is a virtual bytevector. offset is translated into
      ;; a pid + roffset, inside that bytevector sliced into pages of
      ;; size notebook-page-bytes. The index of the slice is
      ;; xppid0. xppid1 is the index of the slice containing the last
      ;; byte. LENGTH should be smaller than page size.

      ;; TODO: If length is large, or even bigger than the page size, what happens?

      ;; XXX: chapter-x use the same page size
      ;; aka. notebook-page-bytes.  The variables xppid host the page
      ;; index inside a chapter-x. In other words, there is an
      ;; indirection, to do the correct (notebook-ref notebook
      ;; pid). Compute the index inside chapter-x
      ;;
      ;; - start = offset
      ;; - end = offset + length
      ;;
      (define-values (xppid0 roffset0) (div-and-mod offset (notebook-page-bytes notebook)))
      (define-values (xppid1 roffset1) (div-and-mod (+ offset length) (notebook-page-bytes notebook)))

      ;; compute the rpid, and the offset inside that rpid of where is
      ;; stored the notebook index of the virtual pages xppid0, and
      ;; xppid1
      (define-values (i0 i0-offset) (div-and-mod xppid0 (/ (notebook-page-bytes notebook) 8)))
      (define-values (i1 i1-offset) (div-and-mod xppid1 (/ (notebook-page-bytes notebook) 8)))

      (let loop0 ((i i0)
                  (pid pid))
        (if (not (zero? i))
            (loop0 (fx- i 1)
                   (notebook-chapter-x-index-next notebook pid))
            (let loop1 ((k (+ i0 1))
                        (i i0-offset)
                        (pid pid)
                        (out '()))
              (cond
               ((and (= 1 (+ i1 1)) (= i i1-offset)) (bytevector-concatenate (reverse out)))
               ((= i (- (notebook-page-bytes notebook) 8))
                (loop1 (+ k 1)
                       0
                       (notebook-chapter-x-index-next notebook k)
                       out))
               (else
                (loop1 k
                       (+ i 1)
                       pid
                       (cons (notebook-ref notebook (notebook-chapter-x-index-ref notebook pid (+ i 1)))
                             out)))))))))

  (define ~check-notebook-003
    (lambda ()
      (call-with-temporary-filepath "notebook-check"
        (lambda (filepath)
          (with-flow
           (let* ((notebook (make-notebook filepath 512))
                  (x (make-notebook-chapter-x notebook))
                  (bv0 (make-bytevector (notebook-page-bytes notebook) 1))
                  (bv1 (make-bytevector (notebook-page-bytes notebook) 5))
                  (bv2 (make-bytevector (notebook-page-bytes notebook) 9)))
             (notebook-chapter-x-append! notebook x bv0)
             (notebook-chapter-x-append! notebook x bv1)
             (notebook-chapter-x-append! notebook x bv2)
             (assert
              (equal? (notebook-chapter-x-query notebook x 0 (* 3 (notebook-page-bytes notebook)))
                      (bytevector-concatenate (list bv0 bv1 bv2))))))))))

  (define ~check-notebook-004
    (lambda ()
      (call-with-temporary-filepath "notebook-check"
        (lambda (filepath)
          (with-flow
           (let* ((notebook (make-notebook filepath 128))
                  (chapter-x (make-notebook-chapter-x notebook))
                  (bv0 (make-bytevector (notebook-page-bytes notebook) 0))
                  (bv1 (make-bytevector (notebook-page-bytes notebook) 0))
                  (bv2 (make-bytevector (notebook-page-bytes notebook) 0))
                  (i0 (- (notebook-page-bytes notebook) 1))
                  (e0 42)
                  (i1 0)
                  (e1 (bytevector 33 01))
                  (i2 (/ (notebook-page-bytes notebook) 2))
                  (e2 93))
             ;; when
             (bytevector-u8-set! bv0 i0 e0)
             (bytevector-u8-set! bv1 i1 (bytevector-u8-ref e1 0))
             (bytevector-u8-set! bv1 (+ i1 1)  (bytevector-u8-ref e1 1))
             (bytevector-u8-set! bv1 i2 e2)
             (notebook-chapter-x-append! notebook chapter-x bv0)
             (notebook-chapter-x-append! notebook chapter-x bv1)
             (notebook-chapter-x-append! notebook chapter-x bv2)
             ;; then
             (assert
              (equal?
               (bytevector e0)
               (pk (notebook-chapter-x-query notebook
                                             chapter-x
                                             i0
                                             1))))
             (pk 2)
             (assert
              (equal?
               (byter-append (bytevector e0) e1)
               (notebook-chapter-x-query notebook
                                         chapter-x
                                         i0
                                         3)))
             (pk 3)
             (assert
              (equal?
               (bytevector e2)
               (notebook-chapter-x-query notebook
                                         chapter-x
                                         (+ (* (notebook-page-bytes notebook) 2)
                                            i2)
                                         1)))))))))

  )
