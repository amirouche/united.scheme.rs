(define-syntax define*
  (lambda (stx)
    (syntax-case stx ()
      ((define* name procedure)
       (with-syntax ((dname
                     (datum->syntax #'define*
                                    (string->symbol
                                     (string-append "dxdb-"
                                                    (symbol->string
                                                     (syntax->datum #'name)))))))
         #'(define dname (gamma name (lambda (args)
                                       (and (pair? args)
                                            (or (dxdb-handle? (car args))
                                                (dxdb-error? (car args)))))
                                procedure)))))))

(define-record-type* <dxdb>
  (make-dxdb-base lbst key-max-length value-max-length transaction-timeout transaction-max-bytes begin-hook pre-commit-hook post-commit-hook rollback-hook)
  dxdb?
  (lbst dxdb-lbst dxdb-lbst!)
  (key-max-length dxdb-key-max-length)
  (value-max-length dxdb-value-max-length)
  (transaction-timeout dxdb-transaction-timeout)
  (transaction-max-bytes dxdb-transaction-max-bytes)
  (begin-hook dxdb-begin-hook)
  (pre-commit-hook dxdb-pre-commit-hook)
  (post-commit-hook dxdb-post-commit-hook)
  (rollback-hook dxdb-rollback-hook))

(define-record-type* <dxdb-transaction>
  (make-transaction dxdb lbst read-only? context)
  dxdb-transaction?
  (dxdb dxdb-transaction-dxdb)
  (lbst dxdb-transaction-lbst dxdb-transaction-lbst!)
  (read-only? dxdb-transaction-read-only?)
  (context dxdb-transaction-context))

(define-record-type* <dxdb-error>
  (make-dxdb-error key)
  dxdb-error?
  (key dxdb-error-key))

(define-record-type* <dxdb-cursor>
  (make-cursor transaction key value)
  dxdb-cursor?
  (transaction dxdb-cursor-transaction)
  (key dxdb-key dxdb-key!)
  (value dxdb-value dxdb-value!))

(define* okvs-key dxdb-key)

(define* okvs-value dxdb-value)

(define* okvs-transaction? dxdb-transaction?)

(define* okvs-transaction-timeout dxdb-transaction-timeout)

(define* okvs-transaction-max-bytes dxdb-transaction-max-bytes)

(define* okvs-transaction-read-only? dxdb-transaction-read-only?)

(define* okvs-cursor? dxdb-cursor?)

(define* okvs-error? dxdb-error?)

(define* okvs? dxdb?)

(define* okvs-begin-hook dxdb-begin-hook)
(define* okvs-pre-commit-hook dxdb-pre-commit-hook)
(define* okvs-post-commit-hook dxdb-post-commit-hook)
(define* okvs-rollback-hook dxdb-rollback-hook)

(define* okvs-root
  (lambda (handle)
    (cond
     ((dxdb? handle) handle)
     ((dxdb-transaction? handle) (dxdb-transaction-dxdb handle))
     ((dxdb-cursor? handle) (okvs-root (dxdb-cursor-transaction handle))))))

(define dxdb-key-max-length-default
  (lambda ()
    ;; taken from foundationdb
    (expt 10 3)))

(define dxdb-value-max-length-default
  (lambda ()
    ;; taken from foundationdb
    (expt 10 5)))

(define* okvs-key-max-length dxdb-key-max-length)
(define* okvs-value-max-length dxdb-value-max-length)

(define make-dxdb
  (lambda ()
    (make-dxdb-base (make-lbst)
                    (dxdb-key-max-length-default)
                    (dxdb-value-max-length-default)
                    5 ;; timeout
                    (expt 10 9) ;; 1G
                    (make-hook 1)
                    (make-hook 1)
                    (make-hook 1)
                    (make-hook 1))))

(define call-with-dxdb-transaction-base
  (case-lambda
   ((tx proc)
    (call-with-dxdb-transaction-base tx proc raise values))
   ((tx proc failure)
    (call-with-dxdb-transaction-base tx proc failure values))
   ((tx proc failure success)
    (hook-run (dxdb-begin-hook (dxdb-transaction-dxdb tx)) tx)
    (guard (ex (else (hook-run (dxdb-rollback-hook
                                (dxdb-transaction-dxdb tx)) tx)
                     (failure ex)))
      (call-with-values (lambda () (proc tx))
        (lambda args
          (hook-run (dxdb-pre-commit-hook (dxdb-transaction-dxdb tx)) tx)
          (unless (dxdb-transaction-read-only? tx)
            (dxdb-lbst! (dxdb-transaction-dxdb tx)
                        (dxdb-transaction-lbst tx)))
          (hook-run (dxdb-post-commit-hook (dxdb-transaction-dxdb tx)) tx)
          (apply success args)))))))

(define call-with-dxdb-transaction
  (lambda (dxdb . args)
    (apply call-with-dxdb-transaction-base
           (make-transaction dxdb (dxdb-lbst dxdb) #f (make-eq-hashtable))
           args)))

(define* call-with-okvs-transaction call-with-dxdb-transaction)

(define* call-with-okvs-transaction-read-only
  (lambda (dxdb . args)
    (apply call-with-dxdb-transaction-base
           (make-transaction dxdb (dxdb-lbst dxdb) #t (make-eq-hashtable))
           args)))

(define* okvs-transaction-context
  (lambda (tx key default)
    (hashtable-ref (dxdb-transaction-context tx) key default)))

(define* okvs-transaction-context!
  (lambda (tx key value)
    (hashtable-set! (dxdb-transaction-context tx) key value)))

;; (define* alist->okvs
;;   (lambda (alist)
;;     (define dxdb (make-dxdb))
;;     (define lbst (let loop ((alist alist)
;;                             (lbst (make-lbst)))
;;                    (if (null? alist)
;;                        lbst
;;                        (loop (cdr alist)
;;                              (lbst-set lbst (caar alist) (cdar alist))))))
;;     (dxdb-lbst! dxdb lbst)
;;     dxdb))

(define* okvs->alist
  (lambda (dxdb)
    (lbst->alist (dxdb-lbst dxdb))))

(define handle-dxdb
  (lambda (handle)
    (cond
     ((dxdb? handle) handle)
     ((dxdb-transaction? handle) (dxdb-transaction-dxdb handle))
     ((dxdb-cursor? handle) (dxdb-transaction-dxdb
                             (dxdb-cursor-transaction handle))))))

(define* okvs-approximate-byte-count
  (lambda (handle)
    (define dxdb (handle-dxdb handle))
    (lbst-bytes (dxdb-lbst dxdb))))

(define* okvs-approximate-key-count
  (lambda (handle)
    (lbst-length (dxdb-lbst (handle-dxdb handle)))))

(define bytevector-compare
  (lambda (bytevector other)
    ;; Returns the symbol 'smaller if BYTEVECTOR is before OTHER,
    ;; returns the bytevector 'equal if they are equal and otherwise
    ;; returns 'bigger
    (let ((end (fxmin (bytevector-length bytevector)
                      (bytevector-length other))))
      (let loop ((index 0))
        (if (fx=? end index)
            ;; BYTEVECTOR and OTHER are equal until index; BYTEVECTOR
            ;; is smaller lexicographically, if it is smaller in
            ;; length.
            (if (fx=? (bytevector-length bytevector)
                      (bytevector-length other))
                'equal
                (if (fx<? (bytevector-length bytevector)
                          (bytevector-length other))
                    'smaller
                    'bigger))
            (let ((delta (fx- (bytevector-u8-ref bytevector index)
                              (bytevector-u8-ref other index))))
              (if (fxzero? delta)
                  (loop (fx+ 1 index))
                  (if (fxnegative? delta)
                      'smaller
                      'bigger))))))))

(define generator-foreach
  (lambda (proc g)
    (let loop ()
      (let ((object (g)))
        (unless (eof-object? object)
          (proc object)
          (loop))))))

(define dxdb-clear!
  (case-lambda
   ((handle key)
    (cond
     ((dxdb? handle)
      (call-with-dxdb-transaction handle (lambda (tx) (dxdb-clear! tx key))))
     ((dxdb-transaction? handle)
      (dxdb-transaction-lbst! handle
                              (lbst-delete
                               (dxdb-transaction-lbst handle) key)))
     ((dxdb-cursor? handle) (dxdb-clear! (dxdb-cursor-transaction handle) key))))
   ((handle key other)
    ;; this works because there is a single writer
    (cond
     ((dxdb? handle)
      (call-with-dxdb-transaction handle (lambda (tx) (dxdb-clear! tx key other))))
     ((dxdb-transaction? handle)
      (generator-foreach (lambda (x) (dxdb-clear! handle (car x)))
                         (dxdb-query handle key other)))))))

(define* okvs-clear! dxdb-clear!)

(define* okvs-close
  (lambda (dxdb)
    #t))

(define dxdb-cursor-lbst
  (lambda (cursor)
    (dxdb-transaction-lbst (dxdb-cursor-transaction cursor))))

(define* okvs-empty?
  (lambda (handle)
    (= (lbst-length (dxdb-lbst (handle-dxdb handle))) 0)))

(define dxdb-handle?
  (lambda (object)
    (or (dxdb? object)
        (dxdb-transaction? object)
        (dxdb-cursor? object))))

(define* okvs-valid?
  (lambda (cursor)
    (not (not (dxdb-key cursor)))))

(define dxdb-next
  (lambda (cursor)
    (define root (dxdb-transaction-lbst (dxdb-cursor-transaction cursor)))

    (call-with-lbst root (dxdb-key cursor)
      (lambda (lbst position)
        (if (not position)
            (begin
              (dxdb-key! cursor #f)
              (dxdb-value! cursor #f)
              #f)
            (if (eq? position 'bigger)
                (begin
                  (dxdb-key! cursor (lbst-key lbst))
                  (dxdb-value! cursor (lbst-value lbst))
                  #t)
                (let loop ((lbst lbst))
                  (if (not lbst)
                      (begin
                        (dxdb-key! cursor #f)
                        (dxdb-value! cursor #f)
                        #f)
                      (case (bytevector-compare (lbst-key lbst) (dxdb-key cursor))
                        ((smaller equal) (loop (lbst-next lbst)))
                        (else
                         (if (not lbst)
                             (begin
                               (dxdb-key! cursor #f)
                               (dxdb-value! cursor #f)
                               #f)
                             (begin
                               (dxdb-key! cursor (lbst-key lbst))
                               (dxdb-value! cursor (lbst-value lbst))
                               #t))))))))))))

(define dxdb-previous
  (lambda (cursor)
    (define root (dxdb-transaction-lbst (dxdb-cursor-transaction cursor)))

    (call-with-lbst root (dxdb-key cursor)
      (lambda (lbst position)
        (if (not position)
            (begin
              (dxdb-key! cursor #f)
              (dxdb-value! cursor #f)
              #f)
            (if (eq? position 'smaller)
                (begin
                  (dxdb-key! cursor (lbst-key lbst))
                  (dxdb-value! cursor (lbst-value lbst))
                  #t)
                (let loop ((lbst lbst))
                  (if (not lbst)
                      (begin
                        (dxdb-key! cursor #f)
                        (dxdb-value! cursor #f)
                        #f)
                      (case (bytevector-compare (lbst-key lbst) (dxdb-key cursor))
                        ((bigger equal) (loop (lbst-previous lbst)))
                        (else
                         (if (not lbst)
                             (begin
                               (dxdb-key! cursor #f)
                               (dxdb-value! cursor #f)
                               #f)
                             (begin
                               (dxdb-key! cursor (lbst-key lbst))
                               (dxdb-value! cursor (lbst-value lbst))
                               #t))))))))))))

(define* okvs-next dxdb-next)
(define* okvs-previous dxdb-previous)

(define dxdb-ref
  (lambda (tx key)
    (call-with-dxdb-cursor tx key
      (lambda (cursor position)
        (case position
          ((key-exact) (dxdb-value cursor))
          (else #f))))))

(define (make-coroutine-generator proc)
  ;; TODO: avoid the need for this procedure
  (define return #f)
  (define resume #f)
  (define yield (lambda (v)
                  ;; TODO: chez replace with call/1cc
                  (call/cc (lambda (r) (set! resume r) (return v)))))
  ;; TODO: chez replace with call/1cc
  (lambda () (call/cc
              (lambda (cc) (set! return cc)
                      (if resume
                          (resume (if #f #f))  ; void? or yield again?
                          (begin (proc yield)
                                 (set! resume (lambda (v) (return (eof-object))))
                                 (return (eof-object))))))))

(define dxdb-query-base-generator
  (lambda (tx key other offset limit iterate symbol1 symbol2)
    ;; TODO (low): remove make-coroutine-generator
    (make-coroutine-generator
     (lambda (yield)
       (call-with-dxdb-cursor
        tx key
        (lambda (cursor position)
          (when cursor

            (when (if (eq? position symbol1)
                      (iterate cursor)
                      #t)
              (let loop ((offset offset)
                         (limit limit))
                (when (or (< 0 limit) (>= -1 limit))
                  (when (dxdb-key cursor)
                    (when (eq? symbol2 (bytevector-compare (dxdb-key cursor) other))
                      (if (< 0 offset)
                          (begin
                            (if (iterate cursor)
                                (loop (- offset 1) limit)))
                          (begin
                            (yield (cons (dxdb-key cursor)
                                         (dxdb-value cursor)))
                            (if (iterate cursor)
                                (loop offset (- limit 1)))))))))))))))))

(define dxdb-query-base
  (case-lambda
   ((tx key) (dxdb-ref tx key))
   ((tx key other) (dxdb-query-base tx key other 0 -1))
   ((tx key other offset) (dxdb-query-base tx key other offset -1))
   ((tx key other offset limit)
    (case (bytevector-compare key other)
      ((smaller) (dxdb-query-base-generator tx key other offset limit
                                            dxdb-next 'key-before 'smaller))
      ((bigger) (dxdb-query-base-generator tx key other offset limit
                                           dxdb-previous 'key-after 'bigger))
      ((equal) (error 'dxdb "Invald dxdb-query arguments" key other))))))

(define generator->list
  (lambda (g)
    (let f ()
      (let ((o (g)))
        (if (eof-object? o)
            '()
            (cons o (f)))))))

(define dxdb-query
  (lambda (handle . args)
    (cond
     ((dxdb? handle)
      (call-with-dxdb-transaction handle
        (lambda (tx)
          (define out (apply dxdb-query-base tx args))
          (if (null? (cdr args))
              ;; it is (dxdb-query x key) aka. dxdb-ref
              out
              ;; Otherwise `out` is a generator, convert to a list.
              (generator->list out)))))
     ((dxdb-transaction? handle)
      (apply dxdb-query-base handle args))
     ((dxdb-cursor? handle)
      (apply dxdb-query-base (dxdb-cursor-transaction handle) args)))))

(define* okvs-query dxdb-query)

(define call-with-dxdb-cursor
  (lambda (handle key proc)

    (define search
      (lambda (tx key proc)
        (define root (dxdb-transaction-lbst tx))

        (call-with-lbst root key
          (lambda (lbst position)
            (if (not position)
                (proc #f #f)
                (proc (make-cursor tx
                                   (lbst-key lbst)
                                   (lbst-value lbst))
                      (case position
                        ((exact) 'key-exact)
                        ((before) 'key-before)
                        ((after) 'key-after))))))))

    (cond
     ((dxdb? handle) (call-with-dxdb-transaction handle
                       (lambda (tx)
                         (search tx key proc))))
     ((dxdb-transaction? handle)
      (search handle key proc))
     ((dxdb-cursor? handle)
      ;; TODO: re-use the cursor HANDLE?
      (search (dxdb-cursor-transaction handle) key proc)))))

(define* call-with-okvs-cursor call-with-dxdb-cursor)

(define dxdb-set!
  (lambda (handle key value)
    (cond
     ((dxdb? handle) (call-with-dxdb-transaction handle
                       (lambda (tx)
                         (dxdb-set! tx key value))))
     ((dxdb-transaction? handle)
      (when (dxdb-transaction-read-only? handle)
        (raise (make-dxdb-error 'readonly)))
      (dxdb-transaction-lbst! handle
                              (lbst-set
                               (dxdb-transaction-lbst handle) key value)))
     ((dxdb-cursor? handle)
      (dxdb-set! (dxdb-cursor-transaction handle) key value)))))

(define* okvs-set! dxdb-set!)

(define* okvs-read
  (lambda (filepath)
    (define dxdb (make-dxdb))
    (define port (open-file-input-port filepath))
    (define lbst (fasl-read port))
    (close-port port)
    (dxdb-lbst! dxdb lbst)
    dxdb))

(define* okvs-write
  (lambda (dxdb filepath)
    (define port (open-file-output-port filepath))
    (fasl-write (dxdb-lbst dxdb) port)
    (close-port port)))
