(define-record-type* <eavt>
  (make-eavt% triplestore now)
  eavt?
  (triplestore eavt-triplestore)
  (now eavt-now%))

(define make-eavt
  (lambda (prefix now)
    (make-eavt% (make-nstore prefix 3) now)))

(define eavt-now
  (lambda (eavt)
    ((eavt-now%))))

(define eavt-add!
  (lambda (eavt entity attribute value)
    (nstore-add! (eavt-triplestore eavt)
                 (list entity attribute value)
                 (eavt-now eavt))))

(define-record-type* <eavt-variable>
  (make-eavt-variable name)
  eavt-variable?
  (name eavt-variable-name))

(define eavt-query
  (lambda (eavt entity attribute value)
    (nstore-query (eavt-triplestore eavt)
                  (list entity attribute value))))

(define eavt-query-at
  (lambda (eavt entity attribute value at)
    (raise 'not-implemented)))
