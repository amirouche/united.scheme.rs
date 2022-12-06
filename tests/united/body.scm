(define check
  (lambda (obj)
    (if obj
        obj
        (exit 1))))

(define magic-number 4213372006)

(define ~check-united-base-000
  (lambda ()
    (display "youhou zero\n")
    #t))

(define ~check-united-base-001
  (lambda ()
    (display "youhou one\n")
    #t))

(define ~check-united-base-002
  (lambda ()
    (display "youhou two\n")
    (check #t)))
