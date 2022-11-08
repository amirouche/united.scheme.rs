(define-library (bar)
  (import (scheme base))
  (export bar)

  (begin
    (define bar
      (lambda ()
        "bar returns bar"))))
