(define-library (foo)
  (import (scheme base))
  (export foo)

  (begin
    (define foo
      (lambda ()
        "foo returns foo"))))
