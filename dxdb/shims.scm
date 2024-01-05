(library (letloop dxdb shims)
  (export check pk)
  (import (chezscheme))

  (define-syntax check
    (syntax-rules ()
      ((check v)
       (let ((v* v))
         (eq? v* #t)))
      ((check a b)
       (let ((a* a)
             (b* b))
         (check (equal? a* b*))))))

  (define pk
    (lambda args
      (write args)
      (newline)
      (flush-output-port)
      (car (reverse args)))))
