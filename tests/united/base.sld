(define-library (united base)
  (export display newline command-line
          ~check-united-base-000
          ~check-united-base-001
          ~check-united-base-002
          magic-number)
  (import (scheme base)
          (scheme write)
          (scheme process-context))

  (cond-expand
   (guile
    (import (only (guile) include-from-path)))
   (else
    (include "body.scm")))

  (begin
    (cond-expand (guile
                  (include-from-path "united/body.scm"))
                 (else #t))))
