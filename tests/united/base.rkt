#!r7rs
(define-library (united base)
  (export display newline command-line
          ~check-united-base-000
          ~check-united-base-001
          ~check-united-base-002
          magic-number)
  (import (scheme base)
          (scheme write)
          (scheme process-context))

  (include "body.scm"))
