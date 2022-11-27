(define-library (united base)
  (export display newline command-line
          ~check-united-base-000
          ~check-united-base-001
          )
  (import (scheme base)
          (scheme write)
          (scheme process-context))

  (include "body.scm"))
  ;; (cond-expand
  ;;  (guile
  ;;   (import (only (guile) include-from-path)))
  ;;  ((and (not guile) (not cyclone))
  ;;   (include "body.scm")))

  ;; (begin
  ;;   (cond-expand (guile
  ;;                 (include-from-path "united/body.scm"))
  ;;                (cyclone
  ;;                 (include "tests/united/body.scm")))))
