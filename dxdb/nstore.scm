;; Copyright © 2019-2023 Amirouche BOUBEKKI <amirouche at hyper dev>
(library (letloop dxdb nstore)

  (export make-nstore nstore-add! nstore-clear!
          nstore-var nstore-var? nstore-var-name
          nstore-ref nstore-query

          ~check-nstore-000
          ~check-nstore-001
          ~check-nstore-002
          ~check-nstore-003
          ~check-nstore-004)

  (import (chezscheme)
          (letloop r999)
          (letloop okvs)
          (letloop dxdb)
          (letloop dxdb shims)
          (letloop byter))

  (begin

    (include "letloop/dxdb/nstore.body.scm")
    (include "letloop/dxdb/nstore.check.scm")))
