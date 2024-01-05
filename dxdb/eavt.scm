(library (letloop dxdb eavt)
  (export make-eavt eavt-add! eavt-query eavt-query-at)
  (import (chezscheme)
          (letloop r999)
          (letloop dxdb)
          (letloop byter)
          (letloop dxdb nstore))

  (include "letloop/dxdb/eavt.body.scm"))
