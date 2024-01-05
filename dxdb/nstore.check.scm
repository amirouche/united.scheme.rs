;; Copyright © 2019-2023 Amirouche BOUBEKKI <amirouche at hyper dev>

(define triplestore (make-nstore (bytevector 101) 3))

(define ~check-nstore-000
  (lambda ()
    (check (not
            ;; ask an empty database
            (let* ((okvs (make-dxdb)))
              (call-with-okvs-transaction okvs
                (lambda (tx)
                  (nstore-ref tx triplestore '("P4X432" blog/title "hyper.dev")))))))))

(define ~check-nstore-001
  (lambda ()
    (check (bytevector 42)
           (let ((okvs (make-dxdb)))
             ;; add
             (call-with-okvs-transaction okvs
               (lambda (tx)
                 (nstore-add! tx triplestore '("P4X432" blog/title "hyper.dev") (bytevector 42))))
             (call-with-okvs-transaction okvs
               (lambda (tx)
                 (nstore-ref tx triplestore '("P4X432" blog/title "hyper.dev"))))))))

(define ~check-nstore-002
  (lambda ()
    (check
     (not
      (let ((okvs (make-dxdb)))
        (call-with-okvs-transaction okvs
          (lambda (tx)
            ;; add!
            (nstore-add! tx triplestore '("P4X432" blog/title "hyper.dev") (bytevector 42))
            ;; clear!
            (nstore-clear! tx triplestore '("P4X432" blog/title "hyper.dev"))
            ;; ref
            (nstore-ref tx triplestore '("P4X432" blog/title "hyper.dev")))))))))

(define generator->list
  (lambda (g)
    (let fx ()
      (let ((object (g)))
        (if (eof-object? object)
            '()
            (cons object (fx)))))))

(define ~check-nstore-003
  (lambda ()
    (check '("DIY a database" "DIY a full-text search engine")
      (let ((okvs (make-dxdb)))
        (call-with-okvs-transaction okvs
          (lambda (tx)
            ;; add hyper.dev blog posts
            (nstore-add! tx triplestore '("P4X432" blog/title "hyper.dev") (bytevector))
            (nstore-add! tx triplestore '("123456" post/title "DIY a database") (bytevector))
            (nstore-add! tx triplestore '("123456" post/blog "P4X432") (bytevector))
            (nstore-add! tx triplestore '("654321" post/title "DIY a full-text search engine") (bytevector))
            (nstore-add! tx triplestore '("654321" post/blog "P4X432") (bytevector))
            ;; add dthompson.us blog posts
            (nstore-add! tx triplestore '("1" blog/title "dthompson.us") (bytevector))
            (nstore-add! tx triplestore '("2" post/title "Haunt 0.2.4 released") (bytevector))
            (nstore-add! tx triplestore '("2" post/blog "1") (bytevector))
            (nstore-add! tx triplestore '("3" post/title "Haunt 0.2.3 released") (bytevector))
            (nstore-add! tx triplestore '("3" post/blog "1") (bytevector))))
        ;; query
        (call-with-okvs-transaction okvs
          (lambda (tx)
            (generator->list
             (gmap (lambda (x) (cdr (assq 'post/title x)))
                   (nstore-query tx triplestore
                                 (list (list (nstore-var 'blog/uid)
                                             'blog/title
                                             "hyper.dev")
                                       (list (nstore-var 'post/uid)
                                             'post/blog
                                             (nstore-var 'blog/uid))
                                      (list (nstore-var 'post/uid)
                                            'post/title
                                            (nstore-var 'post/title))))))))))))

(define ~check-nstore-004
  (lambda ()
    (check '("hyper.dev" "hyperdev.fr" "hypermove.net")
           (let ((okvs (make-dxdb)))
             (call-with-okvs-transaction okvs
               (lambda (tx)
                 ;; add!
                 (nstore-add! tx triplestore '("P4X432" blog/title "hyper.dev") (bytevector))
                 (nstore-add! tx triplestore '("P4X433" blog/title "hyperdev.fr") (bytevector))
                 (nstore-add! tx triplestore '("P4X434" blog/title "hypermove.net") (bytevector))))
             (call-with-okvs-transaction okvs
               (lambda (tx)
                 (generator->list
                  (gmap
                   (lambda (item) (cdr (assq 'title item)))
                   (nstore-query tx triplestore (list (list (nstore-var 'uid)
                                                            'blog/title
                                                            (nstore-var 'title))))))))))))
