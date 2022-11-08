(import (scheme base))
(import (scheme write))
(import (scheme process-context))
(import (foo))
(import (bar))

(display "#;> (command-line) ;; => ") (write (command-line)) (newline)
(display "#;> (foo) ;; => ") (write (foo)) (newline)
(display "#;> (bar) ;; =>") (write (bar)) (newline)
