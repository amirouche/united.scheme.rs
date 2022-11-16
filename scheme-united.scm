#!/usr/bin/env chibi-scheme
(import (scheme small))
(import (scheme list))
(import (scheme process-context))
(import (chibi process))
(import (chibi match))
(import (chibi filesystem))
(import (only (chibi ast) setenv unsetenv))


(define pk
  (lambda args
    (write args)
    (newline)
    (car (reverse args))))

(define call-with-env
  (lambda (env thunk)
    ;; backup variables before overriding
    (define variables (get-environment-variables))
    ;; override!
    (let loop ((env env))
      (unless (null? env)
	(setenv (symbol->string (caar env)) (cdar env))
	(loop (cdr env))))
    ;; call thunk
    (call-with-values thunk
      (lambda args
	(let loop ((env env))
	  (if (null? env)
              ;; bring back original variables
              (let loop ((variables variables))
                (unless (null? variables)
                  (setenv (caar variables) (cdar variables))
                  (loop (cdr variables))))
	      (begin
                ;; unset
                (unsetenv (symbol->string (caar env)))
	        (loop (cdr env)))))
	(apply values args)))))

(define (command-line-parse* arguments)

  ;; Given the following ARGUMENTS:
  ;;
  ;;   '("--foo=bar" "--qux" "-vvv" "name" "another" "--" "olive" "extra")
  ;;
  ;; command-line-parse* returns the following values:
  ;;
  ;;   (values '((--foo . "bar") (--qux . #t) (-vvv . #t))
  ;;           '("name" "other")
  ;;           '("olive" "extra"))
  ;;
  ;; Standalone arguments e.g. "name" and "other" and extra arguments
  ;; e.g. "olive" and "extra" are returned in the same order as
  ;; found in ARGUMENTS.

  (define keyword/value
    (lambda (string)
      (define index (list-index (lambda (x) (char=? x #\=)) (string->list string)))

      (if (not index)
          (values (string->symbol string) #t)
          (values (string->symbol (substring string 0 index)) (substring string (+ index 1) (string-length string))))))

  (let loop ((arguments arguments)
             (keywords '())
             (standalone '()))
    (if (null? arguments)
        (values keywords (reverse standalone) '())
        (let ((head (car arguments)))
          (cond
           ((string=? head "--")
            (values keywords (reverse standalone) (cdr arguments)))
           ((char=? (string-ref head 0) #\-)
            (call-with-values (lambda () (keyword/value head))
              (lambda (key value)
                (loop (cdr arguments) (cons (cons key value) keywords) standalone))))
           (else (loop (cdr arguments) keywords (cons head standalone))))))))

(define united-usage
  (lambda ()
    (display "
  scheme-united available [SCHEME]
  scheme-united install [--latest|--stable|--version=VERSION] SCHEME ...
  scheme-united prefix [DIRECTORY]
  scheme-united SCHEME repl [DIRECTORY ...]
  scheme-united SCHEME exec [DIRECTORY ...] PROGRAM [-- EXTRA ...]
  scheme-united SCHEME version

")))

(define command-line-parse-standalone
  (lambda (arguments)

    (define guess
      (lambda (string)
        (cond
         ((file-directory? string) (values 'directory (make-filepath string)))
         ((file-exists? string)
          (values 'file (make-filepath string)))
         ;; the first char is a dot, the associated path is neither a
         ;; file or a directory, hence it is prolly an
         ;; extension... breaks when the user made a typo in a file or
         ;; directory name.
         ((char=? (string-ref string 0) #\.)
          (values 'extension string))
         (else (values 'unknown string)))))

    (let loop ((arguments arguments)
               (directories '())
               (files '())
               (extensions '())
               (unknowns '()))
      (if (null? arguments)
          (values directories files extensions unknowns)
          (call-with-values (lambda () (guess (car arguments)))
            (lambda (type value)
              (case type
                ((directory) (loop (cdr arguments)
                                   (cons value directories)
                                   files
                                   extensions
                                   unknowns))
                ((file) (loop (cdr arguments)
                              directories
                              (cons value files)
                              extensions
                              unknowns))
                ((extension) (loop (cdr arguments)
                                   directories
                                   files
                                   (cons value extensions)
                                   unknowns))
                ((unknown) (loop (cdr arguments)
                                 directories
                                 files
                                 extensions
                                 (cons value unknowns))))))))))

(define command-line-parse
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse* arguments))
      (lambda (keywords standalone extra)
        (call-with-values (lambda () (command-line-parse-standalone standalone))
          (lambda (directories files extensions unknowns)
            (values keywords directories files extensions unknowns extra)))))))

(define run-singleton-failure '(run singleton failure))

(define run-failure?
  (lambda (object)
    (eq? object run-singleton-failure)))

(define run
  (lambda (directory env . command)
    ;; (pk command)
    (unless (call-with-env env (lambda ()
			         (if directory
				     (with-directory directory (lambda () (apply system? command)))
				     (apply system? command))))
      (raise run-singleton-failure))))

(define union '())

(define unionize
  (lambda (scheme version methods)
    (set! union (cons (list scheme version methods) union))))

(define united-available
  (lambda (scheme)
    (if scheme
	(for-each (lambda (x) (display (cadr x)) (newline))
		  (filter (lambda (x) (string=? scheme (symbol->string (car x))))
			  union))
	(for-each (lambda (x) (display x) (newline))
		  (delete-duplicates (map car union))))))

(define worker-count
  (lambda ()
    (let ((count (get-environment-variable "UNITED_WORKER_COUNT")))
      (if (and count (string->number count))
          (string->number count)
          1))))

(define united-prefix-ref
  (lambda ()
    (or (get-environment-variable "UNITED_PREFIX")
        "/opt/united/")))

(define united-prefix-display
  (lambda ()
    (display (united-prefix-ref))
    (newline)))

(define chibi-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/chibi/"))

    (display (string-append "* Installing chibi @ " work "\n"))

    (guard (ex (else #f))
	   (delete-file-hierarchy work))
    (create-directory* work)

    ;; TODO: support cloning with full history
    (run work '() "git" "clone" "--depth=1" "https://github.com/ashinn/chibi-scheme/" "src")
    (run (string-append work "/src/") '() "git" "checkout" version)
    (run (string-append work "/src/")
	 '()
	 "make"
	 (string-append "-j" (number->string (worker-count)))
	 (string-append "PREFIX=" work)
	 "install")))

(unionize 'chibi 'v0.10
	  `((install . ,(lambda () (chibi-install "0.10")))))
(unionize 'chibi 'stable
	  `((install . ,(lambda () (chibi-install "stable")))))
(unionize 'chibi 'latest
	  `((install . ,(lambda () (chibi-install "HEAD")))))

(define chez-cisco-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/chez-cisco/"))

    (display (string-append "* Installing chez-cisco @ " work "\n"))

    (guard (ex (else #f))
	   (delete-file-hierarchy work))
    (create-directory* work)

    ;; TODO: support cloning with full history
    (run work '() "git" "clone" "--depth=1" "https://github.com/cisco/ChezScheme/" "src")
    (run (string-append work "/src/") '() "git" "checkout" version)
    (run (string-append work "/src/") '()
         "sh" "configure" "--disable-curses" "--disable-x11" "--threads"
         (string-append "--installprefix=" work))
    (run (string-append work "/src/")
	 '()
	 "make"
	 (string-append "-j" (number->string (worker-count))))
    (run (string-append work "/src/")
	 '()
	 "make"
         "install")))

(unionize 'chez-cisco 'v9.5.8
	  `((install . ,(lambda () (chez-cisco-install "9.5.8")))))
(unionize 'chez-cisco 'stable
	  `((install . ,(lambda () (chez-cisco-install "9.5.8")))))
(unionize 'chez-cisco 'latest
	  `((install . ,(lambda () (chez-cisco-install "HEAD")))))

(define guile-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/guile/"))

    (display (string-append "* Installing guile @ " work "\n"))

    (guard (ex (else #f))
	   (delete-file-hierarchy work))
    (create-directory* work)

    ;; TODO: support cloning with full history
    (run work '() "git" "clone" "--depth=1" "https://git.sv.gnu.org/git/guile.git" "src")
    (run (string-append work "/src/") '() "git" "checkout" version)
    (run (string-append work "/src/") '()
         "sh" "autogen.sh")
    (run (string-append work "/src/") '()
         "sh" "configure" (string-append "--prefix=" work))
    (run (string-append work "/src/")
	 '()
	 "make"
	 (string-append "-j" (number->string (worker-count))))
    (run (string-append work "/src/")
	 '()
	 "make"
         "install")))

(unionize 'guile 'latest
	  `((install . ,(lambda () (guile-install "HEAD")))))

(define gambit-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/gambit/"))

    (display (string-append "* Installing gambit @ " work "\n"))

    (guard (ex (else #f))
	   (delete-file-hierarchy work))
    (create-directory* work)

    ;; TODO: support cloning with full history
    ;; (run work '() "git" "clone" "--depth=1" "https://github.com/gambit/gambit/" "src")
    (run work '() "git" "clone" "--depth=1" "/home/amirouche/src/scheme/gambit/" "src")
    (run (string-append work "/src/") '() "git" "checkout" version)
    (run (string-append work "/src/") '()
         "sh" "configure" (string-append "--prefix=" work))
    (run (string-append work "/src/")
	 '()
	 "make"
	 (string-append "-j" (number->string (worker-count))))
    (run (string-append work "/src/")
	 '()
	 "make"
         "install")))

(unionize 'gambit 'latest
	  `((install . ,(lambda () (gambit-install "HEAD")))))

(define gauche-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/gauche/"))

    (display (string-append "* Installing gauche @ " work "\n"))

    (guard (ex (else #f))
	   (delete-file-hierarchy work))
    (create-directory* work)

    (run work '() "wget" "https://raw.githubusercontent.com/shirok/get-gauche/master/get-gauche.sh")
    ;; TODO: support cloning with full history
    (run work '() "bash" "get-gauche.sh" (string-append "--prefix=" work) "--force" "--skip-tests" "--auto")
    (run work '() "git" "clone" "--depth=1" "https://github.com/shirok/Gauche" "src")
    (run (string-append work "/src/") '() "git" "checkout" version)
    (run (string-append work "/src/") '()
         "sh" "DIST" "gen")
    (run (string-append work "/src/") '()
         "sh" "configure" (string-append "--prefix=" work))
    (let ((PATH (string-append (get-environment-variable "PATH") ":" work "/bin")))
      (run (string-append work "/src/")
	   `((PATH . ,PATH))
	   "make"
	   (string-append "-j" (number->string (worker-count)))))
    (run (string-append work "/src/")
	 '()
	 "make"
         "install")))

(unionize 'gauche 'latest
	  `((install . ,(lambda () (gauche-install "HEAD")))))

(define chez-racket-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/chez-racket/"))

    (display (string-append "* Installing chez-racket @ " work "\n"))

    (guard (ex (else #f))
	   (delete-file-hierarchy work))
    (create-directory* work)

    ;; TODO: support cloning with full history
    (run work '() "git" "clone" "--depth=1" "https://github.com/racket/ChezScheme/" "src")
    (run (string-append work "/src/") '() "git" "checkout" version)
    (run (string-append work "/src/") '()
         "sh" "configure" "--disable-curses" "--disable-x11" "--threads" "--kernelobj"
         (string-append "--installprefix=" work))
    (run (string-append work "/src/")
	 '()
	 "make"
	 (string-append "-j" (number->string (worker-count))))
    (run (string-append work "/src/")
	 '()
	 "make"
         "install")))

(unionize 'chez-racket 'latest
	  `((install . ,(lambda () (chez-racket-install "HEAD")))))

(unionize 'chez-racket 'latest
	  `((install . ,(lambda () (chez-racket-install "HEAD")))))

(define chicken-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/chicken/"))

    (display (string-append "* Installing chicken @ " work "\n"))

    (guard (ex (else #f))
	   (delete-file-hierarchy work))
    (create-directory* work)

    ;; install latest release to be able to compile from git
    (run work '() "wget" "https://code.call-cc.org/releases/5.3.0/chicken-5.3.0.tar.gz")
    (run work '() "tar" "xf" "chicken-5.3.0.tar.gz")
    (run (string-append work "/chicken-5.3.0") '()
         "make"
         (string-append "PREFIX=" work)
         (string-append "-j" (number->string (worker-count))))
    (run (string-append work "/chicken-5.3.0") '()
         "make" "install"
         (string-append "PREFIX=" work))

    ;; TODO: support cloning with full history
    (run work '() "git" "clone" "--depth=1" "git://code.call-cc.org/chicken-core" "src")
    (run (string-append work "/src/") '() "git" "checkout" version)
    (run (string-append work "/src") '()
         "make"
         (string-append "PREFIX=" work)
         (string-append "-j" (number->string (worker-count))))
    (run (string-append work "/src") '()
         "make" "install"
         (string-append "PREFIX=" work))))

(unionize 'chicken 'latest
	  `((install . ,(lambda () (chicken-install "HEAD")))))

(define cyclone-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/cyclone/"))

    (display (string-append "* Installing cyclone @ " work "\n"))

    (guard (ex (else #f))
	   (delete-file-hierarchy work))
    (create-directory* work)

    ;; install latest release to be able to compile from git
    (run work '() "wget" "https://github.com/justinethier/cyclone-bootstrap/archive/refs/tags/v0.33.0.tar.gz")
    (run work '() "tar" "xf" "v0.33.0.tar.gz")
    (run (string-append work "/cyclone-bootstrap-0.33.0")
         '()
         "make"
         (string-append "PREFIX=" work)
         (string-append "-j" (number->string (worker-count))))
    (run (string-append work "/cyclone-bootstrap-0.33.0")
         '()
         "make" "install"
         (string-append "PREFIX=" work))

    ;; TODO: support cloning with full history
    (run work '() "git" "clone" "--depth=1" "https://github.com/justinethier/cyclone" "src")
    (run (string-append work "/src/") '() "git" "checkout" version)
    (let ((PATH (string-append (get-environment-variable "PATH") ":" work "/bin")))
      (run (string-append work "/src")
           `((PATH . ,PATH))
           "make"
           (string-append "PREFIX=" work)
           (string-append "-j" (number->string (worker-count))))
      (run (string-append work "/src")
           `((PATH . ,PATH))
           "make" "install"
           (string-append "PREFIX=" work)))))

(unionize 'chicken 'latest
	  `((install . ,(lambda () (chicken-install "HEAD")))))

(define united-prefix-set
  (lambda (directory)
    (run #f `((UNITED_PREFIX . ,directory)) "sh" "-c" "$SHELL")))

(define make-filepath
  (lambda (filepath)
    (cond
     ((string=? filepath ".") (current-directory))
     ((char=? (string-ref filepath 0) #\/) filepath)
     (else (string-append (current-directory) "/" filepath)))))

(define united-install
  (lambda (args)
    (call-with-values (lambda () (command-line-parse* args))
      (lambda (options schemes extra)
	(when (null? args)
	  (united-usage)
	  (exit 1))
	(let loop ((schemes schemes))
	  (unless (null? schemes)
	    (case (string->symbol (car schemes))
	      ((chibi) (chibi-install "HEAD"))
              ((chez-cisco) (chez-cisco-install "HEAD"))
              ((chez-racket) (chez-racket-install "HEAD"))
              ((chicken) (chicken-install "HEAD"))
              ((guile) (guile-install "HEAD"))
              ((gambit) (gambit-install "HEAD"))
              ((gauche) (gauche-install "HEAD"))
              ((cyclone) (cyclone-install "HEAD")))
	    (loop (cdr schemes))))))))

(unionize 'cyclone 'latest
	  `((install . ,(lambda () (cyclone-install "HEAD")))))

(define accumulator-singleton-flush '(accumulator singleton flush))

(define (accumulator-flush? object)
  (eq? object accumulator-singleton-flush))

(define accumulator-flush
  (lambda (accumulator)
    (accumulator accumulator-singleton-flush)))

(define make-accumulator
  (lambda ()
    (let ((out '()))
      (lambda (object)
        (if (accumulator-flush? object)
            out
            (set! out (cons object out)))))))

(define maybe-display-errors-and-exit
  (lambda (title errors)
    (let ((errors (accumulator-flush errors)))
      (unless (null? errors)
        (display (string-append "* Faulty arguments for: " title "\n"))
        (let loop ((errors errors))
          (unless (null? errors)
            (display (string-append "** " (caar errors) ": "))
            (write (cdar errors))
            (newline)
            (loop (cdr errors))))
        (exit 1)))))

(define chicken-run
  (lambda (arguments)
    (apply run
           #f
           '()
           (string-append (united-prefix-ref) "/chicken/bin/csi")
           arguments)))

(define chibi-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files extensions arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "chibi exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "chibi exec expect only one file" files)))
        (unless (null? extensions)
          (errors (cons "chibi exec does not support custom extensions" extensions)))
        (unless (null? arguments)
          (errors (cons "chibi exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "chibi exec" errors)

        (let ((arguments (append-map (lambda (x) (list "-I" x)) directories)))
          (chibi-run (append arguments files extra)))))))

(define cyclone-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files extensions arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "cyclone exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "cyclone exec expect only one file" files)))
        (unless (null? extensions)
          (errors (cons "cyclone exec does not support custom extensions" extensions)))
        (unless (null? arguments)
          (errors (cons "cyclone exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "cyclone exec" errors)

        (let ((arguments (append-map (lambda (x) (list "-I" x)) directories))
              (a.out (basename-without-extension (car files))))
          (apply run #f
                 '()
                 (string-append (united-prefix-ref) "/cyclone/bin/cyclone")
                 (append arguments files))
          (apply run (directory-path (car files))
                 '()
                 (string-append "./" a.out)
                 extra))))))

(define gauche-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files extensions arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "gauche exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "gauche exec expect only one file" files)))
        (unless (null? extensions)
          (errors (cons "gauche exec does not support custom extensions" extensions)))
        (unless (null? arguments)
          (errors (cons "gauche exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "gauche exec" errors)

        (let ((arguments (map (lambda (x) (string-append "-I" x)) directories)))
          (apply run
                 #f
                 '()
                 (string-append (united-prefix-ref) "/gauche/bin/gosh")
                 "-r7"
                 (append arguments files extra)))))))

(define guile-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files extensions arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "guile exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "guile exec expect only one file" files)))
        (unless (null? extensions)
          (errors (cons "guile exec does not support custom extensions" extensions)))
        (unless (null? arguments)
          (errors (cons "guile exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "guile exec" errors)

        (let ((arguments (append-map (lambda (x) (list "-L" x)) directories)))
          (apply run
                 #f
                 '()
                 (string-append (united-prefix-ref) "/guile/bin/guile")
                 "--r7rs"
                 (append arguments files extra)))))))

(define chibi-repl
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files extensions arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "chibi repl does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "chibi repl does not support files" files)))
        (unless (null? extensions)
          (errors (cons "chibi repl does not support custom extensions" extensions)))
        (unless (null? arguments)
          (errors (cons "chibi repl does not support arguments" arguments)))
        (unless (null? extra)
          (errors (cons "chibi repl does not support extra arguments" extra)))

        (maybe-display-errors-and-exit "chibi repl" errors)

        (let ((arguments (append-map (lambda (x) (list "-I" x)) directories)))
          (chibi-run arguments))))))

(define chibi-version
  (lambda ()
    (chibi-run (list "-V"))))

(define guile-repl
  (lambda ()
    (apply run
           #f
           '()
           (string-append (united-prefix-ref) "/guile/bin/guile")
           (list "--r7rs"))))

(define guile-version
  (lambda ()
    (apply run
           #f
           '()
           (string-append (united-prefix-ref) "/guile/bin/guile")
           (list "--version"))))

(define united-repl
  (lambda (scheme args)
    (case (string->symbol scheme)
      ((chibi) (chibi-repl args))
      ((chez-cisco) (chez-run "chez-cisco" '()))
      ((chez-racket) (chez-run "chez-racket" '()))
      ((guile) (guile-repl))
      ((gauche) (gauche-repl))
      ((cyclone) (cyclone-repl))
      ((chicken) (chicken-repl)))))

(define directory-path
  (lambda (string)
    (let loop ((index (string-length string)))
      (if (char=? (string-ref string (- index 1)) #\/)
          (substring string 0 index)
          (loop (- index 1))))))

(define basename
  (lambda (string)
    (let loop ((index (string-length string)))
      (if (char=? (string-ref string (- index 1)) #\/)
          (substring string index (string-length string))
          (loop (- index 1))))))

(define basename-without-extension
  (lambda (string)
    (define filename (basename string))
    (let loop ((index (string-length filename)))
      (if (char=? (string-ref filename (- index 1)) #\.)
          (substring filename 0 (- index 1))
          (loop (- index 1))))))

(define chibi-run
   (lambda (arguments)
     (apply run
            #f
            `((LD_LIBRARY_PATH . ,(string-append (united-prefix-ref) "/chibi/lib/")))
            (string-append (united-prefix-ref) "/chibi/bin/chibi-scheme")
            arguments)))

(define chicken-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files extensions arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "chicken exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "chicken exec expect only one file" files)))
        (unless (null? extensions)
          (errors (cons "chicken exec does not support custom extensions" extensions)))
        (unless (null? arguments)
          (errors (cons "chicken exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "chicken exec" errors)

        (run (directory-path (car files))
             '()
             (string-append (united-prefix-ref) "/chicken/bin/csm")
             "-r7rs"
             ".")
        (run (directory-path (car files))
             '()
             (string-append (united-prefix-ref) "/chicken/bin/csc")
             "-R" "r7rs" (car files) "-o" (basename-without-extension (car files)))
        (apply run (directory-path (car files))
               '()
               (string-append "./" (basename-without-extension (car files)))
               extra)))))

(define chicken-version
  (lambda ()
    (apply run
           #f
           '()
           (string-append (united-prefix-ref) "/chicken/bin/csi")
           (list "-version"))))

(define gauche-version
  (lambda ()
    (apply run
           #f
           '()
           (string-append (united-prefix-ref) "/gauche/bin/gosh")
           (list "-V"))))

(define chicken-repl
  (lambda ()
    (apply run
           #f
           '()
           (string-append (united-prefix-ref) "/chicken/bin/csi")
           '())))

(define gauche-repl
  (lambda ()
    (apply run
           #f
           '()
           (string-append (united-prefix-ref) "/gauche/bin/repl")
           (list "-r7"))))

(define cyclone-repl
  (lambda ()
    (apply run
           #f
           '()
           (string-append (united-prefix-ref) "/cyclone/bin/icyc")
           '())))

(define cyclone-version
  (lambda ()
    (apply run
           #f
           '()
           (string-append (united-prefix-ref) "/cyclone/bin/icyc")
           '("-v"))))

(define united-version
  (lambda (scheme)
    (case (string->symbol scheme)
      ((chibi) (chibi-version))
      ((chez-cisco) (chez-cisco-version))
      ((chez-racket) (chez-racket-version))
      ((cyclone) (cyclone-version))
      ((gauche) (gauche-version))
      ((chicken) (chicken-version)))))

(define united-not-implemented
  (lambda (scheme)
    (display (string-append "* That command is not implemented for: " scheme))))

(define united-compile
  (lambda (scheme)
    (case (string->symbol scheme)
      ((chibi) (united-not-implemented "chibi"))
      ((chez-cisco) (united-not-implemented "chez-cisco"))
      ((chez-racket) (united-not-implemented "chez-racket")))))

(define united-exec
  (lambda (scheme args)
    (case (string->symbol scheme)
      ((gauche) (gauche-exec args))
      ((guile) (guile-exec args))
      ((chibi) (chibi-exec args))
      ((chicken) (chicken-exec args))
      ((cyclone) (cyclone-exec args))
      ((chez-cisco) (chez-cisco-exec args))
      ((chez-racket) (chez-racket-exec args)))))

(define chez-run
  (lambda (chez arguments)
    (apply run
           #f
           '()
           (string-append (united-prefix-ref) "/" chez "/bin/scheme")
           arguments)))

(define string-join
  (lambda (strings separator)
    (let loop ((out (car strings))
               (strings (cdr strings)))
      (if (null? strings)
          out
          (loop (string-append out separator (car strings))
                (cdr strings))))))

(define chez-cisco-version
  (lambda ()
    (chez-run "chez-cisco" (list "--version"))))

(define chez-racket-version
  (lambda ()
    (chez-run "chez-racket" (list "--version"))))

(define chez-cisco-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files extensions arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "chez-cisco exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "chez-cisco exec expect only one file" files)))
        (unless (null? arguments)
          (errors (cons "chez-cisco exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "chez-cisco exec" errors)

        (let ((arguments '()))
          (unless (null? directories)
            (set! arguments (cons* "--libdirs"
                                   (string-join directories ":")
                                   arguments)))
          (unless (null? extensions)
            (set! arguments (cons* "--libexts"
                                   (string-join extensions ":")
                                  arguments)))
          (set! arguments (append arguments (list "--program" (car files))))

          (chez-run "chez-cisco" (append arguments extra)))))))

(define chez-racket-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files extensions arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "chez-racket exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "chez-racket exec expect only one file" files)))
        (unless (null? arguments)
          (errors (cons "chez-racket exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "chez-racket exec" errors)
        (let ((arguments '()))
          (unless (null? directories)
            (set! arguments (cons* "--libdirs"
                                   (string-join directories ":")
                                   arguments)))
          (unless (null? extensions)
            (set! arguments (cons* "--libexts"
                                   (string-join extensions ":")
                                   arguments)))

          (set! arguments (append arguments (list "--program" (car files))))

          (chez-run "chez-racket" (append arguments extra)))))))

(match (cdr (command-line))
 (("available" scheme) (united-available scheme))
 (("available") (united-available #f))
 (("install" . args) (united-install args))
 (("prefix" directory) (united-prefix-set directory))
 (("prefix") (united-prefix-display))
 ((scheme "compile" . args) (united-compile scheme args))
 ((scheme "exec" . args) (united-exec scheme args))
 ((scheme "repl" . args) (united-repl scheme args))
 ((scheme "version") (united-version scheme))
 (else (united-usage)))
