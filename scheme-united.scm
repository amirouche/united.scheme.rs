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
    ;; TODO: save previous value, if any, and re-install it after.
    (let loop ((env env))
      (unless (null? env)
        ;; XXX: Override existing variable
	(setenv (symbol->string (caar env)) (cdar env))
	(loop (cdr env))))
    (call-with-values thunk
      (lambda args
	(let loop ((env env))
	  (unless (null? env)
            ;; XXX: Unconditionally unset variable
	    (unsetenv (symbol->string (caar env)))
	    (loop (cdr env))))
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
  scheme-united SCHEME repl [DIRECTORY ...] [EXTENSION ...]
  scheme-united SCHEME compile [--optimize-level=0-3] [DIRECTORY ...] [EXTENSION ...] PROGRAM A.OUT
  scheme-united SCHEME exec [DIRECTORY ...] [EXTENSION ...] PROGRAM [-- EXTRA ...]
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
         ;; the first char is a dot, the associated path is neither a file
         ;; or directory, hence it is prolly an extension... breaks when the
         ;; user made a typo in a file or directory name.
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

(define (run directory env . command)
  (unless (call-with-env env (lambda ()
			       (if directory
				   (with-directory directory (lambda () (apply system? command)))
				   (apply system? command))))
    (raise run-singleton-failure)))

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

(define chibi-install-version->reference
  (lambda (version)
    (case version
      ((latest) "HEAD")
      ((stable) "stable")
      (else version))))

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
    (define work (string-append (united-prefix-ref) "/chibi"))

    (display (string-append "* Installing chibi @ " work "\n"))

    (guard (ex (else #f))
	   (delete-file-hierarchy work))
    (create-directory* work)

    ;; TODO: support cloning with full history
    (run work '() "git" "clone" "--depth=1" "https://github.com/ashinn/chibi-scheme/" "src")
    (run (string-append work "/src/") '() "git" "checkout"
	 (chibi-install-version->reference version))
    (run (string-append work "/src/")
	 '()
	 "make"
	 (string-append "-j" (number->string (worker-count)))
	 (string-append "PREFIX=" work)
	 "install")
    (create-directory* (string-append work "/../bin"))
    (let loop ((programs (list "chibi-doc" "chibi-ffi" "chibi-scheme" "snow-chibi" "snow-chibi.scm")))
      (unless (null? programs)
	(symbolic-link-file (string-append work "/bin/" (car programs))
			    (string-append work "/../bin/" (car programs)))
	(loop (cdr programs))))))

(unionize 'chibi "latest"
	  `((install . ,(lambda () (chibi-install 'latest)))))
(unionize 'chibi "stable"
	  `((install . ,(lambda () (chibi-install 'stable)))))
(unionize 'chibi 'v0.10
	  `((install . ,(lambda () (chibi-install "0.10")))))

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
	      ((chibi) (chibi-install 'latest)))
	    (loop (cdr schemes))))))))

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

(define chibi-run
  (lambda (arguments)
    (apply run
           #f
           `((LD_LIBRARY_PATH . ,(string-append (united-prefix-ref) "/chibi/lib/")))
           (string-append (united-prefix-ref) "/bin/chibi-scheme")
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

(define united-repl
  (lambda (scheme args)
    (case (string->symbol scheme)
      ((chibi) (chibi-repl args)))))

(define united-version
  (lambda (scheme)
    (case (string->symbol scheme)
      ((chibi) (chibi-version)))))

(define united-not-implemented
  (lambda (scheme)
    (display (string-append "* That command is not implemented for: " scheme))))

(define united-compile
  (lambda (scheme)
    (case (string->symbol scheme)
      ((chibi) (united-not-implemented "chibi")))))

(define united-exec
  (lambda (scheme args)
    (case (string->symbol scheme)
      ((chibi) (chibi-exec args)))))

(match (cdr (command-line))
 (("available") (united-available #f))
 (("install" . args) (united-install args))
 (("prefix" directory) (united-prefix-set directory))
 (("prefix") (united-prefix-display))
 ((scheme "available") (united-available scheme))
 ((scheme "compile" . args) (united-compile scheme args))
 ((scheme "exec" . args) (united-exec scheme args))
 ((scheme "repl" . args) (united-repl scheme args))
 ((scheme "version") (united-version scheme))
 (else (united-usage)))
