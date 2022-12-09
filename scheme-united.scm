#!/usr/bin/env chibi-scheme
(import (scheme small))
(import (scheme list))
(import (scheme process-context))
(import (only (srfi 130) string-prefix?))
(import (chibi process))
(import (chibi match))
(import (chibi filesystem))
(import (only (chibi ast) setenv unsetenv))


(define pk
  (lambda args
    (when (get-environment-variable "UNITED_DEBUG")
      (display ";; " (current-error-port))
      (write args (current-error-port))
      (newline (current-error-port)))
    (car (reverse args))))

(define call-with-env
  (lambda (env thunk)
    ;; backup variables before overriding
    (define original (get-environment-variables))
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
              (let loop ((original original))
                (unless (null? original)
                  (setenv (caar original) (cdar original))
                  (loop (cdr original))))
              (begin
                ;; unset variables from ENV
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
  scheme-united all check [DIRECTORY ...]
  scheme-united SCHEME check [DIRECTORY ...]
  scheme-united SCHEME exec [DIRECTORY ...] PROGRAM [-- EXTRA ...]
  scheme-united SCHEME repl [DIRECTORY ...]
  scheme-united SCHEME version
  scheme-united available
  scheme-united install SCHEME ...
  scheme-united prefix [DIRECTORY]

")))

(define command-line-parse-standalone
  (lambda (arguments)

    (define guess
      (lambda (string)
        (cond
         ((file-directory? string) (values 'directory (make-filepath string)))
         ((file-exists? string)
          (values 'file (make-filepath string)))
         (else (values 'unknown string)))))

    (let loop ((arguments arguments)
               (directories '())
               (files '())
               (unknowns '()))
      (if (null? arguments)
          (values directories files unknowns)
          (call-with-values (lambda () (guess (car arguments)))
            (lambda (type value)
              (case type
                ((directory) (loop (cdr arguments)
                                   (cons value directories)
                                   files
                                   unknowns))
                ((file) (loop (cdr arguments)
                              directories
                              (cons value files)
                              unknowns))
                ((unknown) (loop (cdr arguments)
                                 directories
                                 files
                                 (cons value unknowns))))))))))

(define command-line-parse
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse* arguments))
      (lambda (keywords standalone extra)
        (call-with-values (lambda () (command-line-parse-standalone standalone))
          (lambda (directories files unknowns)
            (values keywords directories files unknowns extra)))))))

(define run-singleton-failure '(run singleton failure))

(define run-failure?
  (lambda (object)
    (eq? object run-singleton-failure)))

(define run
  (lambda (directory env . command)
    (pk 'apply 'run directory env command)
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
  (lambda ()
    (map symbol->string (delete-duplicates (map car union)))))

(define united-available-display
  (lambda ()
    (for-each (lambda (x) (display x) (newline))
              (united-available))))

(define worker-count
  (lambda ()
    (let ((count (get-environment-variable "UNITED_WORKER_COUNT")))
      (if (and count (string->number count))
          (string->number count)
          1))))

(define united-prefix-ref
  (lambda ()
    (or (get-environment-variable "UNITED_PREFIX")
        (string-append (get-environment-variable "HOME")
                       "/.local/opt/united/"))))

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

(define stklos-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/stklos/"))

    (display (string-append "* Installing stklos @ " work "\n"))

    (guard (ex (else #f))
           (delete-file-hierarchy work))
    (create-directory* work)

    (run work '() "git" "clone" "--depth=1" "https://github.com/egallesio/STklos" "src")
    (run (string-append work "/src/") '() "git" "checkout" version)
    (run (string-append work "/src/") '()
         "bash" "configure" (string-append "--prefix=" work))
    (run (string-append work "/src/")
         '()
         "make")
    (run (string-append work "/src/")
         '()
         "make"
         "install")))

(unionize 'stklos 'latest
          `((install . ,(lambda () (stklos-install "HEAD")))))

(define chez-cisco-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/chez-cisco/"))

    (display (string-append "* Installing chez-cisco @ " work "\n"))

    (guard (ex (else #f))
           (delete-file-hierarchy work))
    (create-directory* work)

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

    (run (string-append work "../") '() "rm" "-rf" "guile")
    (create-directory* work)

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

    (run work '() "git" "clone" "https://github.com/gambit/gambit/" "src")
    (run (string-append work "/src/") '() "git" "checkout" version)
    (run (string-append work "/src/") '()
         "sh" "configure" (string-append "--prefix=" work))
    (run (string-append work "/src/")
         '()
         "make"
         #;(string-append "-j" (number->string (worker-count))))
    (run (string-append work "/src/")
         '()
         "make"
         "install")))

(unionize 'gambit 'latest
          `((install . ,(lambda () (gambit-install "HEAD")))))

(define racket-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/racket/"))

    (display (string-append "* Installing racket @ " work "\n"))

    (guard (ex (else #f))
           (delete-file-hierarchy work))
    (create-directory* work)

    (run work '() "git" "clone" "--depth=1" "https://github.com/racket/racket/" "src")
    (run (string-append work "/src/") '() "git" "checkout" version)
    (run (string-append work "/src/")
         '()
         "make"
         (string-append "-j" (number->string (worker-count)))
         "unix-style"
         (string-append "PREFIX=" work))
    (run (string-append work "/bin/")
         '()
         "./raco"
         "pkg" "install" "--batch" "--deps" "search-auto" "--scope" "installation" "r7rs")))

(unionize 'racket 'latest
          `((install . ,(lambda () (racket-install "HEAD")))))

(define gerbil-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/gerbil/"))
    (define gambit (string-append (united-prefix-ref) "/gambit/"))

    (display (string-append "* Installing gerbil @ " work "\n"))

    (guard (ex (else #f))
           (delete-file-hierarchy work))
    (create-directory* work)

    (run work '() "git" "clone" "https://github.com/vyzo/gerbil/" "src")
    (run (string-append work "/src/") '() "git" "checkout" version)
    (let ((PATH (string-append gambit "/bin/:" (get-environment-variable "PATH"))))
      (run (string-append work "/src/src/")
           `((GERBIL_HOME . ,work)
             (GERBIL_BUILD_CORES . ,(number->string (worker-count)))
             (PATH . ,PATH))
           "gsi-script" "configure"
           (string-append "--prefix=" work)
           (string-append "--with-gambit=" gambit))
      (run (string-append work "/src/src/")
           `((GERBIL_HOME . ,work)
             (GERBIL_BUILD_CORES . ,(number->string (worker-count)))
             (PATH . ,PATH))
           "sh" "build.sh")
      (run (string-append work "/src/src/")
           `((GERBIL_HOME . ,work)
             (GERBIL_BUILD_CORES . ,(number->string (worker-count)))
             (PATH . ,PATH))
           "gsi-script" "install"))))

;; (unionize 'gerbil 'latest
;;           `((install . ,(lambda () (gerbil-install "HEAD")))))

(define gauche-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/gauche/"))

    (display (string-append "* Installing gauche @ " work "\n"))

    (guard (ex (else #f))
           (delete-file-hierarchy work))
    (create-directory* work)

    (run work '() "wget" "https://raw.githubusercontent.com/shirok/get-gauche/master/get-gauche.sh")
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

(define sagittarius-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/sagittarius/"))

    (display (string-append "* Installing sagittarius @ " work "\n"))

    (guard (ex (else #f))
           (delete-file-hierarchy work))
    (create-directory* work)

    (run work '() "wget" "https://bitbucket.org/ktakashi/sagittarius-scheme/downloads/sagittarius-0.9.9.tar.gz")
    (run work '() "tar" "xf" "sagittarius-0.9.9.tar.gz")
    (run (string-append work "/sagittarius-0.9.9") '()
         "cmake"
         "."
         (string-append "-DCMAKE_INSTALL_PREFIX=" work))
    (run (string-append work "/sagittarius-0.9.9") '()
         "make"
         (string-append "-j" (number->string (worker-count))))
    (run (string-append work "/sagittarius-0.9.9") '()
         "make"
         "install")))

(unionize 'sagittarius 'latest
          `((install . ,(lambda () (sagittarius-install "HEAD")))))

(define mit-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/mit/"))

    (display (string-append "* Installing mit @ " work "\n"))

    ;; For some reason the following does not work
    ;; (guard (ex (else #f))
    ;;        (delete-file-hierarchy work))
    (run #f '() "rm" "-rf" work)
    (create-directory* work)

    ;; install latest release to be able to compile from git
    (run work '() "wget" "https://ftp.gnu.org/gnu/mit-scheme/stable.pkg/11.2/mit-scheme-11.2-x86-64.tar.gz")
    (run work '() "tar" "xf" "mit-scheme-11.2-x86-64.tar.gz")
    (run (string-append work "/mit-scheme-11.2/src/") '()
         "sh" "configure" "--disable-x11" (string-append "--prefix=" work))
    (run (string-append work "/mit-scheme-11.2/src/") '()
         "make" (string-append "-j" (number->string (worker-count)))
         ;; XXX: Drop 'warnings as errors' aka. Werror
         "CFLAGS=")

    (run (string-append work "/mit-scheme-11.2/src/") '()
         "make" "install")

    (run work '() "git" "clone" "--depth=1" "https://git.savannah.gnu.org/git/mit-scheme.git" "src")
    (run (string-append work "/src/") '() "git" "checkout" version)
    (let ((PATH (string-append (get-environment-variable "PATH") ":" work "/bin")))
      (run (string-append work "/src/src/") `((PATH . ,PATH))
         "sh" "Setup.sh")
      (run (string-append work "/src/src/") `((PATH . ,PATH))
         "sh" "configure" "--disable-x11" (string-append "--prefix=" work))
      (run (string-append work "/src/src/") `((PATH . ,PATH))
           "make" (string-append "-j" (number->string (worker-count)))
           "CFLAGS=")
      (run (string-append work "/src/src/") `((PATH . ,PATH))
           "make" "install"))))

(unionize 'mit 'latest
          `((install . ,(lambda () (mit-install "HEAD")))))

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

    (run work '() "git" "clone" "--depth=1" "git://code.call-cc.org/chicken-core" "src")
    (run (string-append work "/src/") '() "git" "checkout" version)
    (let ((PATH (string-append (get-environment-variable "PATH") ":" work "/bin")))
      (run (string-append work "/src") `((PATH . ,PATH))
         "make"
         (string-append "PREFIX=" work)
         (string-append "-j" (number->string (worker-count))))
      (run (string-append work "/src") '()
           "make" "install"
           (string-append "PREFIX=" work))
      (run #f
           '()
           (string-append (united-prefix-ref) "/chicken/bin/chicken-install")
           "csm")
      (run #f
           '()
           (string-append (united-prefix-ref) "/chicken/bin/chicken-install")
           "r7rs"))))

(unionize 'chicken 'latest
          `((install . ,(lambda () (chicken-install "HEAD")))))

(define loko-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/loko/"))

    (display (string-append "* Installing loko @ " work "\n"))

    (guard (ex (else #f))
           (delete-file-hierarchy work))
    (create-directory* work)

    (run work '() "wget" "https://gitlab.com/akkuscm/akku/-/archive/v1.1.0/akku-v1.1.0.tar.bz2")
    (run work '() "tar" "xvf" "akku-v1.1.0.tar.bz2")

    (let* ((PATH (string-append (string-append work "/bin:")
                               (united-prefix-ref) "/chez-cisco/bin:"
                               (united-prefix-ref) "/guile/bin:"
                               (get-environment-variable "PATH")))
	  (PKG_CONFIG_PATH (string-append (united-prefix-ref) "/guile/lib/pkgconfig/"))
	  (env `((PATH . ,PATH) (PKG_CONFIG_PATH . ,PKG_CONFIG_PATH))))
      (run (string-append work "/akku-v1.1.0") env  "sh" "bootstrap")
      (run (string-append work "/akku-v1.1.0") env "sh" "configure" (string-append "--prefix=" work))
      (run (string-append work "/akku-v1.1.0") env "make" (string-append "-j" (number->string (worker-count))))
      (run (string-append work "/akku-v1.1.0") env "make" "install")
      (run work env "git" "clone" "https://scheme.fail/git/loko.git/" "src")
      (run (string-append work "/src/") env "make"
           ;; TODO: uncomment when the following is merged:
           ;;
           ;;   https://gitlab.com/weinholt/loko/-/merge_requests/4
           ;;
           ;; (string-append "-j" (number->string (worker-count)))
           (string-append "PREFIX=" work))
      (run (string-append work "/src/") env
           "make" (string-append "PREFIX=" work) "install"))))

(unionize 'loko 'latest
          `((install . ,(lambda () (loko-install "HEAD")))))

(define cyclone-install
  (lambda (version)
    (define work (string-append (united-prefix-ref) "/cyclone/"))

    (display (string-append "* Installing cyclone @ " work "\n"))

    (guard (ex (else #f))
           (delete-file-hierarchy work))
    (create-directory* work)

    (run work '() "git" "clone" "--depth=1" "https://github.com/justinethier/cyclone-bootstrap")
    ;; TODO: delete the next line, and uncomment the following two
    ;; lines, when the following is merged:
    ;;
    ;;   https://github.com/justinethier/cyclone/pull/497
    ;;
    (run work '() "git" "clone" "--depth=1" "--branch=fix-include" "https://github.com/amirouche/cyclone" "src")
    ;; (run work '() "git" "clone" "--depth=1" "https://github.com/justinethier/cyclone" "src")
    ;; (run (string-append work "/src/") '() "git" "checkout" version)
    (run (string-append work "/src/") '() "make" "bootstrap")

    (run (string-append work "/cyclone-bootstrap")
         '()
         "make"
         (string-append "PREFIX=" work)
         (string-append "-j" (number->string (worker-count))))
    (run (string-append work "/cyclone-bootstrap")
         '()
         "make"
         "install"
         (string-append "PREFIX=" work))))

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
              ((mit) (mit-install "HEAD"))
              ((stklos) (stklos-install "HEAD"))
              ((chez-cisco) (chez-cisco-install "HEAD"))
              ((chez-racket) (chez-racket-install "HEAD"))
              ((chicken) (chicken-install "HEAD"))
              ((guile) (guile-install "HEAD"))
              ((gambit) (gambit-install "HEAD"))
              ((racket) (racket-install "HEAD"))
              ((gerbil) (gerbil-install "HEAD"))
              ((gauche) (gauche-install "HEAD"))
              ((loko) (loko-install "HEAD"))
              ((sagittarius) (sagittarius-install "HEAD"))
              ((cyclone) (cyclone-install "HEAD")))
            (loop (cdr schemes))))))))

;; (unionize 'cyclone 'latest
;;           `((install . ,(lambda () (cyclone-install "HEAD")))))

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
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "chibi exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "chibi exec expect only one file" files)))
        (unless (null? arguments)
          (errors (cons "chibi exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "chibi exec" errors)

        (let ((arguments (append-map (lambda (x) (list "-I" x)) directories)))
          (chibi-run (append arguments files extra)))))))


(define directory-files*
  (lambda (directory)
    (remove (lambda (x) (or (string=? x ".") (string=? x ".."))) (directory-files directory))))

(define ftw
  (lambda (directory)
    (let ((paths* (map (lambda (x) (string-append directory "/" x))
                      (directory-files* directory))))
      (lambda ()
        (let loop ((paths paths*))
          (if (null? paths)
              (eof-object)
              (let ((path (car paths)))
                (if (file-directory? path)
                    (loop (append (cdr paths)
                                  (map (lambda (x) (string-append path "/" x))
                                       (directory-files* path))))
                    (begin
                      (set! paths* (cdr paths))
                      path)))))))))

(define gfilter
  (lambda (generator predicate?)
    (lambda ()
      (let loop ()
        (let ((object (generator)))
          (if (eof-object? object)
              (eof-object)
              (if (predicate? object)
                  object
                  (loop))))))))

(define gappend
  (lambda (generators)
    (lambda ()
      (let loop ()
        (if (null? generators)
            (eof-object)
            (let ((object ((car generators))))
              (if (eof-object? object)
                  (begin
                    (set! generators (cdr generators))
                  (loop))
                  object)))))))


(define gmap
  (lambda (proc generator)
    (lambda ()
      (let ((object (generator)))
        (if (eof-object? object)
            (eof-object)
            (proc object))))))

(define extract-check-spec
  (lambda (filepath)

    (define check?
      (lambda (spec)
        ;; XXX: guard against spec that is not just a symbol, TODO:
        ;; support export with rename?
        (guard (ex (else #f))
               (string-prefix? "~check-" (symbol->string spec)))))

    (define extract-checks
      (lambda (exp)
        (match exp
               (('export e ...) (filter check? e))
               (else '()))))

    (let ((maybe-library (call-with-input-file filepath read)))
      (match maybe-library
             (('define-library name e ...)
              (cons name (cons 'list (map (lambda (x) (list 'cons  (list 'quote (list (cons name x))) x))
                                          (append-map extract-checks e)))))
             (else #f)))))

(define generator->reversed-list
  (lambda (generator)
    (let loop ((out '()))
      (let ((object (generator)))
        (if (eof-object? object)
            out
            (loop (cons object out)))))))

(define make-check-program
  (lambda (scheme directories)
    (let* ((libraries+checks (generator->reversed-list
                              (gmap extract-check-spec
                                    (gfilter (gappend (map ftw directories))
                                             (lambda (x) (string=? (extension x) "sld"))))))
           (program `((import (scheme base))
                      (import ,@(map car libraries+checks))

                      (define checks ,@(map cdr libraries+checks))

                      (display "* ")
                      (display ,scheme)
                      (newline)

                      (define (check-one check)
                        (display "** ")
                        (display (car check))
                        (newline)
                        ((cdr check)))

                      (for-each check-one checks))))

      (call-with-output-file "checks.scm"
        (lambda (port)
          (let loop ((program program))
            (unless (null? program)
              (write (car program) port)
              (loop (cdr program))))))

      "checks.scm")))

(define make-stklos-check-program
  (lambda (scheme directories)
    (let* ((libraries+checks (generator->reversed-list
                              (gmap extract-check-spec
                                    (gfilter (gappend (map ftw directories))
                                             (lambda (x) (string=? (extension x) "sld"))))))
           (program `((import ,@(map car libraries+checks))
                      ;; XXX: Do not import (scheme base). Otherwise
                      ;; STKlos will shrug one way to fix it portably:
                      ;; TODO only import ~check procedures that are
                      ;; necessary.
                      (define checks ,@(map cdr libraries+checks))

                      (display "* ")
                      (display ,scheme)
                      (newline)

                      (define (check-one check)
                        (display "** ")
                        (display (car check))
                        (newline)
                        ((cdr check)))

                      (for-each check-one checks))))

      (call-with-output-file "checks.scm"
        (lambda (port)
          (let loop ((program program))
            (unless (null? program)
              (write (car program) port)
              (loop (cdr program))))))

      "checks.scm")))

(define generator-for-each
  (lambda (proc generator)
    (let loop ()
      (let ((obj (generator)))
        obj
        (if (eof-object? obj)
            (eof-object)
            (begin
              (proc obj)
              (loop)))))))

(define clean-c-compilation-leftovers
  (lambda (directories)

    (define maybe
      (lambda (thunk)
        (guard (ex (else #t))
               (thunk))))

    (define directory/basename
      (lambda (x)
        (string-append (directory-path x) "/" (basename-without-extension x))))

    (define clean
      (lambda (basename)
        (for-each (lambda (x) (maybe (lambda () (delete-file (string-append basename x)))))
                  (list ".c" ".o" ".so" ".meta"))))

    (generator-for-each clean (gmap directory/basename
                                    (gfilter (gappend (map ftw directories))
                                             (lambda (x) (string=? (extension x) "sld")))))))

(define make-chez-check-program
  (lambda (scheme directories)
    (let* ((libraries+checks (generator->reversed-list
                              (gmap extract-check-spec
                                    (gfilter (gappend (map ftw directories))
                                             (lambda (x) (string=? (extension x) "sld"))))))
           (program `((import (chezscheme))
                      (import ,@(map car libraries+checks))

                      (define checks ,@(map cdr libraries+checks))

                      (display "* ")
                      (display ,scheme)
                      (newline)

                      (define (check-one check)
                        (display "** ")
                        (display (car check))
                        (newline)
                        ((cdr check)))

                      (for-each check-one checks))))

      (call-with-output-file "checks.scm"
        (lambda (port)
          (let loop ((program program))
            (unless (null? program)
              (write (car program) port)
              (loop (cdr program))))))

      "checks.scm")))

(define chibi-check
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "chibi check does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "chibi check does not support files" files)))
        (unless (null? arguments)
          (errors (cons "chibi check does not support files" arguments)))
        (unless (null? extra)
          (errors (cons "chibi check does not support extra, as of yet..." extra)))

        (maybe-display-errors-and-exit "chibi check" errors)

        (let ((arguments (append-map (lambda (x) (list "-I" x)) directories))
              (checks.scm (make-check-program "chibi" directories)))
          (chibi-run (append arguments (list checks.scm))))))))

(define stklos-check
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "stklos check does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "stklos check does not support files" files)))
        (unless (null? arguments)
          (errors (cons "stklos check does not support files" arguments)))
        (unless (null? extra)
          (errors (cons "stklos check does not support extra, as of yet..." extra)))

        (maybe-display-errors-and-exit "stklos check" errors)

        (let ((checks.scm (make-stklos-check-program "stklos" directories)))
          (stklos-exec (append directories (list checks.scm))))))))

(define gauche-check
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "gauche check does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "gauche check does not support files" files)))
        (unless (null? arguments)
          (errors (cons "gauche check does not support files" arguments)))
        (unless (null? extra)
          (errors (cons "gauche check does not support extra, as of yet..." extra)))

        (maybe-display-errors-and-exit "gauche check" errors)

        (let ((checks.scm (make-check-program "gauche" directories)))
          (gauche-exec (append directories (list checks.scm))))))))

(define gambit-check
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "gambit check does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "gambit check does not support files" files)))
        (unless (null? arguments)
          (errors (cons "gambit check does not support files" arguments)))
        (unless (null? extra)
          (errors (cons "gambit check does not support extra, as of yet..." extra)))

        (maybe-display-errors-and-exit "gambit check" errors)

        (let ((checks.scm (make-check-program "gambit" directories)))
          (gambit-exec (append directories (list checks.scm))))))))

(define mit-check
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "mit check does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "mit check does not support files" files)))
        (unless (null? arguments)
          (errors (cons "mit check does not support files" arguments)))
        (unless (null? extra)
          (errors (cons "mit check does not support extra, as of yet..." extra)))

        (maybe-display-errors-and-exit "mit check" errors)

        (let ((checks.scm (make-check-program "mit" directories)))
          (mit-exec (append directories (list checks.scm))))))))

(define loko-check
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "loko check does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "loko check does not support files" files)))
        (unless (null? arguments)
          (errors (cons "loko check does not support files" arguments)))
        (unless (null? extra)
          (errors (cons "loko check does not support extra, as of yet..." extra)))

        (maybe-display-errors-and-exit "loko check" errors)

        (let ((checks.scm (make-check-program "loko" directories)))
          (loko-exec (append directories (list checks.scm))))))))

(define racket-check
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "racket check does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "racket check does not support files" files)))
        (unless (null? arguments)
          (errors (cons "racket check does not support files" arguments)))
        (unless (null? extra)
          (errors (cons "racket check does not support extra, as of yet..." extra)))

        (maybe-display-errors-and-exit "racket check" errors)

        (let ((checks.scm (make-check-program "racket" directories)))
          (racket-exec (append directories (list checks.scm))))))))

(define gerbil-check
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "gerbil check does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "gerbil check does not support files" files)))
        (unless (null? arguments)
          (errors (cons "gerbil check does not support files" arguments)))
        (unless (null? extra)
          (errors (cons "gerbil check does not support extra, as of yet..." extra)))

        (maybe-display-errors-and-exit "gerbil check" errors)

        (let ((checks.scm (make-check-program "gerbil" directories)))
          (gerbil-exec (append directories (list checks.scm))))))))

(define guile-check
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "guile check does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "guile check does not support files" files)))
        (unless (null? arguments)
          (errors (cons "guile check does not support files" arguments)))
        (unless (null? extra)
          (errors (cons "guile check does not support extra, as of yet..." extra)))

        (maybe-display-errors-and-exit "guile check" errors)

        (let ((checks.scm (make-check-program "guile" directories)))
          (guile-exec (append directories (list checks.scm))))))))

(define cyclone-check
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "cyclone check does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "cyclone check does not support files" files)))
        (unless (null? arguments)
          (errors (cons "cyclone check does not support files" arguments)))
        (unless (null? extra)
          (errors (cons "cyclone check does not support extra, as of yet..." extra)))

        (maybe-display-errors-and-exit "cyclone check" errors)

        (let ((arguments (append-map (lambda (x) (list "-I" x)) directories))
              (checks.scm (make-check-program "cyclone" directories)))
          (apply run #f
                 '()
                 (string-append (united-prefix-ref) "/cyclone/bin/cyclone")
                 (append arguments (list "checks.scm")))
          (run #f
               '()
               "./checks"))))))

(define chez-racket-check
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "chez-racket check does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "chez-racket check does not support files" files)))
        (unless (null? arguments)
          (errors (cons "chez-racket check does not support files" arguments)))
        (unless (null? extra)
          (errors (cons "chez-racket check does not support extra, as of yet..." extra)))

        (maybe-display-errors-and-exit "chez-racket check" errors)

        (let ((checks.scm (make-chez-check-program "chez-racket" directories)))
          (chez-racket-exec (append directories (list checks.scm))))))))

(define chez-cisco-check
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "chez-cisco check does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "chez-cisco check does not support files" files)))
        (unless (null? arguments)
          (errors (cons "chez-cisco check does not support files" arguments)))
        (unless (null? extra)
          (errors (cons "chez-cisco check does not support extra, as of yet..." extra)))

        (maybe-display-errors-and-exit "chez-cisco check" errors)

        (let ((checks.scm (make-chez-check-program "chez-cisco" directories)))
          (chez-cisco-exec (append directories (list checks.scm))))))))

(define chicken-check
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "chicken check does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "chicken check does not support files" files)))
        (unless (null? arguments)
          (errors (cons "chicken check does not support files" arguments)))
        (unless (null? extra)
          (errors (cons "chicken check does not support extra, as of yet..." extra)))

        (maybe-display-errors-and-exit "chicken check" errors)

        (let ((checks.scm  (make-check-program "chicken" directories)))
          (chicken-exec (append directories (list "checks.scm"))))))))

(define sagittarius-check
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "sagittarius check does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "sagittarius check does not support files" files)))
        (unless (null? arguments)
          (errors (cons "sagittarius check does not support files" arguments)))
        (unless (null? extra)
          (errors (cons "sagittarius check does not support extra, as of yet..." extra)))

        (maybe-display-errors-and-exit "sagittarius check" errors)

        (let ((checks.scm  (make-check-program "sagittarius" directories)))
          (sagittarius-exec (append directories (list "checks.scm"))))))))

(define cyclone-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "cyclone exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "cyclone exec expect only one file" files)))
        (unless (null? arguments)
          (errors (cons "cyclone exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "cyclone exec" errors)

        (clean-c-compilation-leftovers directories)

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

(define gambit-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)

        (define make-search-option
          (lambda (directories)
            (string-append "-:" (string-join
                                 (map (lambda (x) (string-append "search=" x))
                                      directories)
                                 ","))))

        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "gambit exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "gambit exec expect only one file" files)))
        (unless (null? arguments)
          (errors (cons "gambit exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "gambit exec" errors)

        (let* ((a.out (basename-without-extension (car files)))
               (arguments (append (list "-:r7rs" "-exe" "-o" a.out "-nopreload") directories files)))
          (apply run #f
                 '()
                 (string-append (united-prefix-ref) "/gambit/bin/gsc")
                 arguments)
          (apply run #f '() (string-append "./" a.out) (make-search-option directories) extra))))))

(define racket-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)

        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "racket exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "racket exec expect only one file" files)))
        (unless (null? arguments)
          (errors (cons "racket exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "racket exec" errors)

        (let ((search (append-map (lambda (x) (list "-S" x)) directories )))
          (apply run #f
                 '()
                 (string-append (united-prefix-ref) "/racket/bin/racket")
                 "-I" "r7rs"
                 (append search (list "-f") files extra)))))))

(define gerbil-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)

        (define make-search-option
          (lambda (directories)
            (string-append "-:" (string-join
                                 (map (lambda (x) (string-append "search=" x))
                                      directories)
                                 ","))))

        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "gerbil exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "gerbil exec expect only one file" files)))
        (unless (null? arguments)
          (errors (cons "gerbil exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "gerbil exec" errors)

        (apply run #f
               `((GERBIL_LOADPATH . ,(string-join directories ":")))
               (string-append (united-prefix-ref) "/gerbil/bin/gxi")
               "--lang" "r7rs" files)))))

(define gambit-repl
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "gambit repl does not support keywords" keywords)))
        (unless (zero? (length files))
          (errors (cons "gambit repl expect zero files" files)))
        (unless (null? arguments)
          (errors (cons "gambit repl does not support arguments" arguments)))

        (maybe-display-errors-and-exit "gambit repl" errors)

        (let* ((arguments (cons "-:r7rs" directories)))
          (apply run #f
                 '()
                 (string-append (united-prefix-ref) "/gambit/bin/gsi")
                 arguments))))))

(define gambit-version
  (lambda ()
    (run #f
         '()
         (string-append (united-prefix-ref) "/gambit/bin/gsc")
         "-v")))

(define gauche-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "gauche exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "gauche exec expect only one file" files)))
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

(define stklos-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "stklos exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "stklos exec expect only one file" files)))
        (unless (null? arguments)
          (errors (cons "stklos exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "stklos exec" errors)

        (let ((arguments (append-map (lambda (x) (list "-I" x)) directories)))
          (apply run
                 #f
                 '()
                 (string-append (united-prefix-ref) "/stklos/bin/stklos")

                 (append arguments (list "-l" "init.stk") (cons "-f" files) extra)))))))

(define guile-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "guile exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "guile exec expect only one file" files)))
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
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "chibi repl does not support keywords" keywords)))
        (unless (null? files)
          (errors (cons "chibi repl does not support files" files)))
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
      ((gambit) (gambit-repl args))
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

(define extension
  (lambda (string)
    (let loop ((index (string-length string)))
      (if (zero? index)
          ""
          (if (char=? (string-ref string (- index 1)) #\.)
              (substring string index (string-length string))
              (loop (- index 1)))))))

(define basename-without-extension
  (lambda (string)
    (define filename (basename string))
    (let loop ((index (string-length filename)))
      (if (char=? (string-ref filename (- index 1)) #\.)
          (substring filename 0 (- index 1))
          (loop (- index 1))))))

(define path-without-extension
  (lambda (filename)
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
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "chicken exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "chicken exec expect only one file" files)))
        (unless (null? arguments)
          (errors (cons "chicken exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "chicken exec" errors)

        (clean-c-compilation-leftovers directories)

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

(define loko-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "loko exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "loko exec expect only one file" files)))
        (unless (null? arguments)
          (errors (cons "loko exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "loko exec" errors)

        (let ((LOKO_LIBRARY_PATH (string-join directories ":")))
          (apply run
                 #f
                 `((LOKO_LIBRARY_PATH . ,LOKO_LIBRARY_PATH))
                 (string-append (united-prefix-ref) "/loko/bin/loko")
                 "-std=r7rs"
                 "--compile"
                 files)
          (apply run
                 #f
                 '()
                 (path-without-extension (make-filepath (car files)))
                 extra))))))

(define sagittarius-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "sagittarius exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "sagittarius exec expect only one file" files)))
        (unless (null? arguments)
          (errors (cons "sagittarius exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "sagittarius exec" errors)

        (apply run
               #f
               '()
               (string-append (united-prefix-ref) "/sagittarius/bin/sagittarius")
               "-r7"
               (append (map (lambda (x) (string-append "-L" x)) directories) files extra))))))

(define mit-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)

        (define find-scheme-libraries
          (lambda (directories)
            (string-append "(find-scheme-libraries! "
                           (string-join (map (lambda (x) (string-append "\"" x "\"")) directories) " ")
                           ")")))

        (define errors (make-accumulator))
        (unless (null? keywords)
          (errors (cons "mit exec does not support keywords" keywords)))
        (when (or (null? files) (not (= 1 (length files))))
          (errors (cons "mit exec expect only one file" files)))
        (unless (null? arguments)
          (errors (cons "mit exec does not support arguments" arguments)))

        (maybe-display-errors-and-exit "mit exec" errors)

        (clean-c-compilation-leftovers directories)

        (apply run
               #f
               '()
               (string-append (united-prefix-ref) "/mit/bin/scheme")
               "--batch-mode" "--eval" (find-scheme-libraries directories) "--load" (car files) "--eval" "(exit 0)" "--" extra)))))


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
           (string-append (united-prefix-ref) "/gauche/bin/gosh")
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
      ((gambit) (gambit-version))
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
      ((stklos) (stklos-exec args))
      ((racket) (racket-exec args))
      ((gauche) (gauche-exec args))
      ((guile) (guile-exec args))
      ((gambit) (gambit-exec args))
      ((mit) (mit-exec args))
      ((sagittarius) (sagittarius-exec args))
      ((gerbil) (gerbil-exec args))
      ((chibi) (chibi-exec args))
      ((chicken) (chicken-exec args))
      ((loko) (loko-exec args))
      ((cyclone) (cyclone-exec args))
      ((chez-cisco) (chez-cisco-exec args))
      ((chez-racket) (chez-racket-exec args)))))

(define united-check-all
  (lambda (args)
    (for-each (lambda (scheme)
                (display "* ")
                (display scheme)
                (united-check scheme args))
              (united-available))))

(define united-check
  (lambda (scheme args)
    (case (string->symbol scheme)
      ((stklos) (stklos-check args))
      ((chibi) (chibi-check args))
      ((mit) (mit-check args))
      ((loko) (loko-check args))
      ((gauche) (gauche-check args))
      ((gambit) (gambit-check args))
      ((racket) (racket-check args))
      ((gerbil) (gerbil-check args))
      ((guile) (guile-check args))
      ((chez-cisco) (chez-cisco-check args))
      ((chicken) (chicken-check args))
      ((sagittarius) (sagittarius-check args))
      ((cyclone) (cyclone-check args))
      ((chez-racket) (chez-racket-check args)))))

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
      (lambda (keywords directories files arguments extra)
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
          (set! arguments (append arguments (list "--program" (car files))))

          (chez-run "chez-cisco" (append arguments extra)))))))

(define chez-racket-exec
  (lambda (arguments)
    (call-with-values (lambda () (command-line-parse arguments))
      (lambda (keywords directories files arguments extra)
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
          (set! arguments (append arguments (list "--program" (car files))))

          (chez-run "chez-racket" (append arguments extra)))))))

(match (cdr (command-line))
 (("available") (united-available-display))
 (("install" . args) (united-install args))
 (("prefix" directory) (united-prefix-set directory))
 (("prefix") (united-prefix-display))
 (("all" "check" . args) (united-check-all args))
 ((scheme "check" . args) (united-check scheme args))
 ((scheme "compile" . args) (united-compile scheme args))
 ((scheme "exec" . args) (united-exec scheme args))
 ((scheme "repl" . args) (united-repl scheme args))
 ((scheme "version") (united-version scheme))
 (else (united-usage)))
