#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 120
(require rackunit
         racket/file
         racket/system
         racket/port
         racket/string
         "../extensions/gsd/evidence-discovery.rkt"
         "../extensions/gsd/evidence-repair-format.rkt"
         "../scripts/gsd-evidence-probe-runner.rkt")
(module+ test
  (test-case "static discovery matches runtime target"
    (define repo (make-temporary-file "discovery-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define (git . args)
         (define out (open-output-string))
         (check-true (parameterize ([current-directory repo]
                                    [current-output-port out])
                       (apply system* (find-executable-path "git") args)))
         (string-trim (get-output-string out)))
       (define (put p s)
         (display-to-file s (build-path repo p) #:exists 'truncate))
       (git "init" "-q")
       (git "config" "user.name" "Test")
       (git "config" "user.email" "test@invalid")
       (put "source.rkt" "#lang racket/base\n(provide answer)\n(define answer 0)\n")
       (git "add" "source.rkt")
       (git "commit" "-qm" "wrong")
       (make-directory (build-path repo "tests"))
       (make-directory (build-path repo "scripts"))
       (put "scripts/run-tests.rkt"
            (string-append
             "#lang racket/base\n(for ([test (in-vector (current-command-line-arguments))])\n"
             "  (dynamic-require `(submod ,(string->path test) test) #f))\n"))
       (define good
         (string-append
          "#lang racket/base\n(require rackunit \"../source.rkt\")\n(provide evidence-suite)\n"
          "(define evidence-suite (test-suite \"outer\" (test-suite \"inner\" "
          "(test-case \"target\" (check-equal? answer 1)))))\n"
          "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))
       (put "tests/probe.rkt" good)
       (put "source.rkt" "#lang racket/base\n(provide answer)\n(define answer 1)\n")
       (git "add" "source.rkt" "tests/probe.rkt" "scripts/run-tests.rkt")
       (git "commit" "-qm" "fix")
       (define (context)
         (hasheq 'plan-id
                 (make-string 64 #\a)
                 'wave
                 1
                 'attempt-id
                 "a"
                 'attempt-fence
                 0
                 'receipt-generation
                 0
                 'receipt-sha256
                 (make-string 64 #\b)
                 'evidence-revision
                 0
                 'repo
                 (path->string repo)
                 'branch
                 "main"
                 'origin
                 "local"
                 'head
                 (git "rev-parse" "HEAD")
                 'tree
                 (git "rev-parse" "HEAD^{tree}")
                 'snapshot-sha256
                 (make-string 64 #\c)
                 'green-proof-sha256
                 (make-string 64 #\d)))
       (define command "racket scripts/run-tests.rkt tests/probe.rkt")
       (define spec (discover-evidence-probe-spec repo (context) command))
       (check-true (and spec (valid-probe-spec? spec)))
       (when spec
         (define case-result
           (car (hash-ref (collect-evidence-suite (build-path repo "tests/probe.rkt")) 'cases)))
         (define target (hash-ref spec 'target))
         (check-equal? (hash-ref target 'suite-path) (hash-ref case-result 'suite-path))
         (check-equal? (hash-ref target 'test-case-name) (hash-ref case-result 'test-case-name))
         (check-equal? (hash-ref target 'check-location)
                       (hash-ref (car (hash-ref case-result 'checks)) 'location)))
       (define approved-runner (file->string (build-path repo "scripts/run-tests.rkt")))
       (put "scripts/run-tests.rkt" "#lang racket/base\n(exit 0)\n")
       (check-false (discover-evidence-probe-spec repo (context) command))
       (put "scripts/run-tests.rkt" approved-runner)
       (for ([cmd (in-list (list (string-append command " --verbose")
                                 "racket scripts/run-tests.rkt source.rkt"
                                 (string-append command " ; echo unsafe")))])
         (check-false (discover-evidence-probe-spec repo (context) cmd)))
       (for ([body (in-list (list (string-append good
                                                 "\n(define-syntax-rule (check-equal? a b) (void))\n")
                                  (string-replace good "(exit (run-tests evidence-suite))" "(exit 0)")
                                  (string-replace good "(provide evidence-suite)" "")
                                  (string-replace good
                                                  "(check-equal? answer 1)"
                                                  "(check-equal? answer 1) (check-true #t)")))])
         (put "tests/probe.rkt" body)
         (git "add" "tests/probe.rkt")
         (git "commit" "-qm" "negative")
         (check-false (discover-evidence-probe-spec repo (context) command)))
       ;; A fixed outer #lang does not make nested #reader datums safe.
       (define marker (build-path repo "reader-executed"))
       (define reader (build-path repo "evil-reader.rkt"))
       (put "evil-reader.rkt"
            (format (string-append "#lang racket/base\n(provide read read-syntax)\n"
                                   "(call-with-output-file ~s #:exists 'truncate "
                                   "(lambda (out) (display 'executed out)))\n")
                    (path->string marker)))
       (put "tests/probe.rkt"
            (string-replace good
                            "(define evidence-suite"
                            (format "#reader (file ~s)\n(define evidence-suite"
                                    (path->string reader))))
       (git "add" "evil-reader.rkt" "tests/probe.rkt")
       (git "commit" "-qm" "nested reader must never execute during discovery")
       (define reader-result (discover-evidence-probe-spec repo (context) command))
       (check-false (file-exists? marker) "discovery executed untrusted nested reader")
       (check-false reader-result)
       (delete-file (build-path repo "tests/probe.rkt"))
       (make-file-or-directory-link "../source.rkt" (build-path repo "tests/probe.rkt"))
       (git "add" "tests/probe.rkt")
       (git "commit" "-qm" "symlink")
       (check-false (discover-evidence-probe-spec repo (context) command)))
     (lambda () (delete-directory/files repo)))))
