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
         racket/runtime-path
         "../extensions/gsd/evidence-discovery.rkt"
         "../extensions/gsd/evidence-repair-format.rkt"
         "../extensions/gsd/evidence-runner-coverage.rkt"
         "../scripts/gsd-evidence-probe-runner.rkt"
         "../util/json/checksum.rkt")
(define-runtime-path owner "..")
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
     (lambda () (delete-directory/files repo))))

  (test-case "reasoned discovery names static refusals without widening the literal grammar"
    (define repo (make-temporary-file "discovery-reasoned-~a" 'directory))
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
       (define baseline (git "rev-parse" "HEAD"))
       (make-directory (build-path repo "tests"))
       (make-directory (build-path repo "scripts"))
       (make-directory (build-path repo "tests/helpers"))
       (put "scripts/run-tests.rkt"
            (string-append
             "#lang racket/base\n(for ([test (in-vector (current-command-line-arguments))])\n"
             "  (dynamic-require `(submod ,(string->path test) test) #f))\n"))
       (put "tests/helpers/plain.rkt" "#lang racket/base\n(provide plain)\n(define plain 1)\n")
       (define (probe-with extra-require check)
         (string-append
          "#lang racket/base\n(require rackunit \"../source.rkt\""
          extra-require
          ")\n"
          "(provide evidence-suite)\n"
          "(define evidence-suite (test-suite \"outer\" (test-case \"target\" "
          check
          ")))\n"
          "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))
       (put "tests/probe.rkt" (probe-with "" "(check-equal? answer 1)"))
       (put "source.rkt" "#lang racket/base\n(provide answer)\n(define answer 1)\n")
       (git "add" "-A")
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
       (define (outcome #:harness-files [harness '()])
         (discover-evidence-probe-outcome repo
                                          (context)
                                          command
                                          #:harness-files harness
                                          #:baseline-head baseline))
       ;; Admitted outcome agrees with the legacy spec/#f wrapper and the same
       ;; one static parser admits the same candidate directly.
       (define good (outcome))
       (check-equal? (hash-ref good 'status) "admitted")
       (check-equal? (hash-ref good 'spec)
                     (discover-evidence-probe-spec repo (context) command #:baseline-head baseline))
       (check-true (valid-probe-spec? (hash-ref good 'spec)))
       (define admission
         (static-probe-admission repo (hash-ref (context) 'head) command "tests/probe.rkt" '()))
       (check-true (hash-ref admission 'admitted?))
       (check-equal? (hash-ref (hash-ref admission 'target) 'test-path) "tests/probe.rkt")
       (define (refused? o code phase path detail-fragment)
         (check-equal? (hash-ref o 'status) "refused")
         (check-equal? (hash-ref o 'code) code)
         (check-equal? (hash-ref o 'phase) phase)
         (check-equal? (hash-ref o 'path) path)
         (check-true (string-contains? (or (hash-ref o 'detail #f) "") detail-fragment))
         (check-false (hash-ref o 'admitted? #t)))
       ;; A structured (only-in) import in the TEST module is unsupported
       ;; intentional grammar: refused, and named without any grammar widening.
       (put "tests/probe.rkt"
            (probe-with " (only-in \"../source.rkt\" answer)" "(check-equal? answer 1)"))
       (git "add" "tests/probe.rkt")
       (git "commit" "-qm" "only-in test require")
       (refused? (outcome) "unsupported-require" "test-module" "tests/probe.rkt" "only-in")
       (check-false (discover-evidence-probe-spec repo (context) command #:baseline-head baseline))
       ;; Reader diagnostics never expose the physical checkout or raw input.
       (put "tests/probe.rkt" "#lang racket/base\n(define PRIVATE-CANDIDATE-TEXT\n")
       (git "add" "tests/probe.rkt")
       (git "commit" "-qm" "malformed reader input")
       (define reader-refusal (outcome))
       (check-equal? (hash-ref reader-refusal 'code) "invalid-test-module")
       (check-false (string-contains? (hash-ref reader-refusal 'detail) (path->string repo)))
       (check-false (string-contains? (hash-ref reader-refusal 'detail) "PRIVATE-CANDIDATE-TEXT"))
       ;; An unknown module symbol require is named too.
       (put "tests/probe.rkt" (probe-with " file/glob" "(check-equal? answer 1)"))
       (git "add" "tests/probe.rkt")
       (git "commit" "-qm" "unknown module require")
       (refused? (outcome) "unsupported-require" "test-module" "tests/probe.rkt" "file/glob")
       ;; A structured (only-in) import in the HELPER module is named equally.
       (define witness
         (string-append "#lang racket/base\n(require (only-in \"plain.rkt\" plain))\n"
                        "(provide witness)\n(define (witness) plain)\n"))
       (define literal-witness
         (string-append "#lang racket/base\n(require \"plain.rkt\")\n"
                        "(provide witness)\n(define (witness) plain)\n"))
       (define (witness-manifest [extra '()])
         (append (list (hasheq 'path
                               "tests/helpers/witness.rkt"
                               'sha256
                               (sha256-file (build-path repo "tests/helpers/witness.rkt"))))
                 extra))
       (put "tests/helpers/witness.rkt" witness)
       (put "tests/probe.rkt" (probe-with " \"helpers/witness.rkt\"" "(check-true (witness))"))
       (git "add" "-A")
       (git "commit" "-qm" "only-in helper require")
       (refused? (outcome #:harness-files (witness-manifest))
                 "unsupported-require"
                 "helper-module"
                 "tests/helpers/witness.rkt"
                 "only-in")
       ;; Traversal through a helper require is named without executing anything.
       (put "tests/helpers/witness.rkt"
            (string-append "#lang racket/base\n(require \"../../../outside.rkt\")\n"
                           "(provide witness)\n(define (witness) 1)\n"))
       (git "add" "-A")
       (git "commit" "-qm" "helper traversal")
       (refused? (outcome #:harness-files (witness-manifest))
                 "unsafe-require-path"
                 "helper-module"
                 "tests/helpers/witness.rkt"
                 "require")
       ;; Declared transitive literal imports still admit; an approved but
       ;; unimported helper entry is named.
       (put "tests/helpers/witness.rkt" literal-witness)
       (git "add" "-A")
       (git "commit" "-qm" "literal transitive helper")
       (define plain-entry
         (hasheq 'path
                 "tests/helpers/plain.rkt"
                 'sha256
                 (sha256-file (build-path repo "tests/helpers/plain.rkt"))))
       (check-equal? (hash-ref (outcome #:harness-files (witness-manifest (list plain-entry)))
                               'status)
                     "admitted")
       (put "tests/helpers/witness.rkt"
            "#lang racket/base\n(provide witness)\n(define (witness) 42)\n")
       (git "add" "-A")
       (git "commit" "-qm" "independent witness helper")
       (refused? (outcome #:harness-files (witness-manifest (list plain-entry)))
                 "unused-approved-helper"
                 "helper-manifest"
                 "tests/helpers/plain.rkt"
                 "approved")
       ;; A nested #reader stays refused and discovery must never execute it.
       (define marker (build-path repo "reader-executed"))
       (define reader (build-path repo "evil-reader.rkt"))
       (put
        "evil-reader.rkt"
        (format
         (string-append
          "#lang racket/base\n(provide read read-syntax)\n"
          "(call-with-output-file ~s #:exists 'truncate (lambda (out) (display 'executed out)))\n")
         (path->string marker)))
       (put "tests/probe.rkt"
            (string-replace (probe-with " \"helpers/witness.rkt\"" "(check-true (witness))")
                            "(define evidence-suite"
                            (format "#reader (file ~s)\n(define evidence-suite"
                                    (path->string reader))))
       (git "add" "-A")
       (git "commit" "-qm" "nested reader must never execute during discovery")
       (refused? (outcome #:harness-files (witness-manifest))
                 "invalid-test-module"
                 "test-module"
                 "tests/probe.rkt"
                 "")
       (check-false (file-exists? marker) "discovery executed an untrusted nested reader")
       ;; A symlinked helper stays refused, now with a named phase.
       (put "tests/probe.rkt" (probe-with " \"helpers/witness.rkt\"" "(check-true (witness))"))
       (git "add" "-A")
       (git "commit" "-qm" "restore literal probe")
       (delete-file (build-path repo "tests/helpers/witness.rkt"))
       (make-file-or-directory-link "plain.rkt" (build-path repo "tests/helpers/witness.rkt"))
       (git "add" "-A")
       (git "commit" "-qm" "symlinked helper")
       (refused? (outcome #:harness-files (witness-manifest))
                 "harness-not-regular-blob"
                 "helper-manifest"
                 "tests/helpers/witness.rkt"
                 "")
       (check-false (discover-evidence-probe-spec repo
                                                  (context)
                                                  command
                                                  #:baseline-head baseline
                                                  #:harness-files (witness-manifest))))
     (lambda () (delete-directory/files repo))))

  (test-case "runner closure mismatch names the first differing installed path"
    (define repo (make-temporary-file "discovery-closure-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define owner-path owner)
       (check-true (parameterize ([current-output-port (open-output-nowhere)]
                                  [current-error-port (open-output-nowhere)])
                     (system* (find-executable-path "git")
                              "clone"
                              "--shared"
                              "--no-checkout"
                              (path->string owner-path)
                              (path->string repo))))
       (define (git . args)
         (define out (open-output-string))
         (check-true (parameterize ([current-directory repo]
                                    [current-output-port out])
                       (apply system* (find-executable-path "git") args)))
         (string-trim (get-output-string out)))
       (define head (git "rev-parse" "HEAD"))
       (define command "racket scripts/run-tests.rkt tests/probe.rkt")
       ;; The installed trusted runner closure matches this checkout's blobs.
       (check-false (regression-command-coverage-refusal repo head command "tests/probe.rkt"))
       ;; W0 actual topology: the installed scripts/run-tests/inventory.rkt
       ;; bytes differ from the execution head. The mismatch is named, the
       ;; refusal stays, and no import/closure grammar is loosened.
       (display-to-file "#lang racket/base\n(provide drifted)\n(define drifted 1)\n"
                        (build-path repo "replacement.rkt")
                        #:exists 'truncate)
       (define blob (git "hash-object" "-w" "replacement.rkt"))
       (git "read-tree" "HEAD")
       (git "update-index"
            "--cacheinfo"
            (string-append "100644," blob ",scripts/run-tests/inventory.rkt"))
       (define tree (git "write-tree"))
       (define altered
         (git "-c"
              "user.name=Fixture"
              "-c"
              "user.email=fixture@invalid"
              "commit-tree"
              tree
              "-p"
              head
              "-m"
              "drifted installed runner dependency"))
       (define refusal (regression-command-coverage-refusal repo altered command "tests/probe.rkt"))
       (check-false (hash-ref refusal 'admitted? #t))
       (check-equal? (hash-ref refusal 'code) "runner-closure-mismatch")
       (check-equal? (hash-ref refusal 'phase) "runner-closure")
       (check-equal? (hash-ref refusal 'path) "scripts/run-tests/inventory.rkt")
       (check-false (regression-command-covered? repo altered command "tests/probe.rkt"))
       ;; The same one static parser gives the same named reason to callers.
       (define admission (static-probe-admission repo altered command "tests/probe.rkt" '()))
       (check-equal? (hash-ref admission 'admitted?) #f)
       (check-equal? (hash-ref admission 'code) "runner-closure-mismatch")
       (check-equal? (hash-ref admission 'path) "scripts/run-tests/inventory.rkt"))
     (lambda () (delete-directory/files repo)))))
